{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.ExplicitFields
  ( descriptor
  ) where

import           Control.Arrow                   ((&&&))
import           Control.Lens                    ((^.))
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except      (ExceptT)
import           Data.Data                       (Data)
import           Data.Generics                   (GenericQ, everything, mkQ)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Maybe                      (catMaybes, isJust,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 (IdeState, NormalizedFilePath,
                                                  Pretty (..), Priority (..),
                                                  Range (..), Recorder (..),
                                                  Rules, WithPriority (..),
                                                  logWith, realSrcSpanToRange,
                                                  srcSpanToRange)
import           Development.IDE.Core.Rules      (runAction)
import           Development.IDE.Core.RuleTypes  (GhcSessionDeps (..),
                                                  TcModuleResult (..),
                                                  TypeCheck (..))
import           Development.IDE.Core.Shake      (define, use)
import qualified Development.IDE.Core.Shake      as Shake
import           Development.IDE.GHC.Compat      (HasSrcSpan (..),
                                                  HsConDetails (RecCon),
                                                  HsRecFields (..), LPat,
                                                  Outputable, RealSrcSpan, SDoc,
                                                  SrcSpan, getLocA, locA, unLoc,
                                                  unLocA)
import           Development.IDE.GHC.Compat.Core (GenLocated (..), GhcPass (..),
                                                  HsBindLR (..),
                                                  HsValBindsLR (..),
                                                  NHsValBindsLR (..), Pass (..),
                                                  Pat (..), hs_valds)
import           Development.IDE.GHC.Compat.Util (bagToList)
import           Development.IDE.GHC.Util        (printOutputable)
import           Development.IDE.Graph           (RuleResult)
import           Development.IDE.Graph.Classes   (Hashable, NFData)
import           Development.IDE.Types.Logger    (cmapWithPrio)
import           GHC.Data.Maybe                  (fromMaybe)
import           GHC.Generics                    (Generic)
import           GHC.Hs.Dump                     (BlankSrcSpan (..),
                                                  showAstData)
import           Ide.PluginUtils                 (getNormalizedFilePath,
                                                  handleMaybeM, pluginResponse,
                                                  subRange)
import           Ide.Types                       (PluginDescriptor (..),
                                                  PluginId, PluginMethodHandler,
                                                  defaultPluginDescriptor,
                                                  mkPluginHandler)
import           Language.LSP.Types              (CodeAction (..),
                                                  CodeActionKind (CodeActionRefactorRewrite),
                                                  CodeActionParams (..),
                                                  Command, List (..),
                                                  Method (..), SMethod (..),
                                                  TextEdit (..),
                                                  WorkspaceEdit (WorkspaceEdit),
                                                  fromNormalizedUri,
                                                  isSubrangeOf,
                                                  normalizedFilePathToUri,
                                                  type (|?) (InR))
import qualified Language.LSP.Types.Lens         as L

showAstDataFull :: Data a => a -> SDoc
showAstDataFull = showAstData NoBlankSrcSpan
--
-- showAstDataFull' :: Data a => a -> SDoc
-- showAstDataFull' = showAstData BlankSrcSpan

-- `Outputable` instance of `HsRecFields` does smart things to print
-- the records that originally had wildcards with dots, even after they
-- are removed by the renamer pass. Here `rec_dotdot` is set to
-- `Nothing` so that fields are printed without such post-processing.
showRecord :: Outputable arg => HsRecFields p arg -> Text
showRecord rec = printOutputable (rec { rec_dotdot = Nothing })

-- Same as `showRecord` but works with `Pat` types. This is a partial function
-- but it is only ever called with record patterns, so it should be okay.
showRecordPat :: Outputable (Pat p) => Pat p -> Maybe Text
showRecordPat pat@(ConPat _ _ (RecCon flds)) =
  Just $ printOutputable $ pat { pat_args = RecCon (flds { rec_dotdot = Nothing }) }
showRecordPat _ = Nothing

data Log
  = LogShake Shake.Log
  | LogTxts [Text]
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog
    LogTxts txts      -> pretty (length txts) <> " - " <> pretty txts

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction (codeActionProvider recorder)
  -- , pluginRules = collectRecordsRule recorder
  }

-- data CollectRecords = CollectRecords
--                     deriving (Eq, Show, Generic)
--
-- instance Hashable CollectRecords
-- instance NFData CollectRecords
--
-- type instance RuleResult CollectRecords = CollectRecordsResult
--
-- -- TODO(ozkutuk)
-- data CollectRecordsResult = CRR -- [RecordInfo]
--                           deriving (Show, Generic)
-- instance NFData CollectRecordsResult

-- newtype RecordPattern = RecordPattern [Pat (GhcPass 'Renamed)]
--                       deriving (Generic)

-- instance Show RecordPattern where
--   show (RecordPattern l) = T.unpack (printOutputable l)

data RecordInfo = RecordInfo !SrcSpan !(Pat (GhcPass 'Renamed))
                -- deriving (Show, Generic)

getSrcSpan :: RecordInfo -> SrcSpan
getSrcSpan (RecordInfo ss _) = ss

getEdit :: RecordInfo -> Maybe Text
getEdit (RecordInfo _ pat) = showRecordPat pat

collectRecords :: GenericQ [LPat (GhcPass 'Renamed)]
collectRecords = everything (<>) (maybeToList . (Nothing `mkQ` getRecPatterns))

getRecPatterns :: LPat (GhcPass 'Renamed) -> Maybe (LPat (GhcPass 'Renamed))
getRecPatterns conPat@(unLoc -> ConPat _ _ (RecCon flds))
  | isJust (rec_dotdot flds) = Just conPat
getRecPatterns _ = Nothing

collectRecordsInRange :: MonadIO m => IdeState -> Range -> NormalizedFilePath -> ExceptT String m [RecordInfo]
collectRecordsInRange ideState range nfp = do
  recs <- collectRecords' ideState range nfp
  let recsInRange = filter inRange recs
  pure recsInRange

  where
    inRange :: RecordInfo -> Bool
    inRange (RecordInfo (srcSpanToRange -> recRange) _) =
      maybe False (subRange range) recRange

collectRecords' :: MonadIO m => IdeState -> Range -> NormalizedFilePath -> ExceptT String m [RecordInfo]
collectRecords' ideState range nfp = do
  tmr <- handleMaybeM "Unable to TypeCheck"
    $ liftIO
    $ runAction "ExplicitFields" ideState
    $ use TypeCheck nfp
  let (hsGroup,_,_,_) = tmrRenamed tmr
      valBinds = hs_valds hsGroup
      recs = collectRecords valBinds
      (srcs, recs') = unzip $ map (getLoc &&& unLoc) recs
      -- recs' = map unLoc recs
      -- srcs = map (locA . (\(L l _) -> l) . pat_con) recs
      -- recsPrinted = map (showRecord . (\(RecCon x) -> x) . pat_args) recs
      recInfos = zipWith RecordInfo srcs recs'
  pure recInfos
      -- logTxts xs = logWith recorder Info (LogTxts xs)
  -- logTxts recsPrinted
  --   *> logTxts (map printOutputable srcs)
  --   *> logTxts (map (printOutputable . showAstDataFull) recs)
  --   *> pure ([], Nothing)

-- collectRecordsRule :: Recorder (WithPriority Log) -> Rules ()
-- collectRecordsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectRecords nfp -> do
--   tmr <- use TypeCheck nfp
--   -- hsc <- use GhcSessionDeps nfp
--   case tmr of
--     Nothing -> undefined
--     Just tmr' ->
--       let (hsGroup,_,_,_) = tmrRenamed tmr'
--           valBinds = hs_valds hsGroup
--           recs = collectRecords valBinds
--           srcs = map (locA . (\(L l _) -> l) . pat_con) recs
--           recsPrinted = map (showRecord . (\(RecCon x) -> x) . pat_args) recs
--           logTxts xs = logWith recorder Info (LogTxts xs)
--       in logTxts recsPrinted
--            *> logTxts (map printOutputable srcs)
--            *> logTxts (map (printOutputable . showAstDataFull) recs)
--            *> pure ([], Nothing)
-- -- *> logWith recorder Info (LogTxts $ map printOutputable bindsList') *> pure ([], Nothing)

codeActionProvider :: Recorder (WithPriority Log)
                   -> PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionProvider recorder ideState _ (CodeActionParams _ _ docId range _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (docId ^. L.uri)
  recs <- collectRecordsInRange ideState range nfp
  logWith recorder Info $ LogTxts $ map (T.pack . show . srcSpanToRange . getSrcSpan) recs
  let actions = map (mkCodeAction nfp) recs
  -- liftIO $ runAction "ExplicitFields" ideState $ use CollectRecords nfp
  pure $ List actions

  where
    mkCodeAction :: NormalizedFilePath -> RecordInfo -> Command |? CodeAction
    mkCodeAction nfp rec = InR CodeAction
      { _title = "Expand the record wildcard"
      , _kind = Just CodeActionRefactorRewrite
      , _diagnostics = Nothing
      , _isPreferred = Nothing
      , _disabled = Nothing
      , _edit = Just $ mkWorkspaceEdit nfp edits
      , _command = Nothing
      , _xdata = Nothing
      }
      where
        edits = catMaybes [TextEdit <$> (srcSpanToRange $ getSrcSpan rec) <*> getEdit rec]

    mkWorkspaceEdit :: NormalizedFilePath -> [TextEdit] -> WorkspaceEdit
    mkWorkspaceEdit nfp edits = WorkspaceEdit changes Nothing Nothing
      where
        changes = Just $ HashMap.singleton (fromNormalizedUri (normalizedFilePathToUri nfp)) (List edits)
