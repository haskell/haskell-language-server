{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.ExplicitFields
  ( descriptor
  ) where

import           Control.Arrow                   ((&&&))
import           Control.Lens                    ((%~), (^.))
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except      (ExceptT)
import           Data.Data                       (Data)
import           Data.Generics                   (GenericQ, everything, extQ,
                                                  mkQ)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Maybe                      (catMaybes, isJust, mapMaybe,
                                                  maybeToList)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 (IdeState, NormalizedFilePath,
                                                  Pretty (..), Priority (..),
                                                  Range (..), Recorder (..),
                                                  Rules, WithPriority (..),
                                                  getFileContents, hscEnv,
                                                  logWith, realSrcSpanToRange,
                                                  srcSpanToRange)
import           Development.IDE.Core.Rules      (runAction)
import           Development.IDE.Core.RuleTypes  (GhcSession (..),
                                                  GhcSessionDeps (..),
                                                  TcModuleResult (..),
                                                  TypeCheck (..))
import           Development.IDE.Core.Shake      (define, use, useWithStale)
import qualified Development.IDE.Core.Shake      as Shake
import           Development.IDE.GHC.Compat      (HasSrcSpan (..),
                                                  HsConDetails (RecCon),
                                                  HsRecFields (..), HscEnv (..),
                                                  LPat, Outputable, RealSrcSpan,
                                                  SDoc, SrcSpan, getLocA, locA,
                                                  pm_mod_summary, unLoc, unLocA)
import           Development.IDE.GHC.Compat.Core (GenLocated (..), GhcPass (..),
                                                  HsBindLR (..),
                                                  HsExpr (RecordCon, rcon_flds),
                                                  HsRecField' (..),
                                                  HsValBindsLR (..), LHsExpr,
                                                  ModSummary (..),
                                                  NHsValBindsLR (..), Pass (..),
                                                  Pat (..), extensionFlags,
                                                  hs_valds)
import           Development.IDE.GHC.Compat.Util (bagToList, toList)
import           Development.IDE.GHC.Util        (printOutputable)
import           Development.IDE.Graph           (RuleResult)
import           Development.IDE.Graph.Classes   (Hashable, NFData (rnf))
import           Development.IDE.Spans.Pragmas   (NextPragmaInfo (..),
                                                  getNextPragmaInfo,
                                                  insertNewPragma)
import           Development.IDE.Types.Logger    (cmapWithPrio)
import           GHC.Data.Maybe                  (fromMaybe)
import           GHC.Generics                    (Generic)
import           GHC.Hs.Dump                     (BlankSrcSpan (..),
                                                  showAstData)
import           GHC.LanguageExtensions.Type     (Extension (..))
import           Ide.PluginUtils                 (getNormalizedFilePath,
                                                  handleMaybeM, pluginResponse,
                                                  subRange)
import           Ide.Types                       (PluginDescriptor (..),
                                                  PluginId (..),
                                                  PluginMethodHandler,
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
preprocessRecord :: HsRecFields p arg -> HsRecFields p arg
preprocessRecord flds = flds { rec_dotdot = Nothing , rec_flds = rec_flds' }
  where
    -- TODO(ozkutuk): HsRecField' is renamed to HsFieldBind in GHC 9.4
    -- Add it as a pattern synonym to the ghcide compat module, so it would
    -- work on all HLS builds.
    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5757

    no_pun_count = maybe (length (rec_flds flds)) unLoc (rec_dotdot flds)
    -- Field binds of the explicit form (e.g. `{ a = a' }`) should be
    -- left as is, hence the split.
    (no_puns, puns) = splitAt no_pun_count (rec_flds flds)
    -- `hsRecPun` is set to `True` in order to pretty-print the fields as field
    -- puns (since there is similar mechanism in the `Outputable` instance as
    -- explained above).
    puns' = map (\(L ss fld) -> L ss (fld { hsRecPun = True })) puns
    rec_flds' = no_puns <> puns'

showRecordPat :: Outputable (Pat p) => Pat p -> Maybe Text
showRecordPat pat@(ConPat _ _ (RecCon flds)) =
  Just $ printOutputable $
    pat { pat_args = RecCon (preprocessRecord flds) }
showRecordPat _ = Nothing

showRecordCon :: Outputable (HsExpr p) => HsExpr p -> Maybe Text
showRecordCon expr@(RecordCon _ _ flds) =
  Just $ printOutputable $
    expr { rcon_flds = preprocessRecord flds }
showRecordCon _ = Nothing

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
  , pluginRules = collectRecordsRule recorder
  }

data CollectRecords = CollectRecords
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecords
instance NFData CollectRecords

data CollectRecordsResult = CRR
  { recordInfos       :: ![RenderedRecordInfo]
  , enabledExtensions :: ![GhcExtension]
  }
  deriving (Generic)

instance NFData CollectRecordsResult

instance Show CollectRecordsResult where
  show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecords = CollectRecordsResult

newtype GhcExtension = GhcExtension { unExt :: Extension }

instance NFData GhcExtension where
  rnf x = x `seq` ()

data RecordInfo
  = RecordInfoPat !SrcSpan !(Pat (GhcPass 'Renamed))
  | RecordInfoCon !SrcSpan !(HsExpr (GhcPass 'Renamed))

data RenderedRecordInfo = RenderedRecordInfo
  { renderedSrcSpan :: !SrcSpan
  , renderedRecord  :: !Text
  }
  deriving (Generic)

instance NFData RenderedRecordInfo

renderRecordInfo :: RecordInfo -> Maybe RenderedRecordInfo
renderRecordInfo (RecordInfoPat ss pat) = RenderedRecordInfo ss <$> showRecordPat pat
renderRecordInfo (RecordInfoCon ss expr) = RenderedRecordInfo ss <$> showRecordCon expr

-- collectRecords :: GenericQ [LPat (GhcPass 'Renamed)]
collectRecords :: GenericQ [RecordInfo]
collectRecords = everything (<>) (maybeToList . (Nothing `mkQ` getRecPatterns `extQ` getRecCons))

getRecCons :: LHsExpr (GhcPass 'Renamed) -> Maybe RecordInfo
getRecCons e@(unLoc -> RecordCon _ _ flds)
  | isJust (rec_dotdot flds) = Just $ mkRecInfo e
  where
    mkRecInfo :: LHsExpr (GhcPass 'Renamed) -> RecordInfo
    mkRecInfo expr = RecordInfoCon (getLoc expr) (unLoc expr)
getRecCons _ = Nothing

getRecPatterns :: LPat (GhcPass 'Renamed) -> Maybe RecordInfo
getRecPatterns conPat@(unLoc -> ConPat _ _ (RecCon flds))
  | isJust (rec_dotdot flds) = Just $ mkRecInfo conPat
  where
    mkRecInfo :: LPat (GhcPass 'Renamed) -> RecordInfo
    mkRecInfo pat = RecordInfoPat (getLoc pat) (unLoc pat)
getRecPatterns _ = Nothing

collectRecordsInRange :: MonadIO m => Range -> IdeState -> NormalizedFilePath -> ExceptT String m CollectRecordsResult
collectRecordsInRange range ideState nfp = do
  CRR renderedRecs exts <- collectRecords' ideState nfp
  pure $ CRR (filter inRange renderedRecs) exts

  where
    inRange :: RenderedRecordInfo -> Bool
    inRange (RenderedRecordInfo ss _) = maybe False (subRange range) (srcSpanToRange ss)

getEnabledExtensions :: TcModuleResult -> [GhcExtension]
getEnabledExtensions = map GhcExtension . toList . extensionFlags . ms_hspp_opts . pm_mod_summary . tmrParsed

getRecords :: TcModuleResult -> [RecordInfo]
getRecords (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) =
  collectRecords valBinds

collectRecords' :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m CollectRecordsResult
collectRecords' ideState =
  handleMaybeM "Unable to TypeCheck"
    . liftIO
    . runAction "ExplicitFields" ideState
    . use CollectRecords

collectRecordsRule :: Recorder (WithPriority Log) -> Rules ()
collectRecordsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectRecords nfp -> do
  tmr <- use TypeCheck nfp
  let exts = getEnabledExtensions <$> tmr
      recs = getRecords <$> tmr
      renderedRecs = mapMaybe renderRecordInfo <$> recs
  pure ([], CRR <$> renderedRecs <*> exts)

codeActionProvider :: Recorder (WithPriority Log)
                   -> PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionProvider recorder ideState pId (CodeActionParams _ _ docId range _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (docId ^. L.uri)
  pragma <- getFirstPragma pId ideState nfp
  CRR renderedRecs (map unExt -> exts) <- collectRecordsInRange range ideState nfp
  logWith recorder Info $ LogTxts $ map (T.pack . show . srcSpanToRange . renderedSrcSpan) renderedRecs
  let actions = map (mkCodeAction nfp exts pragma) renderedRecs
  -- liftIO $ runAction "ExplicitFields" ideState $ use CollectRecords nfp
  pure $ List actions

  where
    mkCodeAction :: NormalizedFilePath -> [Extension] -> NextPragmaInfo -> RenderedRecordInfo -> Command |? CodeAction
    mkCodeAction nfp exts pragma rec = InR CodeAction
      { _title = mkCodeActionTitle exts
      , _kind = Just CodeActionRefactorRewrite
      , _diagnostics = Nothing
      , _isPreferred = Nothing
      , _disabled = Nothing
      , _edit = Just $ mkWorkspaceEdit nfp edits
      , _command = Nothing
      , _xdata = Nothing
      }
      where
        edits = catMaybes [ mkTextEdit rec , pragmaEdit ]

        mkTextEdit :: RenderedRecordInfo -> Maybe TextEdit
        mkTextEdit (RenderedRecordInfo ss r) = TextEdit <$> srcSpanToRange ss <*> pure r

        -- TODO(ozkutuk): `RecordPuns` extension is renamed to `NamedFieldPuns`
        -- in GHC 9.4, so I probably need to add this to the compat module as
        -- well.
        -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6156
        pragmaEdit :: Maybe TextEdit
        pragmaEdit = if RecordPuns `elem` exts
                       then Nothing
                       else Just $ patchExtName $ insertNewPragma pragma RecordPuns
          where
            patchExtName = L.newText %~ T.replace "Record" "NamedField"

    mkWorkspaceEdit :: NormalizedFilePath -> [TextEdit] -> WorkspaceEdit
    mkWorkspaceEdit nfp edits = WorkspaceEdit changes Nothing Nothing
      where
        changes = Just $ HashMap.singleton (fromNormalizedUri (normalizedFilePathToUri nfp)) (List edits)

mkCodeActionTitle :: [Extension] -> Text
mkCodeActionTitle exts =
  if RecordPuns `elem` exts
    then title
    else title <> " (needs extension: NamedFieldPuns)"
    where
      title = "Expand record wildcard"

-- Copied from hls-alternate-number-format-plugin
getFirstPragma :: MonadIO m => PluginId -> IdeState -> NormalizedFilePath -> ExceptT String m NextPragmaInfo
getFirstPragma (PluginId pId) state nfp = handleMaybeM "Could not get NextPragmaInfo" $ do
  ghcSession <- liftIO $ runAction (T.unpack pId <> ".GhcSession") state $ useWithStale GhcSession nfp
  (_, fileContents) <- liftIO $ runAction (T.unpack pId <> ".GetFileContents") state $ getFileContents nfp
  case ghcSession of
    Just (hscEnv -> hsc_dflags -> sessionDynFlags, _) -> pure $ Just $ getNextPragmaInfo sessionDynFlags fileContents
    Nothing                                           -> pure Nothing
