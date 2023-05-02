{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.OverloadedRecordDot
  ( descriptor
  , Log
  ) where

-- based off of Berk Okzuturk's hls-explicit-records-fields-plugin

import           Control.Lens                   ((^.))
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Trans.Except     (ExceptT)
import           Data.Generics                  (GenericQ, everything, mkQ)
import qualified Data.HashMap.Strict            as HashMap
import           Data.Maybe                     (listToMaybe, maybeToList)
import           Data.Text                      (Text)
import           Development.IDE                (IdeState, NormalizedFilePath,
                                                 Pretty (..), Range,
                                                 Recorder (..), Rules,
                                                 WithPriority (..),
                                                 realSrcSpanToRange)
import           Development.IDE.Core.Rules     (runAction)
import           Development.IDE.Core.RuleTypes (TcModuleResult (..),
                                                 TypeCheck (..))
import           Development.IDE.Core.Shake     (define, use)
import qualified Development.IDE.Core.Shake     as Shake

#if __GLASGOW_HASKELL__ >= 903
import           Development.IDE.GHC.Compat     (HsExpr (HsRecSel))
#else
import           Development.IDE.GHC.Compat     (HsExpr (HsRecFld))
#endif

import           Development.IDE.GHC.Compat     (Extension (OverloadedRecordDot),
                                                 GhcPass,
                                                 HsExpr (HsApp, HsPar, HsVar, OpApp),
                                                 LHsExpr, Outputable, Pass (..),
                                                 RealSrcSpan, getLoc, hs_valds,
                                                 pattern RealSrcSpan, unLoc)
import           Development.IDE.GHC.Util       (getExtensions, printOutputable)
import           Development.IDE.Graph          (RuleResult)
import           Development.IDE.Graph.Classes  (Hashable, NFData (rnf))
import           Development.IDE.Spans.Pragmas  (NextPragmaInfo (..),
                                                 getFirstPragma,
                                                 insertNewPragma)
import           Development.IDE.Types.Logger   (Priority (..), cmapWithPrio,
                                                 logWith, (<+>))
import           GHC.Generics                   (Generic)
import           Ide.Plugin.RangeMap            (RangeMap)
import qualified Ide.Plugin.RangeMap            as RangeMap
import           Ide.PluginUtils                (getNormalizedFilePath,
                                                 handleMaybeM, pluginResponse)
import           Ide.Types                      (PluginDescriptor (..),
                                                 PluginId (..),
                                                 PluginMethodHandler,
                                                 defaultPluginDescriptor,
                                                 mkPluginHandler)
import           Language.LSP.Types             (CodeAction (..),
                                                 CodeActionKind (CodeActionRefactorRewrite),
                                                 CodeActionParams (..), Command,
                                                 List (..), Method (..),
                                                 SMethod (..), TextEdit (..),
                                                 WorkspaceEdit (WorkspaceEdit),
                                                 fromNormalizedUri,
                                                 normalizedFilePathToUri,
                                                 type (|?) (InR))
import qualified Language.LSP.Types.Lens        as L

data Log
  = LogShake Shake.Log
  | LogCollectedRecordSelectors [RecordSelectors]
  | LogRenderedRecordSelectors [ConvertedRecordSelector]

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog
    LogCollectedRecordSelectors recs -> "Collected record selectors:" <+> pretty recs
    LogRenderedRecordSelectors recs -> "Rendered record selectors:" <+> pretty recs

data CollectRecordSelectors = CollectRecordSelectors
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecordSelectors
instance NFData CollectRecordSelectors

data CollectConvertedRecordSelectorsResult = CCRSR
  { recordInfos       :: RangeMap ConvertedRecordSelector
  , enabledExtensions :: [GhcExtension]
  }
  deriving (Generic)

instance NFData CollectConvertedRecordSelectorsResult

instance Show CollectConvertedRecordSelectorsResult where
  show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecordSelectors = CollectConvertedRecordSelectorsResult

-- `Extension` is wrapped so that we can provide an `NFData` instance
-- (without resorting to creating an orphan instance).
newtype GhcExtension = GhcExtension { unExt :: Extension }

instance NFData GhcExtension where
  rnf x = x `seq` ()

data RecordSelectors
  = RecordSelectors RealSrcSpan (HsExpr (GhcPass 'Renamed))

instance Pretty RecordSelectors where
  pretty (RecordSelectors ss e) = pretty (printOutputable ss) <> ":" <+> pretty (printOutputable e)

data ConvertedRecordSelector = ConvertedRecordSelector
  { range              :: Range
  , convertedDotRecord :: Text
  }
  deriving (Generic)

instance Pretty ConvertedRecordSelector where
  pretty (ConvertedRecordSelector r cdr) = pretty (show r) <> ":" <+> pretty cdr

instance NFData ConvertedRecordSelector

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
  , pluginRules = collectConvRecSelsRule recorder
  }

codeActionProvider :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionProvider ideState pId (CodeActionParams _ _ caDocId caRange _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (caDocId ^. L.uri)
  pragma <- getFirstPragma pId ideState nfp
  CCRSR crsMap (coerce -> exts) <- collectConvRecSels' ideState nfp
  let actions = map (mkCodeAction nfp exts pragma) (RangeMap.filterByRange caRange crsMap)
  pure $ List actions
  where
    mkCodeAction :: NormalizedFilePath -> [Extension] -> NextPragmaInfo -> ConvertedRecordSelector -> Command |? CodeAction
    mkCodeAction nfp exts pragma crs = InR CodeAction
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
        edits = mkTextEdit crs : maybeToList pragmaEdit

        mkTextEdit :: ConvertedRecordSelector -> TextEdit
        mkTextEdit (ConvertedRecordSelector r cdr) = TextEdit r cdr

        pragmaEdit :: Maybe TextEdit
        pragmaEdit = if OverloadedRecordDot `elem` exts
                       then Nothing
                       else Just $ insertNewPragma pragma OverloadedRecordDot

    mkWorkspaceEdit :: NormalizedFilePath -> [TextEdit] -> WorkspaceEdit
    mkWorkspaceEdit nfp edits = WorkspaceEdit changes Nothing Nothing
      where
        changes = Just $ HashMap.singleton (fromNormalizedUri (normalizedFilePathToUri nfp)) (List edits)

    mkCodeActionTitle :: [Extension] -> Text
    mkCodeActionTitle exts =
      if OverloadedRecordDot `elem` exts
        then title
        else title <> " (needs extension: OverloadedRecordDot)"
        where
          title = "Convert to record dot syntax"

collectConvRecSelsRule :: Recorder (WithPriority Log) -> Rules ()
collectConvRecSelsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectRecordSelectors nfp ->
  use TypeCheck nfp >>= \case
    Nothing -> pure ([], Nothing)
    Just tmr -> do
      let exts = getEnabledExtensions tmr
          recSels = getRecordSelectors tmr
      logWith recorder Debug (LogCollectedRecordSelectors recSels)
      let convertedRecordSelectors = traverse convertRecordSelectors recSels
          crsMap = RangeMap.fromList range <$> convertedRecordSelectors
      logWith recorder Debug (LogRenderedRecordSelectors (concat convertedRecordSelectors))
      pure ([], CCRSR <$> crsMap <*> Just exts)
  where
    getEnabledExtensions :: TcModuleResult -> [GhcExtension]
    getEnabledExtensions = map GhcExtension . getExtensions . tmrParsed

getRecordSelectors :: TcModuleResult -> [RecordSelectors]
getRecordSelectors (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) =
    collectRecordSelectors valBinds

convertRecordSelectors :: RecordSelectors -> Maybe ConvertedRecordSelector
convertRecordSelectors (RecordSelectors ss expr) = ConvertedRecordSelector (realSrcSpanToRange ss) <$> convertRecSel expr

convertRecSel :: Outputable (HsExpr (GhcPass c)) => HsExpr (GhcPass c) -> Maybe Text
convertRecSel (HsApp _ s r) =
    Just $ printOutputable r <> "." <> printOutputable s
convertRecSel ( OpApp _ s _ r) =
    Just $ "(" <> printOutputable r <> ")." <> printOutputable s
convertRecSel _ = Nothing

collectRecordSelectors :: GenericQ [RecordSelectors]
collectRecordSelectors = everything (<>) (maybeToList . (Nothing `mkQ` getRecSels))

#if __GLASGOW_HASKELL__ >= 903
getRecSels :: LHsExpr (GhcPass 'Renamed) -> Maybe RecordSelectors
-- standard record selection: "field record"
getRecSels e@(unLoc -> HsApp _ (unLoc -> HsRecSel _ _) (unLoc -> HsVar _ _)) =
  listToMaybe [ RecordSelectors realSpan' (unLoc e) | RealSrcSpan realSpan' _ <- [ getLoc e ]]
-- Record selection where the field is being applied to a parenthesised expression: "field (record)"
getRecSels e@(unLoc -> HsApp _ (unLoc -> HsRecSel _ _) (unLoc -> HsPar _ _ _ _)) =
  listToMaybe [ RecordSelectors realSpan' (unLoc e) | RealSrcSpan realSpan' _ <- [ getLoc e ]]
-- Record selection where the field is being applied with the "$" operator: "field $ record"
getRecSels e@(unLoc -> OpApp _ (unLoc -> HsRecSel _ _) (unLoc -> HsVar _ (unLoc -> d)) _)
    | printOutputable d == "$" = listToMaybe [ RecordSelectors realSpan' (unLoc e) | RealSrcSpan realSpan' _ <- [ getLoc e ]]
getRecSels _ = Nothing
#else
getRecSels :: LHsExpr (GhcPass 'Renamed) -> Maybe RecordSelectors
getRecSels e@(unLoc -> HsApp _ (unLoc -> HsRecFld _ _) (unLoc -> HsVar _ _)) =
  listToMaybe [ RecordSelectors realSpan' (unLoc e) | RealSrcSpan realSpan' _ <- [ getLoc e ]]
getRecSels e@(unLoc -> HsApp _ (unLoc -> HsRecFld _ _) (unLoc -> HsPar _ _)) =
  listToMaybe [ RecordSelectors realSpan' (unLoc e) | RealSrcSpan realSpan' _ <- [ getLoc e ]]
getRecSels e@(unLoc -> OpApp _ (unLoc -> HsRecFld _ _) (unLoc -> HsVar _ (unLoc -> d)) _)
    | printOutputable d == "$" = listToMaybe [ RecordSelectors realSpan' (unLoc e) | RealSrcSpan realSpan' _ <- [ getLoc e ]]
getRecSels _ = Nothing
#endif

collectConvRecSels' :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m CollectConvertedRecordSelectorsResult
collectConvRecSels' ideState =
  handleMaybeM "Unable to TypeCheck"
    . liftIO
    . runAction "overloadedRecordDot.collectRecordSelectors" ideState
    . use CollectRecordSelectors

