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
import           Data.Generics                  (GenericQ, everything,
                                                 everythingBut, mkQ)
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

import           Control.DeepSeq                (rwhnf)
import           Data.Bifunctor                 (Bifunctor (first))
import           Development.IDE.GHC.Compat     (Extension (OverloadedRecordDot),
                                                 GhcPass,
                                                 HsExpansion (HsExpanded),
                                                 HsExpr (HsApp, HsPar, HsVar, OpApp, XExpr),
                                                 LHsExpr, Outputable, Pass (..),
                                                 RealSrcSpan, appPrec,
                                                 dollarName, getLoc, hs_valds,
                                                 parenthesizeHsContext,
                                                 parenthesizeHsExpr,
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
  | LogCollectedRecordSelectors [RecordSelectorExpr]
  | LogTextEdits [TextEdit]

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog
    LogCollectedRecordSelectors recs -> "Collected record selectors:" <+> pretty recs

data CollectRecordSelectors = CollectRecordSelectors
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecordSelectors
instance NFData CollectRecordSelectors

data CollectRecordSelectorsResult = CRSR
  { recordInfos       :: RangeMap RecordSelectorExpr
  , enabledExtensions :: [Extension]
  }
  deriving (Generic)

instance NFData CollectRecordSelectorsResult

instance Show CollectRecordSelectorsResult where
  show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecordSelectors = CollectRecordSelectorsResult


data RecordSelectorExpr = RecordSelectorExpr { location     :: Range,
                                               selectorExpr :: LHsExpr (GhcPass 'Renamed),
                                               recordExpr   :: LHsExpr (GhcPass 'Renamed) }

instance Pretty RecordSelectorExpr where
  pretty (RecordSelectorExpr l rs se) = pretty (printOutputable rs) <> ":" <+> pretty (printOutputable se)

instance NFData RecordSelectorExpr where
  rnf = rwhnf

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
  , pluginRules = collectRecSelsRule recorder
  }

codeActionProvider :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionProvider ideState pId (CodeActionParams _ _ caDocId caRange _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (caDocId ^. L.uri)
  pragma <- getFirstPragma pId ideState nfp
  CRSR crsMap exts <- collectRecSelResult ideState nfp
  let
    pragmaEdit = if OverloadedRecordDot `elem` exts
                            then Nothing
                            else Just $ insertNewPragma pragma OverloadedRecordDot
    edits crs = maybeToList (convertRecordSelectors crs) <> maybeToList pragmaEdit
    changes crs = Just $ HashMap.singleton (fromNormalizedUri (normalizedFilePathToUri nfp)) (List (edits crs))
    mkCodeAction crs = InR CodeAction
        { _title = mkCodeActionTitle exts crs
        , _kind = Just CodeActionRefactorRewrite
        , _diagnostics = Nothing
        , _isPreferred = Nothing
        , _disabled = Nothing
        , _edit = Just $ WorkspaceEdit (changes crs) Nothing Nothing
        , _command = Nothing
        , _xdata = Nothing
        }
    actions = map mkCodeAction (RangeMap.filterByRange caRange crsMap)
  pure $ List actions
  where
    mkCodeActionTitle :: [Extension] -> RecordSelectorExpr-> Text
    mkCodeActionTitle exts (RecordSelectorExpr _ se _) =
      if OverloadedRecordDot `elem` exts
        then title
        else title <> " (needs extension: OverloadedRecordDot)"
        where
          title = "Convert `" <> name <> "` to record dot syntax"
          name = printOutputable se

collectRecSelsRule :: Recorder (WithPriority Log) -> Rules ()
collectRecSelsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectRecordSelectors nfp ->
  use TypeCheck nfp >>= \case
    Nothing -> pure ([], Nothing)
    Just tmr -> do
      let exts = getEnabledExtensions tmr
          recSels = getRecordSelectors tmr
      logWith recorder Debug (LogCollectedRecordSelectors recSels)
      let crsMap :: RangeMap RecordSelectorExpr
          crsMap = RangeMap.fromList location recSels
      pure ([], CRSR <$> Just crsMap <*> Just exts)
  where
    getEnabledExtensions :: TcModuleResult -> [Extension]
    getEnabledExtensions = getExtensions . tmrParsed

getRecordSelectors :: TcModuleResult -> [RecordSelectorExpr]
getRecordSelectors (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) =
    collectRecordSelectors valBinds

convertRecordSelectors :: RecordSelectorExpr -> Maybe TextEdit
convertRecordSelectors (RecordSelectorExpr l  se re) = TextEdit l <$> convertRecSel se re

convertRecSel :: Outputable (LHsExpr (GhcPass 'Renamed)) => LHsExpr (GhcPass 'Renamed) -> LHsExpr (GhcPass 'Renamed) -> Maybe Text
convertRecSel se re =
    Just $ printOutputable (parenthesizeHsExpr appPrec re) <> "." <> printOutputable se

-- It's important that we use everthingBut here, because if we used everything we would
-- get duplicates for every case that occures inside a HsExpanded expression.
collectRecordSelectors :: GenericQ [RecordSelectorExpr]
collectRecordSelectors = everythingBut (<>) (([], False) `mkQ` getRecSels)

getRecSels :: LHsExpr (GhcPass 'Renamed) -> ([RecordSelectorExpr], Bool)
-- When we stumble upon an occurance of HsExpanded, we only want to follow one branch
-- we do this here, by explicitly returning occurances from traversing the original branch,
-- and returning True, which keeps syb from implicitly continuing to traverse.
getRecSels (unLoc -> XExpr (HsExpanded a _)) = (collectRecordSelectors a, True)
#if __GLASGOW_HASKELL__ >= 903
-- applied record selection: "field record" or "field (record)" or "field field.record"
getRecSels e@(unLoc -> HsApp _ se@(unLoc -> HsRecSel _ _) re) =
  ( [ RecordSelectorExpr (realSrcSpanToRange realSpan') se re | RealSrcSpan realSpan' _ <- [ getLoc e ]], False)
-- Record selection where the field is being applied with the "$" operator: "field $ record"
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> HsRecSel _ _) (unLoc -> HsVar _ (unLoc -> d)) re)
    | d == dollarName = ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re | RealSrcSpan realSpan' _ <- [ getLoc e ]], False)
#else
getRecSels e@(unLoc -> HsApp _ se@(unLoc -> HsRecFld _ _) re) =
  ( [ RecordSelectorExpr (realSrcSpanToRange realSpan') se re | RealSrcSpan realSpan' _ <- [ getLoc e ]], False)
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> HsRecFld _ _) (unLoc -> HsVar _ (unLoc -> d)) re)
    | d == dollarName = ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re | RealSrcSpan realSpan' _ <- [ getLoc e ]], False)
#endif
getRecSels _ = ([], False)

collectRecSelResult :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m CollectRecordSelectorsResult
collectRecSelResult ideState =
  handleMaybeM "Unable to TypeCheck"
    . liftIO
    . runAction "overloadedRecordDot.collectRecordSelectors" ideState
    . use CollectRecordSelectors

