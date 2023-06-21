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

import           Control.Lens                         (_Just, (^.), (^?))
import           Control.Monad                        (replicateM)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (ExceptT)
import           Data.Aeson                           (FromJSON, Result (..),
                                                       ToJSON, fromJSON, toJSON)
import           Data.Generics                        (GenericQ, everything,
                                                       everythingBut, mkQ)
import qualified Data.IntMap.Strict                   as IntMap
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromJust, mapMaybe,
                                                       maybeToList)
import           Data.Text                            (Text)
import           Data.Unique                          (hashUnique, newUnique)
import           Development.IDE                      (IdeState,
                                                       NormalizedFilePath,
                                                       NormalizedUri,
                                                       Pretty (..), Range,
                                                       Recorder (..), Rules,
                                                       WithPriority (..),
                                                       realSrcSpanToRange)
import           Development.IDE.Core.Rules           (runAction)
import           Development.IDE.Core.RuleTypes       (TcModuleResult (..),
                                                       TypeCheck (..))
import           Development.IDE.Core.Shake           (define, use,
                                                       useWithStale)
import qualified Development.IDE.Core.Shake           as Shake

#if __GLASGOW_HASKELL__ >= 903
import           Development.IDE.GHC.Compat           (HsExpr (HsRecSel))
#else
import           Development.IDE.GHC.Compat           (HsExpr (HsRecFld))
#endif

import           Control.DeepSeq                      (rwhnf)
import           Development.IDE.Core.PositionMapping (PositionMapping (PositionMapping),
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat           (Extension (OverloadedRecordDot),
                                                       GhcPass,
                                                       HsExpansion (HsExpanded),
                                                       HsExpr (HsApp, HsPar, HsVar, OpApp, XExpr),
                                                       LHsExpr, Outputable,
                                                       Pass (..), RealSrcSpan,
                                                       appPrec, dollarName,
                                                       getLoc, hs_valds,
                                                       parenthesizeHsContext,
                                                       parenthesizeHsExpr,
                                                       pattern RealSrcSpan,
                                                       unLoc)
import           Development.IDE.GHC.Util             (getExtensions,
                                                       printOutputable)
import           Development.IDE.Graph                (RuleResult)
import           Development.IDE.Graph.Classes        (Hashable, NFData (rnf))
import           Development.IDE.Spans.Pragmas        (NextPragmaInfo (..),
                                                       getFirstPragma,
                                                       insertNewPragma)
import           Development.IDE.Types.Logger         (Priority (..),
                                                       cmapWithPrio, logWith,
                                                       (<+>))
import           GHC.Generics                         (Generic)
import           Ide.Plugin.RangeMap                  (RangeMap)
import qualified Ide.Plugin.RangeMap                  as RangeMap
import           Ide.PluginUtils                      (getNormalizedFilePath,
                                                       handleMaybeM,
                                                       pluginResponse)
import           Ide.Types                            (PluginDescriptor (..),
                                                       PluginId (..),
                                                       PluginMethodHandler,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Protocol.Lens           (HasChanges (changes))
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        (Method (..),
                                                       SMethod (..))
import           Language.LSP.Protocol.Types          (CodeAction (..),
                                                       CodeActionKind (CodeActionKind_RefactorRewrite),
                                                       CodeActionParams (..),
                                                       Command, TextEdit (..),
                                                       Uri (..),
                                                       WorkspaceEdit (WorkspaceEdit),
                                                       fromNormalizedUri,
                                                       normalizedFilePathToUri,
                                                       type (|?) (..))
import           Language.LSP.Server                  (getClientCapabilities)
data Log
    = LogShake Shake.Log
    | LogCollectedRecordSelectors [RecordSelectorExpr]
    | LogTextEdits [TextEdit]

instance Pretty Log where
    pretty = \case
        LogShake shakeLog -> pretty shakeLog
        LogCollectedRecordSelectors recs -> "Collected record selectors:"
                                                <+> pretty recs

data CollectRecordSelectors = CollectRecordSelectors
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecordSelectors
instance NFData CollectRecordSelectors

data CollectRecordSelectorsResult = CRSR
    { records           :: RangeMap Int
    , recordInfos       :: IntMap.IntMap RecordSelectorExpr
    , enabledExtensions :: [Extension]
    }
    deriving (Generic)

instance NFData CollectRecordSelectorsResult

instance Show CollectRecordSelectorsResult where
    show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecordSelectors = CollectRecordSelectorsResult

-- |Where we store our collected record selectors
data RecordSelectorExpr = RecordSelectorExpr
    { -- |The location of the matched expression
    location     :: Range,
    -- |The record selector, this is found in front of recordExpr, but get's
    -- placed after it when converted into record dot syntax
    selectorExpr :: LHsExpr (GhcPass 'Renamed),
    -- |The record expression. The only requirement is that it evaluates to a
    -- record in the end
    recordExpr   :: LHsExpr (GhcPass 'Renamed) }

instance Pretty RecordSelectorExpr where
    pretty (RecordSelectorExpr l rs se) = pretty (printOutputable rs) <> ":"
                                            <+> pretty (printOutputable se)

instance NFData RecordSelectorExpr where
  rnf = rwhnf

data ORDResolveData = ORDRD {
  uri      :: Uri
, uniqueID :: Int
} deriving (Generic, Show)
instance ToJSON ORDResolveData
instance FromJSON ORDResolveData

descriptor :: Recorder (WithPriority Log) -> PluginId
                -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
    { pluginHandlers =
      mkPluginHandler SMethod_TextDocumentCodeAction codeActionProvider
      <> mkPluginHandler SMethod_CodeActionResolve resolveProvider

    , pluginRules = collectRecSelsRule recorder
    }

resolveProvider :: PluginMethodHandler IdeState 'Method_CodeActionResolve
resolveProvider ideState pId ca@(CodeAction _ _ _ _ _ _ _ (Just resData)) =
    pluginResponse $ do
        case fromJSON $ resData of
            Success (ORDRD uri int) -> do
                nfp <- getNormalizedFilePath uri
                CRSR _ crsDetails exts <- collectRecSelResult ideState nfp
                pragma <- getFirstPragma pId ideState nfp
                let pragmaEdit =
                        if OverloadedRecordDot `elem` exts
                        then Nothing
                        else Just $ insertNewPragma pragma OverloadedRecordDot
                    edits (Just crs) = convertRecordSelectors crs : maybeToList pragmaEdit
                    edits _ = []
                    changes = Just $ WorkspaceEdit
                                    (Just (Map.singleton (fromNormalizedUri
                                            (normalizedFilePathToUri nfp))
                                            (edits (IntMap.lookup int crsDetails))))
                                    Nothing Nothing
                pure $ ca {_edit = changes}
            _ -> pure ca

codeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionProvider ideState pId (CodeActionParams _ _ caDocId caRange _) =
    pluginResponse $ do
        nfp <- getNormalizedFilePath (caDocId ^. L.uri)
        pragma <- getFirstPragma pId ideState nfp
        caps <- lift getClientCapabilities
        CRSR crsMap crsDetails exts <- collectRecSelResult ideState nfp
        let supportsResolve :: Maybe Bool
            supportsResolve = caps ^? L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just
            pragmaEdit =
                if OverloadedRecordDot `elem` exts
                then Nothing
                else Just $ insertNewPragma pragma OverloadedRecordDot
            edits (Just crs) = convertRecordSelectors crs : maybeToList pragmaEdit
            edits _ = []
            changes crsM crsD =
                case supportsResolve of
                    Just False -> Just $ WorkspaceEdit
                                    (Just (Map.singleton (fromNormalizedUri
                                            (normalizedFilePathToUri nfp))
                                            (edits (IntMap.lookup crsM crsD))))
                                    Nothing Nothing
                    _ -> Nothing
            resolveData crsM =
                case supportsResolve of
                    Just True -> Just $ toJSON $ ORDRD (caDocId ^. L.uri) crsM
                    _         -> Nothing
            mkCodeAction crsD crsM  = InR CodeAction
                { -- We pass the record selector to the title function, so that
                  -- we can have the name of the record selector in the title of
                  -- the codeAction. This allows the user can easily distinguish
                  -- between the different codeActions when using nested record
                  -- selectors, the disadvantage is we need to print out the
                  -- name of the record selector which will decrease performance
                _title = mkCodeActionTitle exts crsM crsD
                , _kind = Just CodeActionKind_RefactorRewrite
                , _diagnostics = Nothing
                , _isPreferred = Nothing
                , _disabled = Nothing
                , _edit = changes crsM crsD
                , _command = Nothing
                , _data_ = resolveData crsM
                }
            actions = map (mkCodeAction crsDetails) (RangeMap.filterByRange caRange crsMap)
        pure $ InL actions
    where
    mkCodeActionTitle :: [Extension] -> Int -> IntMap.IntMap RecordSelectorExpr-> Text
    mkCodeActionTitle exts crsM crsD =
        if OverloadedRecordDot `elem` exts
            then title
            else title <> " (needs extension: OverloadedRecordDot)"
        where
            title = "Convert `" <> name (IntMap.lookup crsM crsD) <> "` to record dot syntax"
            name (Just (RecordSelectorExpr _ se _)) = printOutputable se
            name _                                  = ""

collectRecSelsRule :: Recorder (WithPriority Log) -> Rules ()
collectRecSelsRule recorder = define (cmapWithPrio LogShake recorder) $
    \CollectRecordSelectors nfp ->
        useWithStale TypeCheck nfp >>= \case
        -- `useWithStale` here allows us to be able to return codeActions even
        -- if the file does not typecheck. The disadvantage being that we
        -- sometimes will end up corrupting code. This is most obvious in that
        -- used code actions will continue to be presented, and when applied
        -- multiple times will almost always cause code corruption.
        Nothing -> pure ([], Nothing)
        Just (tmr, pm) -> do
            let -- We need the file's extensions to check whether we need to add
                -- the OverloadedRecordDot pragma
                exts = getEnabledExtensions tmr
                recSels = mapMaybe (rewriteRange pm) (getRecordSelectors tmr)
            uniques <- liftIO $ replicateM (length recSels) (hashUnique <$> newUnique)
            logWith recorder Debug (LogCollectedRecordSelectors recSels)
            let crsDetails = IntMap.fromList $ zip uniques recSels
                -- We need the rangeMap to be able to filter by range later
                rangeAndUnique = mapM (\x -> (, x) . location <$> IntMap.lookup x crsDetails) uniques
                crsMap :: Maybe (RangeMap Int)
                crsMap = RangeMap.fromList' <$> rangeAndUnique
                crsDetails :: IntMap.IntMap RecordSelectorExpr
            pure ([], CRSR <$> crsMap <*> Just crsDetails <*> Just exts)
    where getEnabledExtensions :: TcModuleResult -> [Extension]
          getEnabledExtensions = getExtensions . tmrParsed
          getRecordSelectors :: TcModuleResult -> [RecordSelectorExpr]
          getRecordSelectors (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) =
            collectRecordSelectors valBinds
          rewriteRange :: PositionMapping -> RecordSelectorExpr
                            -> Maybe RecordSelectorExpr
          rewriteRange pm recSel =
            case toCurrentRange pm (location recSel) of
                Just newLoc -> Just $ recSel{location = newLoc}
                Nothing     -> Nothing

convertRecordSelectors :: RecordSelectorExpr ->  TextEdit
convertRecordSelectors (RecordSelectorExpr l se re) =
    TextEdit l $ convertRecSel se re

-- |Converts a record selector expression into record dot syntax, currently we
-- are using printOutputable to do it. We are also letting GHC decide when to
-- parenthesize the record expression
convertRecSel :: Outputable (LHsExpr (GhcPass 'Renamed))
                    => LHsExpr (GhcPass 'Renamed)
                    -> LHsExpr (GhcPass 'Renamed) -> Text
convertRecSel se re = printOutputable (parenthesizeHsExpr appPrec re) <> "."
                        <> printOutputable se

collectRecordSelectors :: GenericQ [RecordSelectorExpr]
-- It's important that we use everthingBut here, because if we used everything
-- we would get duplicates for every case that occurs inside a HsExpanded
-- expression. Please see the test MultilineExpanded.hs
collectRecordSelectors = everythingBut (<>) (([], False) `mkQ` getRecSels)

-- |We want to return a list here, because on the occasion that we encounter a
-- HsExpanded expression, we want to return all the results from recursing on
-- one branch, which could be multiple matches. Again see MultilineExpanded.hs
getRecSels :: LHsExpr (GhcPass 'Renamed) -> ([RecordSelectorExpr], Bool)
-- When we stumble upon an occurrence of HsExpanded, we only want to follow one
-- branch. We do this here, by explicitly returning occurrences from traversing
-- the original branch, and returning True, which keeps syb from implicitly
-- continuing to traverse.
getRecSels (unLoc -> XExpr (HsExpanded a _)) = (collectRecordSelectors a, True)
#if __GLASGOW_HASKELL__ >= 903
-- applied record selection: "selector record" or "selector (record)" or
-- "selector selector2.record2"
getRecSels e@(unLoc -> HsApp _ se@(unLoc -> HsRecSel _ _) re) =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan') se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
-- Record selection where the field is being applied with the "$" operator:
-- "selector $ record"
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> HsRecSel _ _)
                        (unLoc -> HsVar _ (unLoc -> d)) re) | d == dollarName =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
#else
getRecSels e@(unLoc -> HsApp _ se@(unLoc -> HsRecFld _ _) re) =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan') se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> HsRecFld _ _)
                        (unLoc -> HsVar _ (unLoc -> d)) re) | d == dollarName =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
#endif
getRecSels _ = ([], False)

collectRecSelResult :: MonadIO m => IdeState -> NormalizedFilePath
                        -> ExceptT String m CollectRecordSelectorsResult
collectRecSelResult ideState =
    handleMaybeM "Unable to TypeCheck"
        . liftIO
        . runAction "overloadedRecordDot.collectRecordSelectors" ideState
        . use CollectRecordSelectors

