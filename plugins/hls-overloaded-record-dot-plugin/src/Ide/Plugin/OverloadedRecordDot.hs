{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.OverloadedRecordDot
  ( descriptor
  , Log
  ) where

-- based off of Berk Okzuturk's hls-explicit-records-fields-plugin

import           Control.Lens                         ((^.))
import           Control.Monad                        (replicateM)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Except           (ExceptT)
import           Data.Aeson                           (FromJSON, ToJSON, toJSON)
import           Data.Generics                        (GenericQ, everythingBut,
                                                       mkQ)
import qualified Data.IntMap.Strict                   as IntMap
import qualified Data.Map                             as Map
import           Data.Maybe                           (mapMaybe, maybeToList)
import           Data.Text                            (Text)
import           Data.Unique                          (hashUnique, newUnique)
import           Development.IDE                      (IdeState,
                                                       NormalizedFilePath,
                                                       Pretty (..), Range,
                                                       Recorder (..), Rules,
                                                       WithPriority (..),
                                                       realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes       (TcModuleResult (..),
                                                       TypeCheck (..))
import           Development.IDE.Core.Shake           (define, useWithStale)
import qualified Development.IDE.Core.Shake           as Shake

import           Control.DeepSeq                      (rwhnf)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentRange)
import           Development.IDE.GHC.Compat           (Extension (OverloadedRecordDot),
                                                       GhcPass, HsExpr (..),
                                                       LHsExpr, Pass (..),
                                                       appPrec, dollarName,
                                                       getLoc, hs_valds,
                                                       parenthesizeHsExpr,
                                                       pattern RealSrcSpan,
                                                       unLoc
#if __GLASGOW_HASKELL__ >= 913
                                                       , unLocWithUserRdr
#endif
                                                       )
import           Development.IDE.GHC.Util             (getExtensions,
                                                       printOutputable)
import           Development.IDE.Graph                (RuleResult)
import           Development.IDE.Graph.Classes        (Hashable, NFData (rnf))
import           Development.IDE.Spans.Pragmas        (NextPragmaInfo (..),
                                                       getFirstPragma,
                                                       insertNewPragma)
import           GHC.Generics                         (Generic)
import           Ide.Logger                           (Priority (..),
                                                       cmapWithPrio, logWith,
                                                       (<+>))
import           Ide.Plugin.Error                     (PluginError (..),
                                                       getNormalizedFilePathE,
                                                       handleMaybe)
import           Ide.Plugin.RangeMap                  (RangeMap)
import qualified Ide.Plugin.RangeMap                  as RangeMap
import           Ide.Plugin.Resolve                   (mkCodeActionHandlerWithResolve)
import           Ide.Types                            (PluginDescriptor (..),
                                                       PluginId (..),
                                                       PluginMethodHandler,
                                                       ResolveFunction,
                                                       defaultPluginDescriptor)
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        (Method (..))
import           Language.LSP.Protocol.Types          (CodeAction (..),
                                                       CodeActionKind (CodeActionKind_RefactorRewrite),
                                                       CodeActionParams (..),
                                                       TextEdit (..), Uri (..),
                                                       WorkspaceEdit (WorkspaceEdit, _changeAnnotations, _changes, _documentChanges),
                                                       type (|?) (..))


#if __GLASGOW_HASKELL__ < 910
import           Development.IDE.GHC.Compat           (HsExpansion (HsExpanded))
#else
import           Development.IDE.GHC.Compat           (XXExprGhcRn (..))
#endif


data Log
    = LogShake Shake.Log
    | LogCollectedRecordSelectors [RecordSelectorExpr]
    | forall a. (Pretty a) => LogResolve a

instance Pretty Log where
    pretty = \case
        LogShake shakeLog -> pretty shakeLog
        LogCollectedRecordSelectors recs -> "Collected record selectors:"
                                                <+> pretty recs
        LogResolve msg -> pretty msg

data CollectRecordSelectors = CollectRecordSelectors
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecordSelectors
instance NFData CollectRecordSelectors

data CollectRecordSelectorsResult = CRSR
    { -- |We store everything in here that we need to create the unresolved
      -- codeAction: the range, an uniquely identifiable int, and the selector
      --selector expression  (HSExpr) that we use to generate the name
      records           :: RangeMap (Int, HsExpr (GhcPass 'Renamed))
      -- |This is for when we need to fully generate a textEdit. It contains the
      -- whole expression we are interested in indexed to the unique id we got
      -- from the previous field
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
    pretty (RecordSelectorExpr _ rs se) = pretty (printOutputable rs) <> ":"
                                            <+> pretty (printOutputable se)

instance NFData RecordSelectorExpr where
  rnf = rwhnf

-- |The data that is serialized and placed in the data field of resolvable
-- code actions
data ORDResolveData = ORDRD {
  -- |We need the uri to get shake results
  uri      :: Uri
  -- |The unique id that allows us to find the specific codeAction we want
, uniqueID :: Int
} deriving (Generic, Show)
instance ToJSON ORDResolveData
instance FromJSON ORDResolveData

descriptor :: Recorder (WithPriority Log) -> PluginId
                -> PluginDescriptor IdeState
descriptor recorder plId =
  let resolveRecorder = cmapWithPrio LogResolve recorder
      pluginHandler = mkCodeActionHandlerWithResolve resolveRecorder codeActionProvider resolveProvider
  in (defaultPluginDescriptor plId "Provides a code action to convert record selector usage to use overloaded record dot syntax")
    { pluginHandlers = pluginHandler
    , pluginRules = collectRecSelsRule recorder
    }

resolveProvider :: ResolveFunction IdeState ORDResolveData 'Method_CodeActionResolve
resolveProvider ideState plId ca uri (ORDRD _ int) =
  do
    nfp <- getNormalizedFilePathE uri
    CRSR _ crsDetails exts <- collectRecSelResult ideState nfp
    pragma <- getFirstPragma plId ideState nfp
    rse <- handleMaybe PluginStaleResolve $ IntMap.lookup int crsDetails
    pure $ ca {_edit = mkWorkspaceEdit uri rse exts pragma}

codeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionProvider ideState _ (CodeActionParams _ _ caDocId caRange _) =
    do
        nfp <- getNormalizedFilePathE (caDocId ^. L.uri)
        CRSR crsMap _ exts <- collectRecSelResult ideState nfp
        let mkCodeAction (crsM, nse)  = InR CodeAction
                { -- We pass the record selector to the title function, so that
                  -- we can have the name of the record selector in the title of
                  -- the codeAction. This allows the user can easily distinguish
                  -- between the different codeActions when using nested record
                  -- selectors, the disadvantage is we need to print out the
                  -- name of the record selector which will decrease performance
                  _title = mkCodeActionTitle exts nse
                , _kind = Just CodeActionKind_RefactorRewrite
                , _diagnostics = Nothing
                , _isPreferred = Nothing
                , _disabled = Nothing
                , _edit = Nothing
                , _command = Nothing
                , _data_ = Just $ toJSON $ ORDRD (caDocId ^. L.uri) crsM
                }
            actions = map mkCodeAction (RangeMap.filterByRange caRange crsMap)
        pure $ InL actions
    where
    mkCodeActionTitle :: [Extension] -> HsExpr (GhcPass 'Renamed) -> Text
    mkCodeActionTitle exts se =
        if OverloadedRecordDot `elem` exts
            then title
            else title <> " (needs extension: OverloadedRecordDot)"
        where
            title = "Convert `" <> printOutputable se <> "` to record dot syntax"

mkWorkspaceEdit:: Uri -> RecordSelectorExpr -> [Extension] -> NextPragmaInfo-> Maybe WorkspaceEdit
mkWorkspaceEdit uri recSel exts pragma =
    Just $ WorkspaceEdit
    { _changes =
        Just (Map.singleton uri (convertRecordSelectors recSel : maybeToList pragmaEdit))
    , _documentChanges = Nothing
    , _changeAnnotations = Nothing}
    where pragmaEdit =
            if OverloadedRecordDot `elem` exts
            then Nothing
            else Just $ insertNewPragma pragma OverloadedRecordDot

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
            -- We are creating a list as long as our rec selectors of unique int s
            -- created by calling hashUnique on a Unique. The reason why we are
            -- extracting the ints is because they don't need any work to serialize.
            uniques <- liftIO $ replicateM (length recSels) (hashUnique <$> newUnique)
            logWith recorder Debug (LogCollectedRecordSelectors recSels)
            let crsUniquesAndDetails =  zip uniques recSels
                -- We need the rangeMap to be able to filter by range later
                rangeAndUnique = toRangeAndUnique <$> crsUniquesAndDetails
                crsMap :: RangeMap (Int,  HsExpr (GhcPass 'Renamed))
                crsMap = RangeMap.fromList' rangeAndUnique
            pure ([], CRSR <$> Just crsMap <*> Just (IntMap.fromList crsUniquesAndDetails) <*> Just exts)
    where getEnabledExtensions :: TcModuleResult -> [Extension]
          getEnabledExtensions = getExtensions . tmrParsed
          getRecordSelectors :: TcModuleResult -> [RecordSelectorExpr]
#if __GLASGOW_HASKELL__ >= 910
          getRecordSelectors (tmrRenamed -> (hs_valds -> valBinds,_,_,_,_)) = collectRecordSelectors valBinds
#else
          getRecordSelectors (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) = collectRecordSelectors valBinds
#endif
          rewriteRange :: PositionMapping -> RecordSelectorExpr
                            -> Maybe RecordSelectorExpr
          rewriteRange pm recSel =
            case toCurrentRange pm (location recSel) of
                Just newLoc -> Just $ recSel{location = newLoc}
                Nothing     -> Nothing
          toRangeAndUnique (uid, RecordSelectorExpr l (unLoc -> se) _) = (l, (uid, se))

convertRecordSelectors :: RecordSelectorExpr ->  TextEdit
convertRecordSelectors RecordSelectorExpr{..} =
    TextEdit location $ convertRecSel selectorExpr recordExpr

-- |Converts a record selector expression into record dot syntax, currently we
-- are using printOutputable to do it. We are also letting GHC decide when to
-- parenthesize the record expression
convertRecSel :: LHsExpr (GhcPass 'Renamed) -> LHsExpr (GhcPass 'Renamed) -> Text
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
#if __GLASGOW_HASKELL__ >= 910
getRecSels (unLoc -> XExpr (ExpandedThingRn a _)) = (collectRecordSelectors a, True)
#else
getRecSels (unLoc -> XExpr (HsExpanded a _)) = (collectRecordSelectors a, True)
#endif
-- applied record selection: "selector record" or "selector (record)" or
-- "selector selector2.record2"
#if __GLASGOW_HASKELL__ >= 911
getRecSels e@(unLoc -> HsApp _ se@(unLoc -> XExpr (HsRecSelRn _)) re) =
#else
getRecSels e@(unLoc -> HsApp _ se@(unLoc -> HsRecSel _ _) re) =
#endif
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan') se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
-- Record selection where the field is being applied with the "$" operator:
-- "selector $ record"
#if __GLASGOW_HASKELL__ >= 913
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> XExpr (HsRecSelRn _))
                        (unLoc -> HsVar _ (unLocWithUserRdr -> d)) re) | d == dollarName =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
#elif __GLASGOW_HASKELL__ >= 911
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> XExpr (HsRecSelRn _))
                        (unLoc -> HsVar _ (unLoc -> d)) re) | d == dollarName =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
#else
getRecSels e@(unLoc -> OpApp _ se@(unLoc -> HsRecSel _ _)
                        (unLoc -> HsVar _ (unLoc -> d)) re) | d == dollarName =
    ( [ RecordSelectorExpr (realSrcSpanToRange realSpan')  se re
      | RealSrcSpan realSpan' _ <- [ getLoc e ] ], False )
#endif
getRecSels _ = ([], False)

collectRecSelResult :: MonadIO m => IdeState -> NormalizedFilePath
                        -> ExceptT PluginError m CollectRecordSelectorsResult
collectRecSelResult ideState =
    runActionE "overloadedRecordDot.collectRecordSelectors" ideState
        . useE CollectRecordSelectors

