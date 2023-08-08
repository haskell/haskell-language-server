{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Ide.Plugin.ExplicitFields
  ( descriptor
  , Log
  ) where

import           Control.Lens                     ((&), (?~), (^.))
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Maybe
import           Data.Aeson                       (toJSON)
import           Data.Generics                    (GenericQ, everything,
                                                   everythingBut, extQ, mkQ)
import qualified Data.IntMap.Strict               as IntMap
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust,
                                                   maybeToList)
import           Data.Text                        (Text)
import           Data.Unique                      (hashUnique, newUnique)

import           Control.Monad                    (replicateM)
import           Development.IDE                  (IdeState, Pretty (..), Range,
                                                   Recorder (..), Rules,
                                                   WithPriority (..),
                                                   defineNoDiagnostics,
                                                   realSrcSpanToRange, viaShow)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes   (TcModuleResult (..),
                                                   TypeCheck (..))
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.GHC.Compat       (HsConDetails (RecCon),
                                                   HsExpansion (HsExpanded),
                                                   HsExpr (XExpr),
                                                   HsRecFields (..), LPat,
                                                   Outputable, getLoc,
                                                   recDotDot, unLoc)
import           Development.IDE.GHC.Compat.Core  (Extension (NamedFieldPuns),
                                                   GhcPass,
                                                   HsExpr (RecordCon, rcon_flds),
                                                   HsRecField, LHsExpr,
                                                   LocatedA, Name, Pass (..),
                                                   Pat (..), RealSrcSpan,
                                                   UniqFM, conPatDetails,
                                                   emptyUFM, hfbPun, hfbRHS,
                                                   hs_valds, lookupUFM,
                                                   mapConPatDetail, mapLoc,
                                                   pattern RealSrcSpan,
                                                   plusUFM_C, ufmToIntMap,
                                                   unitUFM)
import           Development.IDE.GHC.Util         (getExtensions,
                                                   printOutputable)
import           Development.IDE.Graph            (RuleResult)
import           Development.IDE.Graph.Classes    (Hashable, NFData (rnf))
import           Development.IDE.Spans.Pragmas    (NextPragmaInfo (..),
                                                   getFirstPragma,
                                                   insertNewPragma)
import           GHC.Generics                     (Generic)
import           Ide.Logger                       (Priority (..), cmapWithPrio,
                                                   logWith, (<+>))
import           Ide.Plugin.Error                 (PluginError (PluginStaleResolve),
                                                   getNormalizedFilePathE,
                                                   handleMaybe)
import           Ide.Plugin.RangeMap              (RangeMap)
import qualified Ide.Plugin.RangeMap              as RangeMap
import           Ide.Plugin.Resolve               (mkCodeActionWithResolveAndCommand)
import           Ide.Types                        (PluginDescriptor (..),
                                                   PluginId (..),
                                                   PluginMethodHandler,
                                                   ResolveFunction,
                                                   defaultPluginDescriptor)
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (..))
import           Language.LSP.Protocol.Types      (CodeAction (..),
                                                   CodeActionKind (CodeActionKind_RefactorRewrite),
                                                   CodeActionParams (..),
                                                   Command, TextEdit (..),
                                                   WorkspaceEdit (WorkspaceEdit),
                                                   type (|?) (InL, InR))


data Log
  = LogShake Shake.Log
  | LogCollectedRecords [RecordInfo]
  | LogRenderedRecords [TextEdit]
  | forall a. (Pretty a) => LogResolve a


instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog
    LogCollectedRecords recs -> "Collected records with wildcards:" <+> pretty recs
    LogRenderedRecords recs -> "Rendered records:" <+> viaShow recs
    LogResolve msg -> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  let resolveRecorder = cmapWithPrio LogResolve recorder
      (carCommands, caHandlers) = mkCodeActionWithResolveAndCommand resolveRecorder plId codeActionProvider codeActionResolveProvider
  in (defaultPluginDescriptor plId)
  { pluginHandlers = caHandlers
  , pluginCommands = carCommands
  , pluginRules = collectRecordsRule recorder
  }

codeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionProvider ideState _ (CodeActionParams _ _ docId range _) = do
  nfp <- getNormalizedFilePathE (docId ^. L.uri)
  CRR{..} <- runActionE "ExplicitFields.CollectRecords" ideState $ useE CollectRecords nfp
  let actions = map (mkCodeAction enabledExtensions) (RangeMap.filterByRange range crCodeActions)
  pure $ InL actions
  where
    mkCodeAction ::  [Extension] ->  Int -> Command |? CodeAction
    mkCodeAction  exts uid = InR CodeAction
      { _title = "Expand record wildcard"
                  <> if NamedFieldPuns `elem` exts
                    then mempty
                    else " (needs extension: NamedFieldPuns)"
      , _kind = Just CodeActionKind_RefactorRewrite
      , _diagnostics = Nothing
      , _isPreferred = Nothing
      , _disabled = Nothing
      , _edit = Nothing
      , _command = Nothing
      , _data_ = Just $ toJSON uid
      }

codeActionResolveProvider :: ResolveFunction IdeState Int 'Method_CodeActionResolve
codeActionResolveProvider ideState pId ca uri uid = do
  nfp <- getNormalizedFilePathE uri
  pragma <- getFirstPragma pId ideState nfp
  CRR{..} <- runActionE "ExplicitFields.CollectRecords" ideState $ useE CollectRecords nfp
  record <- handleMaybe PluginStaleResolve $ IntMap.lookup uid crCodeActionResolve
  let edits = maybeToList (renderRecordInfo nameMap record)
              <> maybeToList (pragmaEdit enabledExtensions pragma)
  pure $ ca & L.edit ?~ mkWorkspaceEdit edits
  where
    mkWorkspaceEdit ::[TextEdit] -> WorkspaceEdit
    mkWorkspaceEdit edits = WorkspaceEdit (Just $ Map.singleton uri edits) Nothing Nothing
    pragmaEdit :: [Extension] -> NextPragmaInfo -> Maybe TextEdit
    pragmaEdit exts pragma = if NamedFieldPuns `elem` exts
                      then Nothing
                      else Just $ insertNewPragma pragma NamedFieldPuns

collectRecordsRule :: Recorder (WithPriority Log) -> Rules ()
collectRecordsRule recorder =
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \CollectRecords nfp -> runMaybeT $ do
  tmr <- useMT TypeCheck nfp
  let recs = getRecords tmr
  logWith recorder Debug (LogCollectedRecords recs)
  uniques <- liftIO $ replicateM (length recs) (hashUnique <$> newUnique)
  let recsWithUniques = zip uniques recs
      crCodeActions = RangeMap.fromList' (toRangeAndUnique <$> recsWithUniques)
      crCodeActionResolve = IntMap.fromList recsWithUniques
      nameMap = getNames tmr
      enabledExtensions = getEnabledExtensions tmr
  pure CRR {crCodeActions, crCodeActionResolve, nameMap, enabledExtensions}
  where
    getEnabledExtensions :: TcModuleResult -> [Extension]
    getEnabledExtensions =  getExtensions . tmrParsed
    toRangeAndUnique (uid, recordInfo) = (recordInfoToRange recordInfo, uid)

getRecords :: TcModuleResult -> [RecordInfo]
getRecords (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) =
  collectRecords valBinds

-- | Collects all 'Name's of a given source file, to be used
-- in the variable usage analysis.
getNames :: TcModuleResult -> NameMap
getNames (tmrRenamed -> (group,_,_,_)) = NameMap (collectNames group)

data CollectRecords = CollectRecords
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecords
instance NFData CollectRecords

data CollectRecordsResult = CRR
  { crCodeActions       :: RangeMap Int
  , crCodeActionResolve :: IntMap.IntMap RecordInfo
  , nameMap             :: NameMap
  , enabledExtensions   :: [Extension]
  }
  deriving (Generic)

instance NFData CollectRecordsResult
instance NFData RecordInfo

instance Show CollectRecordsResult where
  show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecords = CollectRecordsResult


-- As with `GhcExtension`, this newtype exists mostly to attach
-- an `NFData` instance to `UniqFM`.(without resorting to creating an orphan instance).
newtype NameMap = NameMap (UniqFM Name [Name])

instance NFData NameMap where
  rnf (NameMap (ufmToIntMap -> m)) = rnf m

data RecordInfo
  = RecordInfoPat RealSrcSpan (Pat (GhcPass 'Renamed))
  | RecordInfoCon RealSrcSpan (HsExpr (GhcPass 'Renamed))
  deriving (Generic)

instance Pretty RecordInfo where
  pretty (RecordInfoPat ss p) = pretty (printOutputable ss) <> ":" <+> pretty (printOutputable p)
  pretty (RecordInfoCon ss e) = pretty (printOutputable ss) <> ":" <+> pretty (printOutputable e)

recordInfoToRange :: RecordInfo -> Range
recordInfoToRange (RecordInfoPat ss _) = realSrcSpanToRange ss
recordInfoToRange (RecordInfoCon ss _) = realSrcSpanToRange ss

renderRecordInfo :: NameMap -> RecordInfo -> Maybe TextEdit
renderRecordInfo names (RecordInfoPat ss pat) = TextEdit (realSrcSpanToRange ss) <$> showRecordPat names pat
renderRecordInfo _ (RecordInfoCon ss expr) = TextEdit (realSrcSpanToRange ss) <$> showRecordCon expr

-- | Checks if a 'Name' is referenced in the given map of names. The
-- 'hasNonBindingOcc' check is necessary in order to make sure that only the
-- references at the use-sites are considered (i.e. the binding occurence
-- is excluded). For more information regarding the structure of the map,
-- refer to the documentation of 'collectNames'.
referencedIn :: Name -> NameMap -> Bool
referencedIn name (NameMap names) = maybe True hasNonBindingOcc $ lookupUFM names name
  where
    hasNonBindingOcc :: [Name] -> Bool
    hasNonBindingOcc = (> 1) . length

-- Default to leaving the element in if somehow a name can't be extracted (i.e.
-- `getName` returns `Nothing`).
filterReferenced :: (a -> Maybe Name) -> NameMap -> [a] -> [a]
filterReferenced getName names = filter (\x -> maybe True (`referencedIn` names) (getName x))

preprocessRecordPat
  :: p ~ GhcPass 'Renamed
  => NameMap
  -> HsRecFields p (LPat p)
  -> HsRecFields p (LPat p)
preprocessRecordPat = preprocessRecord (getFieldName . unLoc)
  where
    getFieldName x = case unLoc (hfbRHS x) of
      VarPat _ x' -> Just $ unLoc x'
      _           -> Nothing

-- No need to check the name usage in the record construction case
preprocessRecordCon :: HsRecFields (GhcPass c) arg -> HsRecFields (GhcPass c) arg
preprocessRecordCon = preprocessRecord (const Nothing) (NameMap emptyUFM)

-- This function does two things:
-- 1) Tweak the AST type so that the pretty-printed record is in the
--    expanded form
-- 2) Determine the unused record fields so that they are filtered out
--    of the final output
--
-- Regarding first point:
-- We make use of the `Outputable` instances on AST types to pretty-print
-- the renamed and expanded records back into source form, to be substituted
-- with the original record later. However, `Outputable` instance of
-- `HsRecFields` does smart things to print the records that originally had
-- wildcards in their original form (i.e. with dots, without field names),
-- even after the wildcard is removed by the renamer pass. This is undesirable,
-- as we want to print the records in their fully expanded form.
-- Here `rec_dotdot` is set to `Nothing` so that fields are printed without
-- such post-processing.
preprocessRecord
  :: p ~ GhcPass c
  => (LocatedA (HsRecField p arg) -> Maybe Name)
  -> NameMap
  -> HsRecFields p arg
  -> HsRecFields p arg
preprocessRecord getName names flds = flds { rec_dotdot = Nothing , rec_flds = rec_flds' }
  where
    no_pun_count = fromMaybe (length (rec_flds flds)) (recDotDot flds)
    -- Field binds of the explicit form (e.g. `{ a = a' }`) should be
    -- left as is, hence the split.
    (no_puns, puns) = splitAt no_pun_count (rec_flds flds)
    -- `hsRecPun` is set to `True` in order to pretty-print the fields as field
    -- puns (since there is similar mechanism in the `Outputable` instance as
    -- explained above).
    puns' = map (mapLoc (\fld -> fld { hfbPun = True })) puns
    -- Unused fields are filtered out so that they don't end up in the expanded
    -- form.
    punsUsed = filterReferenced getName names puns'
    rec_flds' = no_puns <> punsUsed

showRecordPat :: Outputable (Pat (GhcPass 'Renamed)) => NameMap -> Pat (GhcPass 'Renamed) -> Maybe Text
showRecordPat names = fmap printOutputable . mapConPatDetail (\case
  RecCon flds -> Just $ RecCon (preprocessRecordPat names flds)
  _           -> Nothing)

showRecordCon :: Outputable (HsExpr (GhcPass c)) => HsExpr (GhcPass c) -> Maybe Text
showRecordCon expr@(RecordCon _ _ flds) =
  Just $ printOutputable $
    expr { rcon_flds = preprocessRecordCon flds }
showRecordCon _ = Nothing

collectRecords :: GenericQ [RecordInfo]
collectRecords = everythingBut (<>) (([], False) `mkQ` getRecPatterns `extQ` getRecCons)

-- | Collect 'Name's into a map, indexed by the names' unique identifiers.
-- The 'Eq' instance of 'Name's makes use of their unique identifiers, hence
-- any 'Name' referring to the same entity is considered equal. In effect,
-- each individual list of names contains the binding occurence, along with
-- all the occurences at the use-sites (if there are any).
--
-- @UniqFM Name [Name]@ is morally the same as @Map Unique [Name]@.
-- Using 'UniqFM' gains us a bit of performance (in theory) since it
-- internally uses 'IntMap', and saves us rolling our own newtype wrapper over
-- 'Unique' (since 'Unique' doesn't have an 'Ord' instance, it can't be used
-- as 'Map' key as is). More information regarding 'UniqFM' can be found in
-- the GHC source.
collectNames :: GenericQ (UniqFM Name [Name])
collectNames = everything (plusUFM_C (<>)) (emptyUFM `mkQ` (\x -> unitUFM x [x]))

getRecCons :: LHsExpr (GhcPass 'Renamed) -> ([RecordInfo], Bool)
-- When we stumble upon an occurrence of HsExpanded, we only want to follow a
-- single branch. We do this here, by explicitly returning occurrences from
-- traversing the original branch, and returning True, which keeps syb from
-- implicitly continuing to traverse.
getRecCons (unLoc -> XExpr (HsExpanded _ expanded)) = (collectRecords expanded, True)
getRecCons e@(unLoc -> RecordCon _ _ flds)
  | isJust (rec_dotdot flds) = (mkRecInfo e, False)
  where
    mkRecInfo :: LHsExpr (GhcPass 'Renamed) -> [RecordInfo]
    mkRecInfo expr =
      [ RecordInfoCon realSpan' (unLoc expr) | RealSrcSpan realSpan' _ <- [ getLoc expr ]]
getRecCons _ = ([], False)

getRecPatterns :: LPat (GhcPass 'Renamed) -> ([RecordInfo], Bool)
getRecPatterns conPat@(conPatDetails . unLoc -> Just (RecCon flds))
  | isJust (rec_dotdot flds) = (mkRecInfo conPat, False)
  where
    mkRecInfo :: LPat (GhcPass 'Renamed) -> [RecordInfo]
    mkRecInfo pat =
      [ RecordInfoPat realSpan' (unLoc pat) | RealSrcSpan realSpan' _ <- [ getLoc pat ]]
getRecPatterns _ = ([], False)


