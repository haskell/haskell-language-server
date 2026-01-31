{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.ExplicitFields
  ( descriptor
  , Log
  ) where

import           Control.Arrow                        ((&&&))
import           Control.Lens                         ((&), (?~), (^.))
import           Control.Monad                        (replicateM)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           (ToJSON (toJSON))
import           Data.Function                        (on)
import           Data.Generics                        (GenericQ, everything,
                                                       everythingBut, extQ, mkQ)
import qualified Data.IntMap.Strict                   as IntMap
import           Data.List                            (find, intersperse,
                                                       sortOn)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe, isJust,
                                                       mapMaybe, maybeToList)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Unique                          (hashUnique, newUnique)
import           Development.IDE                      (IdeState,
                                                       Location (Location),
                                                       Pretty (..),
                                                       Range (Range, _end, _start),
                                                       Recorder (..), Rules,
                                                       WithPriority (..),
                                                       defineNoDiagnostics,
                                                       getDefinition, hsep,
                                                       printName,
                                                       realSrcSpanToRange,
                                                       shakeExtras,
                                                       srcSpanToLocation,
                                                       srcSpanToRange, viaShow)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       toCurrentPosition,
                                                       toCurrentRange)
import           Development.IDE.Core.RuleTypes       (TcModuleResult (..),
                                                       TypeCheck (..))
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat           (FieldLabel (flSelector),
                                                       FieldOcc (FieldOcc),
                                                       GenLocated (L), GhcPass,
                                                       GhcTc,
                                                       HasSrcSpan (getLoc),
                                                       HsConDetails (RecCon),
                                                       HsExpr (HsApp, HsVar, XExpr),
                                                       HsFieldBind (hfbLHS),
                                                       HsRecFields (..),
                                                       HsWrap (HsWrap), LPat,
                                                       Located,
                                                       NamedThing (getName),
                                                       Outputable,
                                                       TcGblEnv (tcg_binds),
                                                       Var (varName),
                                                       XXExprGhcTc (..),
                                                       conLikeFieldLabels,
                                                       nameSrcSpan,
                                                       pprNameUnqualified,
                                                       recDotDot, unLoc)
import           Development.IDE.GHC.Compat.Core      (Extension (NamedFieldPuns),
                                                       HsExpr (RecordCon, rcon_flds),
                                                       HsRecField, LHsExpr,
                                                       LocatedA, Name, Pat (..),
                                                       RealSrcSpan, UniqFM,
                                                       conPatDetails, emptyUFM,
                                                       hfbPun, hfbRHS,
                                                       lookupUFM,
                                                       mapConPatDetail, mapLoc,
                                                       pattern RealSrcSpan,
                                                       plusUFM_C, unitUFM)
import           Development.IDE.GHC.Util             (getExtensions,
                                                       printOutputable,
                                                       stripOccNamePrefix)
import           Development.IDE.Graph                (RuleResult)
import           Development.IDE.Graph.Classes        (Hashable, NFData)
import           Development.IDE.Spans.Pragmas        (NextPragmaInfo (..),
                                                       getFirstPragma,
                                                       insertNewPragma)
import           GHC.Generics                         (Generic)
import           GHC.Iface.Ext.Types                  (Identifier)
import           Ide.Logger                           (Priority (..),
                                                       cmapWithPrio, logWith,
                                                       (<+>))
import           Ide.Plugin.Error                     (PluginError (PluginInternalError, PluginStaleResolve),
                                                       getNormalizedFilePathE,
                                                       handleMaybe)
import           Ide.Plugin.RangeMap                  (RangeMap)
import qualified Ide.Plugin.RangeMap                  as RangeMap
import           Ide.Plugin.Resolve                   (mkCodeActionWithResolveAndCommand)
import           Ide.PluginUtils                      (subRange)
import           Ide.Types                            (PluginDescriptor (..),
                                                       PluginId (..),
                                                       PluginMethodHandler,
                                                       ResolveFunction,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        (Method (..),
                                                       SMethod (SMethod_TextDocumentInlayHint))
import           Language.LSP.Protocol.Types          (CodeAction (..),
                                                       CodeActionKind (CodeActionKind_RefactorRewrite),
                                                       CodeActionParams (CodeActionParams),
                                                       Command, InlayHint (..),
                                                       InlayHintLabelPart (InlayHintLabelPart),
                                                       InlayHintParams (InlayHintParams, _range, _textDocument),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       TextEdit (TextEdit),
                                                       WorkspaceEdit (WorkspaceEdit),
                                                       type (|?) (InL, InR))

#if __GLASGOW_HASKELL__ < 910
import           Development.IDE.GHC.Compat           (HsExpansion (HsExpanded))
#endif

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
      ihDotdotHandler = mkPluginHandler SMethod_TextDocumentInlayHint (inlayHintDotdotProvider recorder)
      ihPosRecHandler = mkPluginHandler SMethod_TextDocumentInlayHint (inlayHintPosRecProvider recorder)
  in (defaultPluginDescriptor plId "Provides a code action to make record wildcards explicit")
  { pluginHandlers = caHandlers <> ihDotdotHandler <> ihPosRecHandler
  , pluginCommands = carCommands
  , pluginRules = collectRecordsRule recorder *> collectNamesRule
  }

data RecordConversionType
  = RecordWildcardExpansion
  | RecordTraditionalSyntaxConversion

data RecordConversion =
  RecordConversion
    Int -- ^ uid
    RecordConversionType

-- | Given a record, determine whether it is a case of wildcard expansion
-- or a conversion to the traditional record syntax.
getConversionType :: RecordInfo -> Maybe RecordConversionType
getConversionType = \case
  -- Only fully saturated constructor applications can be converted to
  -- the record syntax through the code action
  RecordInfoApp _ (RecordAppExpr Unsaturated _ _) -> Nothing
  RecordInfoApp {} -> Just RecordTraditionalSyntaxConversion
  _ -> Just RecordWildcardExpansion

codeActionProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionProvider ideState _ (CodeActionParams _ _ docId range _) = do
  nfp <- getNormalizedFilePathE (docId ^. L.uri)
  CRR {crCodeActions, crCodeActionResolve, enabledExtensions} <- runActionE "ExplicitFields.CollectRecords" ideState $ useE CollectRecords nfp
  -- All we need to build a code action is the list of extensions, and a int to
  -- allow us to resolve it later.
  let recordsWithUid = [ (RecordConversion uid conversionType, record)
                      | uid <- RangeMap.filterByRange range crCodeActions
                      , Just record <- [IntMap.lookup uid crCodeActionResolve]
                      , Just conversionType <- [getConversionType record]
                      ]
      recordsOnly = map snd recordsWithUid
      sortedRecords = sortOn (recordDepth recordsOnly . snd) recordsWithUid
  pure $ InL $ case sortedRecords of
    (top : _) -> [mkCodeAction enabledExtensions (fst top)]
    []        -> []
  where
    mkCodeAction :: [Extension] -> RecordConversion -> Command |? CodeAction
    mkCodeAction exts (RecordConversion uid conversionType) = InR CodeAction
      { _title = mkTitle exts conversionType
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
  CRR {crCodeActionResolve, nameMap, enabledExtensions} <- runActionE "ExplicitFields.CollectRecords" ideState $ useE CollectRecords nfp
  -- If we are unable to find the unique id in our IntMap of records, it means
  -- that this resolve is stale.
  record <- handleMaybe PluginStaleResolve $ IntMap.lookup uid crCodeActionResolve
  -- We should never fail to render
  rendered <- handleMaybe (PluginInternalError "Failed to render") $ renderRecordInfoAsTextEdit nameMap record
  let shouldInsertNamedFieldPuns (RecordInfoApp _ _) = False
      shouldInsertNamedFieldPuns _                   = True
      whenMaybe True x  = x
      whenMaybe False _ = Nothing
      edits = [rendered]
              <> maybeToList (whenMaybe (shouldInsertNamedFieldPuns record) (pragmaEdit enabledExtensions pragma))
  pure $ ca & L.edit ?~ mkWorkspaceEdit edits
  where
    mkWorkspaceEdit ::[TextEdit] -> WorkspaceEdit
    mkWorkspaceEdit edits = WorkspaceEdit (Just $ Map.singleton uri edits) Nothing Nothing

inlayHintDotdotProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentInlayHint
inlayHintDotdotProvider _ state pId InlayHintParams {_textDocument = TextDocumentIdentifier uri, _range = visibleRange} = do
  nfp <- getNormalizedFilePathE uri
  pragma <- getFirstPragma pId state nfp
  runIdeActionE "ExplicitFields.CollectRecords" (shakeExtras state) $ do
    (crr@CRR {crCodeActions, crCodeActionResolve}, pm) <- useWithStaleFastE CollectRecords nfp
    let -- Get all records with dotdot in current nfp
        records = [ record
                  | Just range <- [toCurrentRange pm visibleRange]
                  , uid <- RangeMap.elementsInRange range crCodeActions
                  , Just record <- [IntMap.lookup uid crCodeActionResolve] ]
        -- Get the definition of each dotdot of record
        locations = [ fmap (,record) (getDefinition nfp pos)
                    | record <- records
                    , pos <- maybeToList $ fmap _start $ recordInfoToDotDotRange record ]
    defnLocsList <- lift $ sequence locations
    pure $ InL $ mapMaybe (mkInlayHint crr pragma pm) defnLocsList
   where
     mkInlayHint :: CollectRecordsResult -> NextPragmaInfo -> PositionMapping -> (Maybe [(Location, Identifier)], RecordInfo) -> Maybe InlayHint
     mkInlayHint CRR {enabledExtensions, nameMap} pragma pm (defnLocs, record) =
       let range = recordInfoToDotDotRange record
           textEdits = maybeToList (renderRecordInfoAsTextEdit nameMap record)
                    <> maybeToList (pragmaEdit enabledExtensions pragma)
           names = renderRecordInfoAsDotdotLabelName record
       in do
         currentEnd <- range >>= toCurrentPosition pm . _end
         names' <- names
         defnLocs' <- defnLocs
         let excludeDotDot (Location _ (Range _ end)) = end /= currentEnd
             -- find location from dotdot definitions that name equal to label name
             findLocation name locations =
               let -- filter locations not within dotdot range
                   filteredLocations = filter (excludeDotDot . fst) locations
                   -- checks if 'a' is equal to 'Name' if the 'Either' is 'Right a', otherwise return 'False'
                   nameEq = either (const False) ((==) name)
                in fmap fst $ find (nameEq . snd) filteredLocations
             valueWithLoc = [ (stripOccNamePrefix $ T.pack $ printName name, findLocation name defnLocs') | name <- names' ]
             -- use `, ` to separate labels with definition location
             label = intersperse (mkInlayHintLabelPart (", ", Nothing)) $ fmap mkInlayHintLabelPart valueWithLoc
         pure $ InlayHint { _position = currentEnd -- at the end of dotdot
                          , _label = InR label
                          , _kind = Nothing -- neither a type nor a parameter
                          , _textEdits = Just textEdits -- same as CodeAction
                          , _tooltip = Just $ InL (mkTitle enabledExtensions RecordWildcardExpansion) -- same as CodeAction
                          , _paddingLeft = Just True -- padding after dotdot
                          , _paddingRight = Nothing
                          , _data_ = Nothing
                          }
     mkInlayHintLabelPart (value, loc) = InlayHintLabelPart value Nothing loc Nothing


inlayHintPosRecProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentInlayHint
inlayHintPosRecProvider _ state _pId InlayHintParams {_textDocument = TextDocumentIdentifier uri, _range = visibleRange} = do
  nfp <- getNormalizedFilePathE uri
  runIdeActionE "ExplicitFields.CollectRecords" (shakeExtras state) $ do
    (CRR {crCodeActions, nameMap, crCodeActionResolve}, pm) <- useWithStaleFastE CollectRecords nfp
    let records = [ record
                  | Just range <- [toCurrentRange pm visibleRange]
                  , uid <- RangeMap.elementsInRange range crCodeActions
                  , Just record <- [IntMap.lookup uid crCodeActionResolve] ]
    pure $ InL (concatMap (mkInlayHints nameMap pm) records)
   where
     mkInlayHints :: UniqFM Name [Name] -> PositionMapping -> RecordInfo -> [InlayHint]
     mkInlayHints nameMap pm record@(RecordInfoApp _ (RecordAppExpr sat _ fla)) =
       -- Only create inlay hints for fully saturated constructors
       case sat of
         Saturated -> let textEdits = renderRecordInfoAsTextEdit nameMap record
                      in mapMaybe (mkInlayHint textEdits pm) fla
         Unsaturated -> []
     mkInlayHints _ _ _ = []

     mkInlayHint :: Maybe TextEdit -> PositionMapping -> (Located FieldLabel, HsExpr GhcTc) -> Maybe InlayHint
     mkInlayHint te pm (label, _) =
       let (name, loc) = ((flSelector . unLoc) &&& (srcSpanToLocation . getLoc)) label
           fieldDefLoc = srcSpanToLocation (nameSrcSpan name)
       in do
         (Location _ recRange) <- loc
         currentStart <- toCurrentPosition pm (_start recRange)
         pure InlayHint { _position = currentStart
                        , _label = InR $ pure (mkInlayHintLabelPart name fieldDefLoc)
                        , _kind = Nothing -- neither a type nor a parameter
                        , _textEdits = Just (maybeToList te) -- same as CodeAction
                        , _tooltip = Just $ InL (mkTitle [] RecordTraditionalSyntaxConversion) -- same as CodeAction
                        , _paddingLeft = Nothing
                        , _paddingRight = Nothing
                        , _data_ = Nothing
                        }

     mkInlayHintLabelPart name loc = InlayHintLabelPart (printFieldName (pprNameUnqualified name) <> "=") Nothing loc Nothing

mkTitle :: [Extension] -> RecordConversionType -> Text
mkTitle exts = \case
  RecordWildcardExpansion ->
    "Expand record wildcard"
      <> if NamedFieldPuns `elem` exts
         then mempty
         else " (needs extension: NamedFieldPuns)"
  RecordTraditionalSyntaxConversion ->
    "Convert to traditional record syntax"

-- Calculate the nesting depth of a record by counting how many other records
-- contain it. Used to prioritize more deeply nested records in code actions.
recordDepth :: [RecordInfo] -> RecordInfo -> Int
recordDepth allRecords record =
  let isSubrangeOf = subRange `on` recordInfoToRange
  in length $ filter (`isSubrangeOf` record) allRecords

pragmaEdit :: [Extension] -> NextPragmaInfo -> Maybe TextEdit
pragmaEdit exts pragma = if NamedFieldPuns `elem` exts
                  then Nothing
                  else Just $ insertNewPragma pragma NamedFieldPuns


collectRecordsRule :: Recorder (WithPriority Log) -> Rules ()
collectRecordsRule recorder =
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \CollectRecords nfp -> runMaybeT $ do
  tmr <- useMT TypeCheck nfp
  (CNR nameMap) <- useMT CollectNames nfp
  let recs = getRecords tmr
  logWith recorder Debug (LogCollectedRecords recs)
  -- We want a list of unique numbers to link our the original code action we
  -- give out, with the actual record info that we resolve it to.
  uniques <- liftIO $ replicateM (length recs) (hashUnique <$> newUnique)
  let recsWithUniques = zip uniques recs
      -- For creating the code actions, a RangeMap of unique ids
      crCodeActions = RangeMap.fromList' (toRangeAndUnique <$> recsWithUniques)
      -- For resolving the code actions, a IntMap which links the unique id to
      -- the relevant record info.
      crCodeActionResolve = IntMap.fromList recsWithUniques
      enabledExtensions = getEnabledExtensions tmr
  pure CRR {crCodeActions, crCodeActionResolve, nameMap, enabledExtensions}
  where
    getEnabledExtensions :: TcModuleResult -> [Extension]
    getEnabledExtensions = getExtensions . tmrParsed
    toRangeAndUnique (uid, recordInfo) = (recordInfoToRange recordInfo, uid)

getRecords :: TcModuleResult -> [RecordInfo]
getRecords (tcg_binds . tmrTypechecked -> valBinds) = collectRecords valBinds

collectNamesRule :: Rules ()
collectNamesRule = defineNoDiagnostics mempty $ \CollectNames nfp -> runMaybeT $ do
  tmr <- useMT TypeCheck nfp
  pure (CNR (getNames tmr))

-- | Collects all 'Name's of a given source file, to be used
-- in the variable usage analysis.
getNames :: TcModuleResult -> UniqFM Name [Name]
#if __GLASGOW_HASKELL__ < 910
getNames (tmrRenamed -> (group,_,_,_))   = collectNames group
#else
getNames (tmrRenamed -> (group,_,_,_,_)) = collectNames group
#endif

data CollectRecords = CollectRecords
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecords
instance NFData CollectRecords

-- |The result of our map, this record includes everything we need to provide
-- code actions and resolve them later
data CollectRecordsResult = CRR
  { -- |For providing the code action we need the unique id (Int) in a RangeMap
    crCodeActions       :: RangeMap Int
    -- |For resolving the code action we need to link the unique id we
    -- previously gave out with the record info that we use to make the edit
    -- with.
  , crCodeActionResolve :: IntMap.IntMap RecordInfo
    -- |The name map allows us to prune unused record fields (some of the time)
  , nameMap             :: UniqFM Name [Name]
    -- |We need to make sure NamedFieldPuns is enabled, if it's not we need to
    -- add that to the text edit. (In addition we use it in creating the code
    -- action title)
  , enabledExtensions   :: [Extension]
  }
  deriving (Generic)

instance NFData CollectRecordsResult
instance NFData RecordInfo
instance NFData RecordAppExpr

instance Show CollectRecordsResult where
  show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecords = CollectRecordsResult

data CollectNames = CollectNames
                  deriving (Eq, Show, Generic)

instance Hashable CollectNames
instance NFData CollectNames

data CollectNamesResult = CNR (UniqFM Name [Name])
  deriving (Generic)

instance NFData CollectNamesResult

instance Show CollectNamesResult where
  show _ = "<CollectNamesResult>"

type instance RuleResult CollectNames = CollectNamesResult

data Saturated = Saturated | Unsaturated
  deriving (Generic)

instance NFData Saturated

data RecordAppExpr
  = RecordAppExpr
      Saturated  -- ^ Is the DataCon application fully saturated or partially applied?
      (LHsExpr GhcTc)
      [(Located FieldLabel, HsExpr GhcTc)]
  deriving (Generic)

data RecordInfo
  = RecordInfoPat RealSrcSpan (Pat GhcTc)
  | RecordInfoCon RealSrcSpan (HsExpr GhcTc)
  | RecordInfoApp RealSrcSpan RecordAppExpr
  deriving (Generic)

instance Pretty RecordInfo where
  pretty (RecordInfoPat ss p) = pretty (printFieldName ss) <> ":" <+> pretty (printOutputable p)
  pretty (RecordInfoCon ss e) = pretty (printFieldName ss) <> ":" <+> pretty (printOutputable e)
  pretty (RecordInfoApp ss (RecordAppExpr _ _ fla))
    = pretty (printFieldName ss) <> ":" <+> hsep (map (pretty . printOutputable) fla)

recordInfoToRange :: RecordInfo -> Range
recordInfoToRange (RecordInfoPat ss _) = realSrcSpanToRange ss
recordInfoToRange (RecordInfoCon ss _) = realSrcSpanToRange ss
recordInfoToRange (RecordInfoApp ss _) = realSrcSpanToRange ss

recordInfoToDotDotRange :: RecordInfo -> Maybe Range
recordInfoToDotDotRange (RecordInfoPat _ (ConPat _ _ (RecCon flds))) = srcSpanToRange . getLoc =<< rec_dotdot flds
recordInfoToDotDotRange (RecordInfoCon _ (RecordCon _ _ flds)) = srcSpanToRange . getLoc =<< rec_dotdot flds
recordInfoToDotDotRange _ = Nothing

renderRecordInfoAsTextEdit :: UniqFM Name [Name] -> RecordInfo -> Maybe TextEdit
renderRecordInfoAsTextEdit names (RecordInfoPat ss pat) = TextEdit (realSrcSpanToRange ss) <$> showRecordPat names pat
renderRecordInfoAsTextEdit _ (RecordInfoCon ss expr) = TextEdit (realSrcSpanToRange ss) <$> showRecordCon expr
renderRecordInfoAsTextEdit _ (RecordInfoApp ss appExpr) = TextEdit (realSrcSpanToRange ss) <$> showRecordApp appExpr

renderRecordInfoAsDotdotLabelName :: RecordInfo -> Maybe [Name]
renderRecordInfoAsDotdotLabelName (RecordInfoPat _ pat)  = showRecordPatFlds pat
renderRecordInfoAsDotdotLabelName (RecordInfoCon _ expr) = showRecordConFlds expr
renderRecordInfoAsDotdotLabelName _                      = Nothing


-- | Checks if a 'Name' is referenced in the given map of names. The
-- 'hasNonBindingOcc' check is necessary in order to make sure that only the
-- references at the use-sites are considered (i.e. the binding occurence
-- is excluded). For more information regarding the structure of the map,
-- refer to the documentation of 'collectNames'.
referencedIn :: Name -> UniqFM Name [Name] -> Bool
referencedIn name names = maybe True hasNonBindingOcc $ lookupUFM names name
  where
    hasNonBindingOcc :: [Name] -> Bool
    hasNonBindingOcc = (> 1) . length

-- Default to leaving the element in if somehow a name can't be extracted (i.e.
-- `getName` returns `Nothing`).
filterReferenced :: (a -> Maybe Name) -> UniqFM Name [Name] -> [a] -> [a]
filterReferenced getName names = filter (\x -> maybe True (`referencedIn` names) (getName x))


preprocessRecordPat
  :: p ~ GhcTc
  => UniqFM Name [Name]
  -> HsRecFields p (LPat p)
  -> HsRecFields p (LPat p)
preprocessRecordPat = preprocessRecord (fmap varName . getFieldName . unLoc)
  where getFieldName x = case unLoc (hfbRHS x) of
          VarPat _ x' -> Just $ unLoc x'
          _           -> Nothing

-- No need to check the name usage in the record construction case
preprocessRecordCon :: HsRecFields (GhcPass c) arg -> HsRecFields (GhcPass c) arg
preprocessRecordCon = preprocessRecord (const Nothing) emptyUFM

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
  -> UniqFM Name [Name]
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

processRecordFlds
  :: p ~ GhcPass c
  => HsRecFields p arg
  -> HsRecFields p arg
processRecordFlds flds = flds { rec_dotdot = Nothing , rec_flds = puns' }
  where
    no_pun_count = fromMaybe (length (rec_flds flds)) (recDotDot flds)
    -- Field binds of the explicit form (e.g. `{ a = a' }`) should be drop
    puns = drop no_pun_count (rec_flds flds)
    -- `hsRecPun` is set to `True` in order to pretty-print the fields as field
    -- puns (since there is similar mechanism in the `Outputable` instance as
    -- explained above).
    puns' = map (mapLoc (\fld -> fld { hfbPun = True })) puns


showRecordPat :: Outputable (Pat GhcTc) => UniqFM Name [Name] -> Pat GhcTc -> Maybe Text
showRecordPat names = fmap printFieldName . mapConPatDetail (\case
  RecCon flds -> Just $ RecCon (preprocessRecordPat names flds)
  _           -> Nothing)

showRecordPatFlds :: Pat GhcTc -> Maybe [Name]
showRecordPatFlds (ConPat _ _ args) = do
  fields <- processRecCon args
  names <- mapM getFieldName (rec_flds fields)
  pure names
  where
    processRecCon (RecCon flds) = Just $ processRecordFlds flds
    processRecCon _             = Nothing
#if __GLASGOW_HASKELL__ < 911
    getOccName (FieldOcc x _) = Just $ getName x
#else
    getOccName (FieldOcc _ x) = Just $ getName (unLoc x)
#endif
    getOccName _              = Nothing
    getFieldName = getOccName . unLoc . hfbLHS . unLoc
showRecordPatFlds _ = Nothing

showRecordCon :: Outputable (HsExpr (GhcPass c)) => HsExpr (GhcPass c) -> Maybe Text
showRecordCon expr@(RecordCon _ _ flds) =
  Just $ printOutputable $
    expr { rcon_flds = preprocessRecordCon flds }
showRecordCon _ = Nothing

showRecordConFlds :: p ~ GhcTc => HsExpr p -> Maybe [Name]
showRecordConFlds (RecordCon _ _ flds) =
  mapM getFieldName (rec_flds $ processRecordFlds flds)
  where
    getVarName (HsVar _ lidp) = Just $ getName lidp
    getVarName _              = Nothing
    getFieldName = getVarName . unLoc . hfbRHS . unLoc
showRecordConFlds _ = Nothing

showRecordApp :: RecordAppExpr -> Maybe Text
showRecordApp (RecordAppExpr _ recConstr fla)
  = Just $ printOutputable recConstr <>  " { "
         <> T.intercalate ", " (showFieldWithArg <$> fla)
         <> " }"
  where showFieldWithArg (field, arg) = printFieldName field <> " = " <> printOutputable arg

collectRecords :: GenericQ [RecordInfo]
collectRecords = everythingBut (<>) (([], False) `mkQ` getRecPatterns `extQ` getRecCons)

-- | Collect 'Name's into a map, indexed by the names' unique identifiers.
-- The 'Eq' instance of 'Name's makes use of their unique identifiers, hence
-- any 'Name' referring to the same entity is considered equal. In effect,
-- each individual list of names contains the binding occurrence, along with
-- all the occurrences at the use-sites (if there are any).
--
-- @UniqFM Name [Name]@ is morally the same as @Map Unique [Name]@.
-- Using 'UniqFM' gains us a bit of performance (in theory) since it
-- internally uses 'IntMap'. More information regarding 'UniqFM' can be found in
-- the GHC source.
collectNames :: GenericQ (UniqFM Name [Name])
collectNames = everything (plusUFM_C (<>)) (emptyUFM `mkQ` (\x -> unitUFM x [x]))

getRecCons :: LHsExpr GhcTc -> ([RecordInfo], Bool)
-- When we stumble upon an occurrence of HsExpanded, we only want to follow a
-- single branch. We do this here, by explicitly returning occurrences from
-- traversing the original branch, and returning True, which keeps syb from
-- implicitly continuing to traverse. In addition, we have to return a list,
-- because there is a possibility that there were be more than one result per
-- branch

#if __GLASGOW_HASKELL__ >= 910
getRecCons (unLoc -> XExpr (ExpandedThingTc a _)) = (collectRecords a, False)
#else
getRecCons (unLoc -> XExpr (ExpansionExpr (HsExpanded _ a))) = (collectRecords a, True)
#endif
getRecCons e@(unLoc -> RecordCon _ _ flds)
  | isJust (rec_dotdot flds) = (mkRecInfo e, False)
  where
    mkRecInfo :: LHsExpr GhcTc -> [RecordInfo]
    mkRecInfo expr =
      [ RecordInfoCon realSpan' (unLoc expr) | RealSrcSpan realSpan' _ <- [ getLoc expr ]]
getRecCons expr@(unLoc -> app@(HsApp _ _ _)) =
  let fieldss = maybeToList $ getFields app []
      recInfo = concatMap mkRecInfo fieldss
  -- Search control for positional constructors.
  -- True stops further (nested) searching; False allows recursive search.
  -- Currently hardcoded to False to enable nested positional searches.
  -- Use `in (recInfo, not (null recInfo))` to disable nested searching.
  in (recInfo, False)
  where
    mkRecInfo :: RecordAppExpr -> [RecordInfo]
    mkRecInfo appExpr =
      [ RecordInfoApp realSpan' appExpr | RealSrcSpan realSpan' _ <- [ getLoc expr ] ]

    getFields :: HsExpr GhcTc -> [LHsExpr GhcTc] -> Maybe RecordAppExpr
    getFields (HsApp _ constr@(unLoc -> expr) arg) args
      | not (null fls) = Just $
      -- Code action is only valid if the constructor application is fully
      -- saturated, but we still want to display the inlay hints for partially
      -- applied constructors
        RecordAppExpr
          (if length fls <= length args + 1 then Saturated else Unsaturated)
          constr
          labelWithArgs
      where fls = getExprFields expr
            labelWithArgs = zipWith mkLabelWithArg fls (arg : args)
            mkLabelWithArg label arg = (L (getLoc arg) label, unLoc arg)
    getFields (HsApp _ constr arg) args = getFields (unLoc constr) (arg : args)
    getFields _ _ = Nothing

    getExprFields :: HsExpr GhcTc -> [FieldLabel]
    getExprFields (XExpr (ConLikeTc (conLikeFieldLabels -> fls) _ _)) = fls
#if __GLASGOW_HASKELL__ >= 911
    getExprFields (XExpr (WrapExpr _ expr)) = getExprFields expr
#else
    getExprFields (XExpr (WrapExpr (HsWrap _ expr))) = getExprFields expr
#endif
    getExprFields _ = []
getRecCons _ = ([], False)

getRecPatterns :: LPat GhcTc -> ([RecordInfo], Bool)
getRecPatterns conPat@(conPatDetails . unLoc -> Just (RecCon flds))
  | isJust (rec_dotdot flds) = (mkRecInfo conPat, False)
  where
    mkRecInfo :: LPat GhcTc -> [RecordInfo]
    mkRecInfo pat =
      [ RecordInfoPat realSpan' (unLoc pat) | RealSrcSpan realSpan' _ <- [ getLoc pat ]]
getRecPatterns _ = ([], False)

printFieldName :: Outputable a => a -> Text
printFieldName = stripOccNamePrefix . printOutputable

