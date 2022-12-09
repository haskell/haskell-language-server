{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Ide.Plugin.ExplicitFields
  ( descriptor
  , Log
  ) where

import           Control.Lens                    ((^.))
import           Control.Monad.Extra             (maybeM)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Trans.Except      (ExceptT)
import           Data.Foldable                   (find)
import           Data.Generics                   (GenericQ, everything, extQ,
                                                  mkQ)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (isJust, listToMaybe,
                                                  maybeToList)
import           Data.Text                       (Text)
import           Development.IDE                 (IdeState, NormalizedFilePath,
                                                  Pretty (..), Recorder (..),
                                                  Rules, WithPriority (..),
                                                  realSpan, realSrcSpanToRange)
import           Development.IDE.Core.Rules      (runAction)
import           Development.IDE.Core.RuleTypes  (TcModuleResult (..),
                                                  TypeCheck (..))
import           Development.IDE.Core.Shake      (define, use)
import qualified Development.IDE.Core.Shake      as Shake
import           Development.IDE.GHC.Compat      (HsConDetails (RecCon),
                                                  HsRecFields (..), LPat,
                                                  Outputable, getLoc, unLoc)
import           Development.IDE.GHC.Compat.Core (Extension (NamedFieldPuns),
                                                  FieldOcc, GhcPass,
                                                  HsExpr (RecordCon, rcon_flds),
                                                  HsRecField', LHsExpr,
                                                  LocatedA, LocatedN, Name,
                                                  Pass (..), Pat (..),
                                                  RealSrcSpan, conPatDetails,
                                                  getUnique, hfbPun, hfbRHS,
                                                  hs_valds, mapConPatDetail,
                                                  mapLoc, nameSrcSpan,
                                                  nameUnique,
                                                  pattern RealSrcSpan)
import           Development.IDE.GHC.Compat.Util (Unique, nonDetCmpUnique)
import           Development.IDE.GHC.Util        (getExtensions,
                                                  printOutputable)
import           Development.IDE.Graph           (RuleResult)
import           Development.IDE.Graph.Classes   (Hashable, NFData (rnf))
import           Development.IDE.Spans.Pragmas   (NextPragmaInfo (..),
                                                  getFirstPragma,
                                                  insertNewPragma)
import           Development.IDE.Types.Logger    (Priority (..), cmapWithPrio,
                                                  logWith, (<+>))
import           GHC.Generics                    (Generic)
import           Ide.Plugin.RangeMap             (RangeMap)
import qualified Ide.Plugin.RangeMap             as RangeMap
import           Ide.PluginUtils                 (getNormalizedFilePath,
                                                  handleMaybeM, pluginResponse)
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
                                                  normalizedFilePathToUri,
                                                  type (|?) (InR))
import qualified Language.LSP.Types.Lens         as L


data Log
  = LogShake Shake.Log
  | LogCollectedRecords [RecordInfo]
  | LogRenderedRecords [RenderedRecordInfo]

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog
    LogCollectedRecords recs -> "Collected records with wildcards:" <+> pretty recs
    LogRenderedRecords recs -> "Rendered records:" <+> pretty recs

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
  , pluginRules = collectRecordsRule recorder
  }

codeActionProvider :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionProvider ideState pId (CodeActionParams _ _ docId range _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (docId ^. L.uri)
  pragma <- getFirstPragma pId ideState nfp
  CRR recMap (map unExt -> exts) <- collectRecords' ideState nfp
  let actions = map (mkCodeAction nfp exts pragma) (RangeMap.filterByRange range recMap)
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
        edits = mkTextEdit rec : maybeToList pragmaEdit

        mkTextEdit :: RenderedRecordInfo -> TextEdit
        mkTextEdit (RenderedRecordInfo ss r) = TextEdit (realSrcSpanToRange ss) r

        pragmaEdit :: Maybe TextEdit
        pragmaEdit = if NamedFieldPuns `elem` exts
                       then Nothing
                       else Just $ insertNewPragma pragma NamedFieldPuns

    mkWorkspaceEdit :: NormalizedFilePath -> [TextEdit] -> WorkspaceEdit
    mkWorkspaceEdit nfp edits = WorkspaceEdit changes Nothing Nothing
      where
        changes = Just $ HashMap.singleton (fromNormalizedUri (normalizedFilePathToUri nfp)) (List edits)

    mkCodeActionTitle :: [Extension] -> Text
    mkCodeActionTitle exts =
      if NamedFieldPuns `elem` exts
        then title
        else title <> " (needs extension: NamedFieldPuns)"
        where
          title = "Expand record wildcard"

collectRecordsRule :: Recorder (WithPriority Log) -> Rules ()
collectRecordsRule recorder = define (cmapWithPrio LogShake recorder) $ \CollectRecords nfp ->
  justOrFail "Unable to TypeCheck" (use TypeCheck nfp) $ \tmr -> do
    let exts = getEnabledExtensions tmr
        recs = getRecords tmr
    logWith recorder Debug (LogCollectedRecords recs)
    let names = getNames tmr
        renderedRecs = traverse (renderRecordInfo names) recs
        recMap = RangeMap.fromList (realSrcSpanToRange . renderedSrcSpan) <$> renderedRecs
    logWith recorder Debug (LogRenderedRecords (concat renderedRecs))
    pure ([], CRR <$> recMap <*> Just exts)

  where
    getEnabledExtensions :: TcModuleResult -> [GhcExtension]
    getEnabledExtensions = map GhcExtension . getExtensions . tmrParsed

    justOrFail :: MonadFail m => String -> m (Maybe a) -> (a -> m b) -> m b
    justOrFail = flip . maybeM . fail

getRecords :: TcModuleResult -> [RecordInfo]
getRecords (tmrRenamed -> (hs_valds -> valBinds,_,_,_)) =
  collectRecords valBinds

-- | Collects all 'Name's of a given source file, to be used
-- in the variable usage analysis.
getNames :: TcModuleResult -> Map UniqueKey [LocatedN Name]
getNames (tmrRenamed -> (group,_,_,_)) = collectNames group

newtype UniqueKey = UniqueKey Unique
                  deriving newtype Eq

getUniqueKey :: Name -> UniqueKey
getUniqueKey = UniqueKey . nameUnique

instance Ord UniqueKey where
  compare (UniqueKey u1) (UniqueKey u2) = getUnique u1 `nonDetCmpUnique` getUnique u2

data CollectRecords = CollectRecords
                    deriving (Eq, Show, Generic)

instance Hashable CollectRecords
instance NFData CollectRecords

data CollectRecordsResult = CRR
  { recordInfos       :: RangeMap RenderedRecordInfo
  , enabledExtensions :: [GhcExtension]
  }
  deriving (Generic)

instance NFData CollectRecordsResult

instance Show CollectRecordsResult where
  show _ = "<CollectRecordsResult>"

type instance RuleResult CollectRecords = CollectRecordsResult

-- `Extension` is wrapped so that we can provide an `NFData` instance
-- (without resorting to creating an orphan instance).
newtype GhcExtension = GhcExtension { unExt :: Extension }

instance NFData GhcExtension where
  rnf x = x `seq` ()

data RecordInfo
  = RecordInfoPat RealSrcSpan (Pat (GhcPass 'Renamed))
  | RecordInfoCon RealSrcSpan (HsExpr (GhcPass 'Renamed))

instance Pretty RecordInfo where
  pretty (RecordInfoPat ss p) = pretty (printOutputable ss) <> ":" <+> pretty (printOutputable p)
  pretty (RecordInfoCon ss e) = pretty (printOutputable ss) <> ":" <+> pretty (printOutputable e)

data RenderedRecordInfo = RenderedRecordInfo
  { renderedSrcSpan :: RealSrcSpan
  , renderedRecord  :: Text
  }
  deriving (Generic)

instance Pretty RenderedRecordInfo where
  pretty (RenderedRecordInfo ss r) = pretty (printOutputable ss) <> ":" <+> pretty r

instance NFData RenderedRecordInfo

renderRecordInfo :: Map UniqueKey [LocatedN Name] -> RecordInfo -> Maybe RenderedRecordInfo
renderRecordInfo names (RecordInfoPat ss pat) = RenderedRecordInfo ss <$> showRecordPat names pat
renderRecordInfo _ (RecordInfoCon ss expr) = RenderedRecordInfo ss <$> showRecordCon expr

-- | Checks if a 'Name' is referenced in a given list of names. The 'Eq'
-- instance of 'Name's makes use of their unique identifiers, hence any
-- to 'Name' referring to the same entity is considered equal. In order
-- to ensure that no false-positive is reported (in the case where the
-- 'name' itself is part of the given list), the inequality of source
-- locations is also checked.
referencedIn :: Name -> Map UniqueKey [LocatedN Name] -> Bool
referencedIn name names = maybe True hasNameRef $ Map.lookup (getUniqueKey name) names
  where
    hasNameRef :: [LocatedN Name] -> Bool
    hasNameRef = isJust . find (\n -> realSpan (getLoc n) /= realSpan (nameSrcSpan name))

-- Default to leaving the element in if somehow a name can't be extracted (i.e.
-- `getName` returns `Nothing`).
filterReferenced :: (a -> Maybe Name) -> Map UniqueKey [LocatedN Name] -> [a] -> [a]
filterReferenced getName names = filter (\x -> maybe True (`referencedIn` names) (getName x))

preprocessRecordPat
  :: p ~ GhcPass 'Renamed
  => Map UniqueKey [LocatedN Name]
  -> HsRecFields p (LPat p)
  -> HsRecFields p (LPat p)
preprocessRecordPat = preprocessRecord (getFieldName . unLoc)
  where
    getFieldName x = case unLoc (hfbRHS x) of
      VarPat _ x' -> Just $ unLoc x'
      _           -> Nothing

-- No need to check the name usage in the record construction case
preprocessRecordCon :: HsRecFields (GhcPass c) arg -> HsRecFields (GhcPass c) arg
preprocessRecordCon = preprocessRecord (const Nothing) Map.empty

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
  => (LocatedA (HsRecField' (FieldOcc p) arg) -> Maybe Name)
  -> Map UniqueKey [LocatedN Name]
  -> HsRecFields p arg
  -> HsRecFields p arg
preprocessRecord getName names flds = flds { rec_dotdot = Nothing , rec_flds = rec_flds' }
  where
    no_pun_count = maybe (length (rec_flds flds)) unLoc (rec_dotdot flds)
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

showRecordPat :: Outputable (Pat (GhcPass 'Renamed)) => Map UniqueKey [LocatedN Name] -> Pat (GhcPass 'Renamed) -> Maybe Text
showRecordPat names = fmap printOutputable . mapConPatDetail (\case
  RecCon flds -> Just $ RecCon (preprocessRecordPat names flds)
  _           -> Nothing)

showRecordCon :: Outputable (HsExpr (GhcPass c)) => HsExpr (GhcPass c) -> Maybe Text
showRecordCon expr@(RecordCon _ _ flds) =
  Just $ printOutputable $
    expr { rcon_flds = preprocessRecordCon flds }
showRecordCon _ = Nothing

collectRecords :: GenericQ [RecordInfo]
collectRecords = everything (<>) (maybeToList . (Nothing `mkQ` getRecPatterns `extQ` getRecCons))

collectNames :: GenericQ (Map UniqueKey [LocatedN Name])
collectNames = everything (Map.unionWith (<>)) (Map.empty `mkQ` (\x -> Map.singleton (getUniqueKey (unLoc x)) [x]))

getRecCons :: LHsExpr (GhcPass 'Renamed) -> Maybe RecordInfo
getRecCons e@(unLoc -> RecordCon _ _ flds)
  | isJust (rec_dotdot flds) = mkRecInfo e
  where
    mkRecInfo :: LHsExpr (GhcPass 'Renamed) -> Maybe RecordInfo
    mkRecInfo expr = listToMaybe
      [ RecordInfoCon realSpan' (unLoc expr) | RealSrcSpan realSpan' _ <- [ getLoc expr ]]
getRecCons _ = Nothing

getRecPatterns :: LPat (GhcPass 'Renamed) -> Maybe RecordInfo
getRecPatterns conPat@(conPatDetails . unLoc -> Just (RecCon flds))
  | isJust (rec_dotdot flds) = mkRecInfo conPat
  where
    mkRecInfo :: LPat (GhcPass 'Renamed) -> Maybe RecordInfo
    mkRecInfo pat = listToMaybe
      [ RecordInfoPat realSpan' (unLoc pat) | RealSrcSpan realSpan' _ <- [ getLoc pat ]]
getRecPatterns _ = Nothing

collectRecords' :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m CollectRecordsResult
collectRecords' ideState =
  handleMaybeM "Unable to TypeCheck"
    . liftIO
    . runAction "ExplicitFields" ideState
    . use CollectRecords

