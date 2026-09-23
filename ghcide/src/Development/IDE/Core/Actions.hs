{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Core.Actions
( getAtPoint
, getAtPointRange
, getDefinition
, getTypeDefinition
, getImplementationDefinition
, highlightAtPoint
, refsAtPoint
, workspaceSymbols
, lookupMod
) where

import           Control.Applicative                  ((<|>))
import           Control.Monad.Extra                  (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict                  as HM
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Tuple.Extra
import           Development.IDE.Core.LookupMod       (lookupMod)
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat           (DynFlags (..),
                                                       ms_hspp_opts)
import           Development.IDE.GHC.Error            (rangeToRealSrcSpan,
                                                       realSrcSpanToRange)
import           Development.IDE.GHC.Util             (printOutputableOneLine)
import           Development.IDE.Graph
import qualified Development.IDE.Spans.AtPoint        as AtPoint
import           Development.IDE.Types.HscEnvEq       (hscEnv)
import           Development.IDE.Types.Location
import           GHC.Iface.Ext.Types                  (Identifier)
import qualified HieDb
import           Language.LSP.Protocol.Types          (DocumentHighlight (..),
                                                       SymbolInformation (..),
                                                       normalizedFilePathToUri,
                                                       uriToNormalizedFilePath)

-- IMPORTANT NOTE : make sure all rules `useWithStaleFastMT`d by these have a "Persistent Stale" rule defined,
-- so we can quickly answer as soon as the IDE is opened
-- Even if we don't have persistent information on disk for these rules, the persistent rule
-- should just return an empty result
-- It is imperative that the result of the persistent rule succeed in such a case, or we will
-- block waiting for the rule to be properly computed.

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = getAtPointRange file (Range pos pos)

-- | Try to get hover text for the smallest expression enclosing the given
-- range, e.g. the current selection.
getAtPointRange :: NormalizedFilePath -> Range -> IdeAction (Maybe (Maybe Range, [T.Text]))
getAtPointRange file (Range start end) = runMaybeT $ do
  ide <- ask
  opts <- liftIO $ getIdeOptionsIO ide

  (hf, mapping) <- useWithStaleFastMT GetHieAst file
  shakeExtras <- lift askShake

  env <- hscEnv . fst <$> useWithStaleFastMT GhcSession file
  modSummary <- fst <$> useWithStaleFastMT GetModSummary file
  dkMap <- lift $ maybe (DKMap mempty mempty mempty) fst <$> runMaybeT (useWithStaleFastMT GetDocMap file)
  let enabledExtensions = extensionFlags (ms_hspp_opts (msrModSummary modSummary))

  !start' <- MaybeT (return $ fromCurrentPosition mapping start)
  !end' <- MaybeT (return $ fromCurrentPosition mapping end)

  mResult <- liftIO $ fmap (first (toCurrentRange mapping =<<)) <$>
    AtPoint.atPoint opts shakeExtras hf dkMap env (Range start' end') enabledExtensions

  -- The HieAST does not record the types of many intermediate expression
  -- nodes (e.g. applications), so a non-empty selection often lands on a
  -- node without any hover information. In that case, recover the type of
  -- the enclosing expression from the typechecked source.
  let emptyHover = maybe True (all T.null . snd) mResult
  if start == end || not emptyHover
    then hoistMaybe mResult
    else exprTypeAtRange file (Range start end) <|> hoistMaybe mResult

-- | Hover information with the type of the smallest expression enclosing the
-- given range, computed from the typechecked source.
exprTypeAtRange :: NormalizedFilePath -> Range -> MaybeT IdeAction (Maybe Range, [T.Text])
exprTypeAtRange file (Range start end) = do
  (tmr, mapping) <- useWithStaleFastMT TypeCheck file
  !start' <- MaybeT (return $ fromCurrentPosition mapping start)
  !end' <- MaybeT (return $ fromCurrentPosition mapping end)
  let sp = rangeToRealSrcSpan file (Range start' end')
  (exprSpan, exprTy) <- hoistMaybe $ AtPoint.exprTypeAtSpan sp (tmrTypechecked tmr)
  let typeSig = "\n```haskell\n_ :: " <> printOutputableOneLine exprTy <> "\n```\n"
  pure (toCurrentRange mapping (realSrcSpanToRange exprSpan), [typeSig])

-- | Converts locations in the source code to their current positions,
-- taking into account changes that may have occurred due to edits.
toCurrentLocation
  :: PositionMapping
  -> NormalizedFilePath
  -> Location
  -> IdeAction (Maybe Location)
toCurrentLocation mapping file (Location uri range) =
  -- The Location we are going to might be in a different
  -- file than the one we are calling gotoDefinition from.
  -- So we check that the location file matches the file
  -- we are in.
  if nUri == normalizedFilePathToUri file
  -- The Location matches the file, so use the PositionMapping
  -- we have.
  then pure $ Location uri <$> toCurrentRange mapping range
  -- The Location does not match the file, so get the correct
  -- PositionMapping and use that instead.
  else do
    otherLocationMapping <- fmap (fmap snd) $ runMaybeT $ do
      otherLocationFile <- MaybeT $ pure $ uriToNormalizedFilePath nUri
      useWithStaleFastMT GetHieAst otherLocationFile
    pure $ Location uri <$> (flip toCurrentRange range =<< otherLocationMapping)
  where
    nUri :: NormalizedUri
    nUri = toNormalizedUri uri

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [(Location, Identifier)])
getDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useWithStaleFastMT GetHieAst file
    (ImportMap imports, _) <- useWithStaleFastMT GetImportMap file
    !pos' <- MaybeT (pure $ fromCurrentPosition mapping pos)
    locationsWithIdentifier <- AtPoint.gotoDefinition withHieDb (lookupMod hiedbWriter) opts imports hf pos'
    mapMaybeM (\(location, identifier) -> do
      fixedLocation <- MaybeT $ toCurrentLocation mapping file location
      pure $ Just (fixedLocation, identifier)
      ) locationsWithIdentifier


getTypeDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [(Location, Identifier)])
getTypeDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useWithStaleFastMT GetHieAst file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    locationsWithIdentifier <- AtPoint.gotoTypeDefinition withHieDb (lookupMod hiedbWriter) opts hf pos'
    mapMaybeM (\(location, identifier) -> do
      fixedLocation <- MaybeT $ toCurrentLocation mapping file location
      pure $ Just (fixedLocation, identifier)
      ) locationsWithIdentifier

getImplementationDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [Location])
getImplementationDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useWithStaleFastMT GetHieAst file
    !pos' <- MaybeT (pure $ fromCurrentPosition mapping pos)
    locs <- AtPoint.gotoImplementation withHieDb (lookupMod hiedbWriter) opts hf pos'
    traverse (MaybeT . toCurrentLocation mapping file) locs

highlightAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe [DocumentHighlight])
highlightAtPoint file pos = runMaybeT $ do
    (HAR _ hf rf _ _,mapping) <- useWithStaleFastMT GetHieAst file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    let toCurrentHighlight (DocumentHighlight range t) = flip DocumentHighlight t <$> toCurrentRange mapping range
    mapMaybe toCurrentHighlight <$>AtPoint.documentHighlight hf rf pos'

-- Refs are not an IDE action, so it is OK to be slow and (more) accurate
refsAtPoint :: NormalizedFilePath -> Position -> Action [Location]
refsAtPoint file pos = do
    ShakeExtras{withHieDb} <- getShakeExtras
    fs <- HM.keys <$> getFilesOfInterestUntracked
    asts <- HM.fromList . mapMaybe sequence . zip fs <$> usesWithStale GetHieAst fs
    AtPoint.referencesAtPoint withHieDb file pos (AtPoint.FOIReferences asts)

workspaceSymbols :: T.Text -> IdeAction (Maybe [SymbolInformation])
workspaceSymbols query = runMaybeT $ do
  ShakeExtras{withHieDb} <- ask
  res <- liftIO $ withHieDb (\hieDb -> HieDb.searchDef hieDb $ T.unpack query)
  pure $ mapMaybe AtPoint.defRowToSymbolInfo res
