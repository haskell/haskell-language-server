{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Core.Actions
( getAtPoint
, getDefinition
, getTypeDefinition
, getImplementationDefinition
, highlightAtPoint
, refsAtPoint
, workspaceSymbols
, lookupMod
) where

import           Control.Monad.Extra                  (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict                  as HM
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Tuple.Extra
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat           hiding (writeHieFile)
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


-- | Eventually this will lookup/generate URIs for files in dependencies, but not in the
-- project. Right now, this is just a stub.
lookupMod
  :: HieDbWriter -- ^ access the database
  -> FilePath -- ^ The `.hie` file we got from the database
  -> ModuleName
  -> Unit
  -> Bool -- ^ Is this file a boot file?
  -> MaybeT IdeAction Uri
lookupMod _dbchan _hie_f _mod _uid _boot = MaybeT $ pure Nothing


-- IMPORTANT NOTE : make sure all rules `useWithStaleFastMT`d by these have a "Persistent Stale" rule defined,
-- so we can quickly answer as soon as the IDE is opened
-- Even if we don't have persistent information on disk for these rules, the persistent rule
-- should just return an empty result
-- It is imperative that the result of the persistent rule succeed in such a case, or we will
-- block waiting for the rule to be properly computed.

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = runMaybeT $ do
  ide <- ask
  opts <- liftIO $ getIdeOptionsIO ide

  (hf, mapping) <- useWithStaleFastMT GetHieAst file
  env <- hscEnv . fst <$> useWithStaleFastMT GhcSession file
  dkMap <- lift $ maybe (DKMap mempty mempty) fst <$> runMaybeT (useWithStaleFastMT GetDocMap file)

  !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
  MaybeT $ liftIO $ fmap (first (toCurrentRange mapping =<<)) <$> AtPoint.atPoint opts hf dkMap env pos'

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
