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
import           Development.IDE.Core.LookupMod       (lookupMod)
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.Graph
import qualified Development.IDE.Spans.AtPoint        as AtPoint
import           Development.IDE.Types.HscEnvEq       (hscEnv)
import           Development.IDE.Types.Location
import           GHC.Iface.Ext.Types                  (Identifier)
import qualified HieDb
import           Language.LSP.Protocol.Types          (DocumentHighlight (..),
                                                       SymbolInformation (..))

-- IMPORTANT NOTE : make sure all rules `useWithStaleFastMT`d by these have a "Persistent Stale" rule defined,
-- so we can quickly answer as soon as the IDE is opened
-- Even if we don't have persistent information on disk for these rules, the persistent rule
-- should just return an empty result
-- It is imperative that the result of the persistent rule succeed in such a case, or we will
-- block waiting for the rule to be properly computed.

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedUri -> Position -> IdeAction (Maybe (Maybe Range, [T.Text]))
getAtPoint uri pos = runMaybeT $ do
  ide <- ask
  opts <- liftIO $ getIdeOptionsIO ide

  (hf, mapping) <- useWithStaleFastMT GetHieAst uri
  shakeExtras <- lift askShake

  env <- hscEnv . fst <$> useWithStaleFastMT GhcSession uri
  dkMap <- lift $ maybe (DKMap mempty mempty mempty) fst <$> runMaybeT (useWithStaleFastMT GetDocMap uri)

  !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)

  MaybeT $ liftIO $ fmap (first (toCurrentRange mapping =<<)) <$>
    AtPoint.atPoint opts shakeExtras hf dkMap env pos'

-- | Converts locations in the source code to their current positions,
-- taking into account changes that may have occurred due to edits.
toCurrentLocation
  :: PositionMapping
  -> NormalizedUri
  -> Location
  -> IdeAction (Maybe Location)
toCurrentLocation mapping uri (Location locUri locRange) =
  -- The Location we are going to might be in a different
  -- file than the one we are calling gotoDefinition from.
  -- So we check that the location file matches the file
  -- we are in.
  if nUri == uri
  -- The Location matches the file, so use the PositionMapping
  -- we have.
  then pure $ Location locUri <$> toCurrentRange mapping locRange
  -- The Location does not match the file, so get the correct
  -- PositionMapping and use that instead.
  else do
    otherLocationMapping <- fmap (fmap snd) $ runMaybeT $ do
      useWithStaleFastMT GetHieAst nUri
    pure $ Location locUri <$> (flip toCurrentRange locRange =<< otherLocationMapping)
  where
    nUri :: NormalizedUri
    nUri = toNormalizedUri locUri

-- | Goto Definition.
getDefinition :: NormalizedUri -> Position -> IdeAction (Maybe [(Location, Identifier)])
getDefinition uri pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useWithStaleFastMT GetHieAst uri
    (ImportMap imports, _) <- useWithStaleFastMT GetImportMap uri
    !pos' <- MaybeT (pure $ fromCurrentPosition mapping pos)
    locationsWithIdentifier <- AtPoint.gotoDefinition withHieDb (lookupMod hiedbWriter) opts imports hf pos'
    mapMaybeM (\(location, identifier) -> do
      fixedLocation <- MaybeT $ toCurrentLocation mapping uri location
      pure $ Just (fixedLocation, identifier)
      ) locationsWithIdentifier


getTypeDefinition :: NormalizedUri -> Position -> IdeAction (Maybe [(Location, Identifier)])
getTypeDefinition uri pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useWithStaleFastMT GetHieAst uri
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    locationsWithIdentifier <- AtPoint.gotoTypeDefinition withHieDb (lookupMod hiedbWriter) opts hf pos'
    mapMaybeM (\(location, identifier) -> do
      fixedLocation <- MaybeT $ toCurrentLocation mapping uri location
      pure $ Just (fixedLocation, identifier)
      ) locationsWithIdentifier

getImplementationDefinition :: NormalizedUri -> Position -> IdeAction (Maybe [Location])
getImplementationDefinition uri pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useWithStaleFastMT GetHieAst uri
    !pos' <- MaybeT (pure $ fromCurrentPosition mapping pos)
    locs <- AtPoint.gotoImplementation withHieDb (lookupMod hiedbWriter) opts hf pos'
    traverse (MaybeT . toCurrentLocation mapping uri) locs

highlightAtPoint :: NormalizedUri -> Position -> IdeAction (Maybe [DocumentHighlight])
highlightAtPoint uri pos = runMaybeT $ do
    (HAR _ hf rf _ _,mapping) <- useWithStaleFastMT GetHieAst uri
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    let toCurrentHighlight (DocumentHighlight range t) = flip DocumentHighlight t <$> toCurrentRange mapping range
    mapMaybe toCurrentHighlight <$>AtPoint.documentHighlight hf rf pos'

-- Refs are not an IDE action, so it is OK to be slow and (more) accurate
refsAtPoint :: NormalizedUri -> Position -> Action [Location]
refsAtPoint uri pos = do
    ShakeExtras{withHieDb} <- getShakeExtras
    fs <- HM.keys <$> getFilesOfInterestUntracked
    asts <- HM.fromList . mapMaybe sequence . zip fs <$> usesWithStale GetHieAst fs
    AtPoint.referencesAtPoint withHieDb uri pos (AtPoint.BOIReferences asts)

workspaceSymbols :: T.Text -> IdeAction (Maybe [SymbolInformation])
workspaceSymbols query = runMaybeT $ do
  ShakeExtras{withHieDb} <- ask
  res <- liftIO $ withHieDb (\hieDb -> HieDb.searchDef hieDb $ T.unpack query)
  pure $ mapMaybe AtPoint.defRowToSymbolInfo res
