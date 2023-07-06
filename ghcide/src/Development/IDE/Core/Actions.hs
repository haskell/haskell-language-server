{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Core.Actions
( getAtPoint
, getDefinition
, getTypeDefinition
, highlightAtPoint
, refsAtPoint
, useE
, useNoFileE
, usesE
, workspaceSymbols
, lookupMod
) where

import           Control.Concurrent.MVar              (MVar, newEmptyMVar, putMVar, readMVar)
import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TQueue        (unGetTQueue)
import           Control.Monad                        (unless)
import           Control.Monad.Extra                  (mapMaybeM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString                      as BS
import qualified Data.HashMap.Strict                  as HM
import           Data.Maybe
import qualified Data.Text                            as T
import           Data.Tuple.Extra
import           Development.IDE.Core.Compile         (loadHieFile)
import           Development.IDE.Core.OfInterest
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat           hiding (writeHieFile)
import           Development.IDE.Graph
import qualified Development.IDE.Spans.AtPoint        as AtPoint
import           Development.IDE.Types.HscEnvEq       (hscEnv)
import           Development.IDE.Types.Location
import qualified HieDb
import           Language.LSP.Protocol.Types          (DocumentHighlight (..),
                                                       SymbolInformation (..),
                                                       normalizedFilePathToUri,
                                                       uriToNormalizedFilePath)
import           Language.LSP.Server                  (resRootPath)
import           System.Directory                     (createDirectoryIfMissing, doesFileExist)
import           System.FilePath                      ((</>), takeDirectory)


-- | Generates URIs for files in dependencies, but not in the
-- project.
lookupMod
  :: HieDbWriter -- ^ access the database
  -> FilePath -- ^ The `.hie` file we got from the database
  -> ModuleName
  -> Unit
  -> Bool -- ^ Is this file a boot file?
  -> MaybeT IdeAction Uri
lookupMod HieDbWriter{indexQueue} hieFile moduleName uid _boot = MaybeT $ do
  mProjectRoot <- (resRootPath =<<) <$> asks lspEnv
  case mProjectRoot of
    Nothing -> pure Nothing
    Just projectRoot -> do
      completionToken <- liftIO $ newEmptyMVar
      moduleUri <- writeAndIndexSource projectRoot completionToken
      liftIO $ readMVar completionToken
      pure $ Just moduleUri
  where
    writeAndIndexSource :: FilePath -> MVar () -> IdeAction Uri
    writeAndIndexSource projectRoot completionToken = do
      fileExists <- liftIO $ doesFileExist writeOutPath
      unless fileExists $ do
        nc <- asks ideNc
        liftIO $ do
          createDirectoryIfMissing True $ takeDirectory writeOutPath
          moduleSource <- hie_hs_src <$> loadHieFile (mkUpdater nc) hieFile
          BS.writeFile writeOutPath moduleSource
      liftIO $ atomically $
        unGetTQueue indexQueue $ \withHieDb -> withHieDb $ \db -> do
          HieDb.addSrcFile db hieFile writeOutPath False
          putMVar completionToken ()
      pure $ moduleUri
      where
        writeOutDir :: FilePath
        writeOutDir = projectRoot </> ".hls" </> "dependencies" </> show uid
        writeOutFile :: FilePath
        writeOutFile = toFilePath moduleName ++ ".hs"
        writeOutPath :: FilePath
        writeOutPath = writeOutDir </> writeOutFile
        moduleUri :: Uri
        moduleUri = AtPoint.toUri writeOutPath
        toFilePath :: ModuleName -> FilePath
        toFilePath = separateDirectories . prettyModuleName
          where
            separateDirectories :: FilePath -> FilePath
            separateDirectories moduleNameString =
              case breakOnDot moduleNameString of
                [] -> ""
                ms -> foldr1 (</>) ms
            breakOnDot :: FilePath -> [FilePath]
            breakOnDot = words . map replaceDotWithSpace
            replaceDotWithSpace :: Char -> Char
            replaceDotWithSpace '.' = ' '
            replaceDotWithSpace c = c
            prettyModuleName :: ModuleName -> String
            prettyModuleName = filter (/= '"')
              . concat
              . drop 1
              . words
              . show



-- IMPORTANT NOTE : make sure all rules `useE`d by these have a "Persistent Stale" rule defined,
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

  (hf, mapping) <- useE GetHieAst file
  (mEnv, mDkMap) <- case getSourceFileOrigin file of
    FromDependency -> pure (Nothing, Nothing)
    FromProject -> do
      env <- hscEnv . fst <$> useE GhcSession file
      dkMap <- lift $ maybe (DKMap mempty mempty) fst <$> runMaybeT (useE GetDocMap file)
      pure (Just env, Just dkMap)

  !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
  MaybeT $ pure $ first (toCurrentRange mapping =<<) <$> AtPoint.atPoint opts hf mDkMap mEnv pos'

-- | For each Loacation, determine if we have the PositionMapping
-- for the correct file. If not, get the correct position mapping
-- and then apply the position mapping to the location.
toCurrentLocations
  :: PositionMapping
  -> NormalizedFilePath
  -> [Location]
  -> IdeAction [Location]
toCurrentLocations mapping file = mapMaybeM go
  where
    go :: Location -> IdeAction (Maybe Location)
    go (Location uri range) =
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
          useE GetHieAst otherLocationFile
        pure $ Location uri <$> (flip toCurrentRange range =<< otherLocationMapping)
      where
        nUri :: NormalizedUri
        nUri = toNormalizedUri uri

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
useE k = MaybeT . useWithStaleFast k

useNoFileE :: IdeRule k v => IdeState -> k -> MaybeT IdeAction v
useNoFileE _ide k = fst <$> useE k emptyFilePath

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT IdeAction [(v,PositionMapping)]
usesE k = MaybeT . fmap sequence . mapM (useWithStaleFast k)

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [Location])
getDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (HAR _ hf _ _ _, mapping) <- useE GetHieAst file
    (ImportMap imports, _) <- useE GetImportMap file
    !pos' <- MaybeT (pure $ fromCurrentPosition mapping pos)
    locations <- AtPoint.gotoDefinition withHieDb (lookupMod hiedbWriter) opts imports hf pos'
    MaybeT $ Just <$> toCurrentLocations mapping file locations

getTypeDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [Location])
getTypeDefinition file pos = runMaybeT $ do
    ide@ShakeExtras{ withHieDb, hiedbWriter } <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (hf, mapping) <- useE GetHieAst file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    locations <- AtPoint.gotoTypeDefinition withHieDb (lookupMod hiedbWriter) opts hf pos'
    MaybeT $ Just <$> toCurrentLocations mapping file locations

highlightAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe [DocumentHighlight])
highlightAtPoint file pos = runMaybeT $ do
    (HAR _ hf rf _ _,mapping) <- useE GetHieAst file
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
