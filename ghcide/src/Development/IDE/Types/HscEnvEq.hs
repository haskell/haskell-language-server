module Development.IDE.Types.HscEnvEq
(   HscEnvEq,
    hscEnv, newHscEnvEq,
    hscEnvWithImportPaths,
    newHscEnvEqPreserveImportPaths,
    newHscEnvEqWithImportPaths,
    updateHscEnvEq,
    envImportPaths,
    envPackageExports,
    envVisibleModuleNames,
    deps
) where


import           Control.Concurrent.Async        (Async, async, waitCatch)
import           Control.Concurrent.MVar         (newEmptyMVar, putMVar, readMVar)
import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TQueue   (writeTQueue)
import           Control.Concurrent.Strict       (modifyVar, newVar)
import           Control.DeepSeq                 (force)
import           Control.Exception               (evaluate, mask, throwIO)
import           Control.Monad                   (unless)
import           Control.Monad.Extra             (eitherM, join, mapMaybeM, void)
import           Data.Either                     (fromRight)
import           Data.Foldable                   (traverse_)
import qualified Data.Map                        as Map
import           Data.Maybe                      (isNothing)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Unique                     (Unique)
import qualified Data.Unique                     as Unique
import           Development.IDE.Core.Compile    (indexHieFile)
import           Development.IDE.Core.Rules      (HieFileCheck(..), Log, checkHieFile)
import           Development.IDE.Core.Shake      (HieDbWriter(indexQueue), ShakeExtras(hiedbWriter, lspEnv))
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util as Maybes
import           Development.IDE.GHC.Error       (catchSrcErrors)
import           Development.IDE.GHC.Util        (lookupPackageConfig)
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Exports   (ExportsMap, createExportsMap)
import           Development.IDE.Types.Location  (NormalizedFilePath, toNormalizedFilePath')
import           Development.IDE.Types.Logger    (Recorder, WithPriority)
import           HieDb                           (SourceFile(FakeFile), removeDependencySrcFiles)
import           Language.LSP.Server             (resRootPath)
import           OpenTelemetry.Eventlog          (withSpan)
import           System.Directory                (doesDirectoryExist, makeAbsolute)
import           System.FilePath

-- | An 'HscEnv' with equality. Two values are considered equal
--   if they are created with the same call to 'newHscEnvEq' or
--   'updateHscEnvEq'.
data HscEnvEq = HscEnvEq
    { envUnique             :: !Unique
    , hscEnv                :: !HscEnv
    , deps                  :: [(UnitId, DynFlags)]
               -- ^ In memory components for this HscEnv
               -- This is only used at the moment for the import dirs in
               -- the DynFlags
    , envImportPaths        :: Maybe (Set FilePath)
        -- ^ If Just, import dirs originally configured in this env
        --   If Nothing, the env import dirs are unaltered
    , envPackageExports     :: IO ExportsMap
    , envVisibleModuleNames :: IO (Maybe [ModuleName])
        -- ^ 'listVisibleModuleNames' is a pure function,
        -- but it could panic due to a ghc bug: https://github.com/haskell/haskell-language-server/issues/1365
        -- So it's wrapped in IO here for error handling
        -- If Nothing, 'listVisibleModuleNames' panic
    }

updateHscEnvEq :: HscEnvEq -> HscEnv -> IO HscEnvEq
updateHscEnvEq oldHscEnvEq newHscEnv = do
  let update newUnique = oldHscEnvEq { envUnique = newUnique, hscEnv = newHscEnv }
  update <$> Unique.newUnique

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEq :: FilePath -> Recorder (WithPriority Log) -> ShakeExtras -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEq cradlePath recorder se hscEnv0 deps = do
    let relativeToCradle = (takeDirectory cradlePath </>)
        hscEnv = removeImportPaths hscEnv0

    -- Make Absolute since targets are also absolute
    importPathsCanon <-
      mapM makeAbsolute $ relativeToCradle <$> importPaths (hsc_dflags hscEnv0)

    newHscEnvEqWithImportPaths (Just $ Set.fromList importPathsCanon) recorder se hscEnv deps

newHscEnvEqWithImportPaths :: Maybe (Set FilePath) -> Recorder (WithPriority Log) -> ShakeExtras -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqWithImportPaths envImportPaths recorder se hscEnv deps = do

    indexDependencyHieFiles

    envUnique <- Unique.newUnique

    -- it's very important to delay the package exports computation
    envPackageExports <- onceAsync $ withSpan "Package Exports" $ \_sp -> do
        -- compute the package imports
        let pkgst   = unitState hscEnv
            depends = explicitUnits pkgst
            modules =
                [ m
                | d        <- depends
                , Just pkg <- [lookupPackageConfig d hscEnv]
                , (modName, maybeOtherPkgMod) <- unitExposedModules pkg
                , let m = case maybeOtherPkgMod of
                        -- When module is re-exported from another package,
                        -- the origin module is represented by value in Just
                        Just otherPkgMod -> otherPkgMod
                        Nothing          -> mkModule (unitInfoId pkg) modName
                ]
        modIfaces <- mapMaybeM loadModIface modules
        return $ createExportsMap modIfaces

    let dflags = hsc_dflags hscEnv
    -- similar to envPackageExports, evaluated lazily
    envVisibleModuleNames <- onceAsync $
      fromRight Nothing
        <$> catchSrcErrors
          dflags
          "listVisibleModuleNames"
          (evaluate . force . Just $ listVisibleModuleNames hscEnv)

    return HscEnvEq{..}
    where
        indexDependencyHieFiles :: IO ()
        indexDependencyHieFiles = do
            dotHlsDirExists <- maybe (pure False) doesDirectoryExist mHlsDir
            unless dotHlsDirExists deleteMissingDependencySources
            void $ Map.traverseWithKey indexPackageHieFiles packagesWithModules
        mHlsDir :: Maybe FilePath
        mHlsDir = do
            projectDir <- resRootPath =<< lspEnv se
            pure $ projectDir </> ".hls"
        deleteMissingDependencySources :: IO ()
        deleteMissingDependencySources = do
            completionToken <- newEmptyMVar
            atomically $ writeTQueue (indexQueue $ hiedbWriter se) $
                \withHieDb -> do
                    withHieDb $ \db ->
                        removeDependencySrcFiles db
                    putMVar completionToken ()
            readMVar completionToken
        indexPackageHieFiles :: Package -> [Module] -> IO ()
        indexPackageHieFiles (Package package) modules = do
            let pkgLibDir :: FilePath
                pkgLibDir = case unitLibraryDirs package of
                  [] -> ""
                  (libraryDir : _) -> libraryDir
                hieDir :: FilePath
                hieDir = pkgLibDir </> "extra-compilation-artifacts"
            traverse_ (indexModuleHieFile hieDir) modules
        indexModuleHieFile :: FilePath -> Module -> IO ()
        indexModuleHieFile hieDir m = do
            let hiePath :: NormalizedFilePath
                hiePath = toNormalizedFilePath' $
                  hieDir </> moduleNameSlashes (moduleName m) <.> "hie"
            hieCheck <- checkHieFile recorder se "newHscEnvEqWithImportPaths" hiePath
            case hieCheck of
                HieFileMissing -> return ()
                HieAlreadyIndexed -> return ()
                CouldNotLoadHie _e -> return ()
                DoIndexing hash hie ->
                    indexHieFile se hiePath (FakeFile Nothing) hash hie
        loadModIface :: Module -> IO (Maybe ModIface)
        loadModIface m = do
            modIface <- initIfaceLoad hscEnv $
                loadInterface "" m (ImportByUser NotBoot)
            return $ case modIface of
                Maybes.Failed    _r -> Nothing
                Maybes.Succeeded mi -> Just mi
        packagesWithModules :: Map.Map Package [Module]
        packagesWithModules = Map.fromSet getModulesForPackage packages
        packages :: Set Package
        packages = Set.fromList
            $ map Package
            $ Map.elems
            $ getUnitInfoMap hscEnv
        getModulesForPackage :: Package -> [Module]
        getModulesForPackage (Package package) =
            map makeModule allModules
            where
                allModules :: [ModuleName]
                allModules = map fst
                    ( filter (isNothing . snd)
                    $ unitExposedModules package
                    )
                    ++ unitHiddenModules package
                makeModule :: ModuleName
                           -> Module
                makeModule = mkModule (unitInfoId package)

newtype Package = Package UnitInfo deriving Eq
instance Ord Package where
  compare (Package u1) (Package u2) = compare (unitId u1) (unitId u2)


-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEqPreserveImportPaths
    :: Recorder (WithPriority Log) -> ShakeExtras -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqPreserveImportPaths = newHscEnvEqWithImportPaths Nothing

-- | Unwrap the 'HscEnv' with the original import paths.
--   Used only for locating imports
hscEnvWithImportPaths :: HscEnvEq -> HscEnv
hscEnvWithImportPaths HscEnvEq{..}
    | Just imps <- envImportPaths
    = hscSetFlags (setImportPaths (Set.toList imps) (hsc_dflags hscEnv)) hscEnv
    | otherwise
    = hscEnv

removeImportPaths :: HscEnv -> HscEnv
removeImportPaths hsc = hscSetFlags (setImportPaths [] (hsc_dflags hsc)) hsc

instance Show HscEnvEq where
  show HscEnvEq{envUnique} = "HscEnvEq " ++ show (Unique.hashUnique envUnique)

instance Eq HscEnvEq where
  a == b = envUnique a == envUnique b

instance NFData HscEnvEq where
  rnf (HscEnvEq a b c d _ _) =
      -- deliberately skip the package exports map and visible module names
      rnf (Unique.hashUnique a) `seq` b `seq` c `seq` rnf d

instance Hashable HscEnvEq where
  hashWithSalt s = hashWithSalt s . envUnique

-- | Given an action, produce a wrapped action that runs at most once.
--   The action is run in an async so it won't be killed by async exceptions
--   If the function raises an exception, the same exception will be reraised each time.
onceAsync :: IO a -> IO (IO a)
onceAsync act = do
    var <- newVar OncePending
    let run as = eitherM throwIO pure (waitCatch as)
    pure $ mask $ \unmask -> join $ modifyVar var $ \v -> case v of
        OnceRunning x -> pure (v, unmask $ run x)
        OncePending -> do
            x <- async (unmask act)
            pure (OnceRunning x, unmask $ run x)

data Once a = OncePending | OnceRunning (Async a)
