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
import           Control.Concurrent.Strict       (modifyVar, newVar)
import           Control.DeepSeq                 (force)
import           Control.Exception               (evaluate, mask, throwIO)
import           Control.Monad.Extra             (eitherM, join, mapMaybeM)
import           Data.Either                     (fromRight)
import qualified Data.Map                        as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import qualified Data.Text                       as T
import           Data.Unique                     (Unique)
import qualified Data.Unique                     as Unique
import           Development.IDE.Core.Compile    (indexHieFile, loadHieFile)
import           Development.IDE.Core.Shake      (ShakeExtras(ideNc, logger), mkUpdater)
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util as Maybes
import           Development.IDE.GHC.Error       (catchSrcErrors)
import           Development.IDE.GHC.Util        (lookupPackageConfig)
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Exports   (ExportsMap, createExportsMap)
import           Development.IDE.Types.Location  (toNormalizedFilePath')
import qualified Development.IDE.Types.Logger    as Logger
import           HieDb                           (SourceFile(FakeFile))
import           OpenTelemetry.Eventlog          (withSpan)
import           System.Directory                (makeAbsolute)
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
newHscEnvEq :: FilePath -> ShakeExtras -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEq cradlePath se hscEnv0 deps = do
    let relativeToCradle = (takeDirectory cradlePath </>)
        hscEnv = removeImportPaths hscEnv0

    -- Make Absolute since targets are also absolute
    importPathsCanon <-
      mapM makeAbsolute $ relativeToCradle <$> importPaths (hsc_dflags hscEnv0)

    newHscEnvEqWithImportPaths (Just $ Set.fromList importPathsCanon) se hscEnv deps

newtype UnitInfoOrd = UnitInfoOrd UnitInfo deriving Eq
instance Ord UnitInfoOrd where
  compare (UnitInfoOrd u1) (UnitInfoOrd u2) = compare (unitId u1) (unitId u2)

newHscEnvEqWithImportPaths :: Maybe (Set FilePath) -> ShakeExtras -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqWithImportPaths envImportPaths se hscEnv deps = do

    let dflags = hsc_dflags hscEnv

    envUnique <- Unique.newUnique

    -- it's very important to delay the package exports computation
    envPackageExports <- onceAsync $ withSpan "Package Exports" $ \_sp -> do
        -- compute the package imports
        let pkgst   = unitState hscEnv
            depends = explicitUnits pkgst
            packages = [ pkg
                       | d <- depends
                       , Just pkg <- [lookupPackageConfig d hscEnv]
                       ]
            modules = Map.fromSet
                (\(UnitInfoOrd pkg) ->
                  [ m
                  | (modName, maybeOtherPkgMod) <- unitExposedModules pkg
                  , let m = case maybeOtherPkgMod of
                          -- When module is re-exported from another package,
                          -- the origin module is represented by value in Just
                          Just otherPkgMod -> otherPkgMod
                          Nothing          -> mkModule (unitInfoId pkg) modName
                  ]
                )
                (Set.fromList $ map UnitInfoOrd packages)

            logPackage :: UnitInfo -> IO ()
            logPackage pkg = Logger.logDebug (logger se) $ "\n\n\n!!!!!!!!!!!! hscEnvEq :\n"
              <> T.pack (concatMap show $ unitLibraryDirs pkg)
              <> "\n!!!!!!!!!!!!!!!!!!!!!!\n\n\n"
            doOnePackage :: UnitInfoOrd -> [Module] -> IO [ModIface]
            doOnePackage (UnitInfoOrd pkg) ms = do
              let pkgLibDir :: FilePath
                  pkgLibDir = case unitLibraryDirs pkg of
                    [] -> ""
                    (libraryDir : _) -> libraryDir
                  hieDir :: FilePath
                  hieDir = pkgLibDir </> "extra-compliation-artifacts"
              logPackage pkg
              mapMaybeM (doOne hieDir) ms

            doOne :: FilePath -> Module -> IO (Maybe ModIface)
            doOne hieDir m = do
                let toFilePath :: ModuleName -> FilePath
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
                    hiePath :: FilePath
                    hiePath = hieDir </> toFilePath (moduleName m) ++ ".hie"
                modIface <- initIfaceLoad hscEnv $
                    loadInterface "" m (ImportByUser NotBoot)
                case modIface of
                    Maybes.Failed    _r -> return Nothing
                    Maybes.Succeeded mi -> do
                      hie <- loadHieFile (mkUpdater $ ideNc se) hiePath
                      indexHieFile se (toNormalizedFilePath' hiePath) (FakeFile Nothing) (mi_src_hash mi) hie
                      return $ Just mi
        modIfaces <- concat . Map.elems <$> Map.traverseWithKey doOnePackage modules
        return $ createExportsMap modIfaces

    -- similar to envPackageExports, evaluated lazily
    envVisibleModuleNames <- onceAsync $
      fromRight Nothing
        <$> catchSrcErrors
          dflags
          "listVisibleModuleNames"
          (evaluate . force . Just $ listVisibleModuleNames hscEnv)

    return HscEnvEq{..}

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEqPreserveImportPaths
    :: ShakeExtras -> HscEnv -> [(UnitId, DynFlags)] -> IO HscEnvEq
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
