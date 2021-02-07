module Development.IDE.Types.HscEnvEq
(   HscEnvEq,
    hscEnv, newHscEnvEq,
    hscEnvWithImportPaths,
    newHscEnvEqPreserveImportPaths,
    newHscEnvEqWithImportPaths,
    envImportPaths,
    envPackageExports,
    deps
) where


import Development.IDE.Types.Exports (ExportsMap, createExportsMap)
import Data.Unique
import Development.Shake.Classes
import Module (InstalledUnitId)
import System.Directory (canonicalizePath)
import Development.IDE.GHC.Compat
import GhcPlugins(HscEnv (hsc_dflags), PackageState (explicitPackages), InstalledPackageInfo (exposedModules), Module(..), packageConfigId)
import System.FilePath
import Development.IDE.GHC.Util (lookupPackageConfig)
import Control.Monad.IO.Class
import TcRnMonad (initIfaceLoad, WhereFrom (ImportByUser))
import LoadIface (loadInterface)
import qualified Maybes
import OpenTelemetry.Eventlog (withSpan)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Extra (mapMaybeM)

-- | An 'HscEnv' with equality. Two values are considered equal
--   if they are created with the same call to 'newHscEnvEq'.
data HscEnvEq = HscEnvEq
    { envUnique :: !Unique
    , hscEnv :: !HscEnv
    , deps   :: [(InstalledUnitId, DynFlags)]
               -- ^ In memory components for this HscEnv
               -- This is only used at the moment for the import dirs in
               -- the DynFlags
    , envImportPaths :: Maybe [String]
        -- ^ If Just, import dirs originally configured in this env
        --   If Nothing, the env import dirs are unaltered
    , envPackageExports :: ExportsMap
    }

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEq :: FilePath -> HscEnv -> [(InstalledUnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEq cradlePath hscEnv0 deps = do
    let relativeToCradle = (takeDirectory cradlePath </>)
        hscEnv = removeImportPaths hscEnv0

    -- Canonicalize import paths since we also canonicalize targets
    importPathsCanon <-
      mapM canonicalizePath $ relativeToCradle <$> importPaths (hsc_dflags hscEnv0)

    newHscEnvEqWithImportPaths (Just importPathsCanon) hscEnv deps

newHscEnvEqWithImportPaths :: Maybe [String] -> HscEnv -> [(InstalledUnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqWithImportPaths envImportPaths hscEnv deps = do
    envUnique <- newUnique

    let
    -- evaluate lazily, using unsafePerformIO for a pure API
      envPackageExports = unsafePerformIO $ withSpan "Package Exports" $ \_sp -> do
        -- compute the package imports
        let pkgst   = pkgState (hsc_dflags hscEnv)
            depends = explicitPackages pkgst
            targets =
                [ (pkg, mn)
                | d        <- depends
                , Just pkg <- [lookupPackageConfig d hscEnv]
                , (mn, _)  <- exposedModules pkg
                ]

            doOne (pkg, mn) = do
                modIface <- liftIO $ initIfaceLoad hscEnv $ loadInterface
                    ""
                    (Module (packageConfigId pkg) mn)
                    (ImportByUser False)
                return $ case modIface of
                    Maybes.Failed    _r -> Nothing
                    Maybes.Succeeded mi -> Just mi
        modIfaces <- mapMaybeM doOne targets
        return $ createExportsMap modIfaces
    return HscEnvEq{..}

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEqPreserveImportPaths
    :: HscEnv -> [(InstalledUnitId, DynFlags)] -> IO HscEnvEq
newHscEnvEqPreserveImportPaths = newHscEnvEqWithImportPaths Nothing

-- | Unwrap the 'HscEnv' with the original import paths.
--   Used only for locating imports
hscEnvWithImportPaths :: HscEnvEq -> HscEnv
hscEnvWithImportPaths HscEnvEq{..}
    | Just imps <- envImportPaths
    = hscEnv{hsc_dflags = (hsc_dflags hscEnv){importPaths = imps}}
    | otherwise
    = hscEnv

removeImportPaths :: HscEnv -> HscEnv
removeImportPaths hsc = hsc{hsc_dflags = (hsc_dflags hsc){importPaths = []}}

instance Show HscEnvEq where
  show HscEnvEq{envUnique} = "HscEnvEq " ++ show (hashUnique envUnique)

instance Eq HscEnvEq where
  a == b = envUnique a == envUnique b

instance NFData HscEnvEq where
  rnf (HscEnvEq a b c d _) =
      -- deliberately skip the package exports map
      rnf (hashUnique a) `seq` b `seq` c `seq` rnf d

instance Hashable HscEnvEq where
  hashWithSalt s = hashWithSalt s . envUnique

-- Fake instance needed to persuade Shake to accept this type as a key.
-- No harm done as ghcide never persists these keys currently
instance Binary HscEnvEq where
  put _ = error "not really"
  get = error "not really"
