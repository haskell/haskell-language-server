-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-missing-fields #-} -- to enable prettyPrint
{-# LANGUAGE CPP #-}

-- | GHC utility functions. Importantly, code using our GHC should never:
--
-- * Call runGhc, use runGhcFast instead. It's faster and doesn't require config we don't have.
--
-- * Call setSessionDynFlags, use modifyDynFlags instead. It's faster and avoids loading packages.
module Development.IDE.GHC.Util(
    lookupPackageConfig,
    modifyDynFlags,
    fakeDynFlags,
    prettyPrint,
    runGhcEnv,
    textToStringBuffer,
    moduleImportPaths
    ) where

import Config
import Data.List.Extra
#if __GLASGOW_HASKELL__ >= 806
import Fingerprint
#endif
import GHC
import GhcMonad
import GhcPlugins
import Data.IORef
import Control.Exception
import FileCleanup
import Platform
import qualified Data.Text as T
import StringBuffer
import System.FilePath


----------------------------------------------------------------------
-- GHC setup

modifyDynFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyDynFlags f = do
  newFlags <- f <$> getSessionDynFlags
  -- We do not use setSessionDynFlags here since we handle package
  -- initialization separately.
  modifySession $ \h ->
    h { hsc_dflags = newFlags, hsc_IC = (hsc_IC h) {ic_dflags = newFlags} }

lookupPackageConfig :: UnitId -> HscEnv -> Maybe PackageConfig
lookupPackageConfig unitId env =
    lookupPackage' False pkgConfigMap unitId
    where
        pkgConfigMap =
            -- For some weird reason, the GHC API does not provide a way to get the PackageConfigMap
            -- from PackageState so we have to wrap it in DynFlags first.
            getPackageConfigMap $ hsc_dflags env


-- would be nice to do this more efficiently...
textToStringBuffer :: T.Text -> StringBuffer
textToStringBuffer = stringToStringBuffer . T.unpack


prettyPrint :: Outputable a => a -> String
prettyPrint = showSDoc fakeDynFlags . ppr

runGhcEnv :: HscEnv -> Ghc a -> IO a
runGhcEnv env act = do
    filesToClean <- newIORef emptyFilesToClean
    dirsToClean <- newIORef mempty
    let dflags = (hsc_dflags env){filesToClean=filesToClean, dirsToClean=dirsToClean}
    ref <- newIORef env{hsc_dflags=dflags}
    unGhc act (Session ref) `finally` do
        cleanTempFiles dflags
        cleanTempDirs dflags

-- Fake DynFlags which are mostly undefined, but define enough to do a
-- little bit.
fakeDynFlags :: DynFlags
fakeDynFlags = defaultDynFlags settings mempty
    where
        settings = Settings
                   { sTargetPlatform = platform
                   , sPlatformConstants = platformConstants
                   , sProgramName = "ghc"
                   , sProjectVersion = cProjectVersion
#if __GLASGOW_HASKELL__ >= 806
                    , sOpt_P_fingerprint = fingerprint0
#endif
                    }
        platform = Platform
          { platformWordSize=8
          , platformOS=OSUnknown
          , platformUnregisterised=True
          }
        platformConstants = PlatformConstants
          { pc_DYNAMIC_BY_DEFAULT=False
          , pc_WORD_SIZE=8
          }

moduleImportPaths :: GHC.ParsedModule -> Maybe FilePath
moduleImportPaths pm
  | rootModDir == "." = Just rootPathDir
  | otherwise =
    dropTrailingPathSeparator <$> stripSuffix (normalise rootModDir) (normalise rootPathDir)
  where
    ms   = GHC.pm_mod_summary pm
    file = GHC.ms_hspp_file ms
    mod'  = GHC.ms_mod ms
    rootPathDir  = takeDirectory file
    rootModDir   = takeDirectory . moduleNameSlashes . GHC.moduleName $ mod'
