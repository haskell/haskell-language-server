-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -Wno-missing-fields #-} -- to enable prettyPrint
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

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
    moduleImportPath,
    HscEnvEq, hscEnv, newHscEnvEq,
    readFileUtf8,
    hDuplicateTo',
    cgGutsToCoreModule
    ) where

import Config
import Control.Concurrent
import Data.List.Extra
import Data.Maybe
import Data.Typeable
#if MIN_GHC_API_VERSION(8,6,0)
import Fingerprint
#endif
import GHC
import GhcMonad
import GhcPlugins hiding (Unique)
import Data.IORef
import Control.Exception
import FileCleanup
import GHC.IO.BufferedIO (BufferedIO)
import GHC.IO.Device as IODevice
import GHC.IO.Encoding
import GHC.IO.Exception
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals
import Platform
import Data.Unique
import Development.Shake.Classes
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.ByteString          as BS
import StringBuffer
import System.FilePath

import Development.IDE.Types.Location


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
    let dflags = (hsc_dflags env){filesToClean=filesToClean, dirsToClean=dirsToClean, useUnicode=True}
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
#if MIN_GHC_API_VERSION(8,6,0)
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

moduleImportPath :: NormalizedFilePath -> GHC.ParsedModule -> Maybe FilePath
-- The call to takeDirectory is required since DAML does not require that
-- the file name matches the module name in the last component.
-- Once that has changed we can get rid of this.
moduleImportPath (takeDirectory . fromNormalizedFilePath -> pathDir) pm
    -- This happens for single-component modules since takeDirectory "A" == "."
    | modDir == "." = Just pathDir
    | otherwise = dropTrailingPathSeparator <$> stripSuffix modDir pathDir
  where
    ms   = GHC.pm_mod_summary pm
    mod'  = GHC.ms_mod ms
    -- A for module A.B
    modDir =
        takeDirectory $
        fromNormalizedFilePath $ toNormalizedFilePath $
        moduleNameSlashes $ GHC.moduleName mod'

-- | An HscEnv with equality.
data HscEnvEq = HscEnvEq Unique HscEnv

hscEnv :: HscEnvEq -> HscEnv
hscEnv (HscEnvEq _ x) = x

newHscEnvEq :: HscEnv -> IO HscEnvEq
newHscEnvEq e = do u <- newUnique; return $ HscEnvEq u e

instance Show HscEnvEq where
  show (HscEnvEq a _) = "HscEnvEq " ++ show (hashUnique a)

instance Eq HscEnvEq where
  HscEnvEq a _ == HscEnvEq b _ = a == b

instance NFData HscEnvEq where
  rnf (HscEnvEq a b) = rnf (hashUnique a) `seq` b `seq` ()

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = T.decodeUtf8With T.lenientDecode <$> BS.readFile f

cgGutsToCoreModule :: SafeHaskellMode -> CgGuts -> ModDetails -> CoreModule
cgGutsToCoreModule safeMode guts modDetails = CoreModule
    (cg_module guts)
    (md_types modDetails)
    (cg_binds guts)
    safeMode

-- This is a slightly modified version of hDuplicateTo in GHC.
-- See the inline comment for more details.
hDuplicateTo' :: Handle -> Handle -> IO ()
hDuplicateTo' h1@(FileHandle path m1) h2@(FileHandle _ m2)  = do
 withHandle__' "hDuplicateTo" h2 m2 $ \h2_ -> do
   -- The implementation in base has this call to hClose_help.
   -- _ <- hClose_help h2_
   -- hClose_help does two things:
   -- 1. It flushes the buffer, we replicate this here
   _ <- flushWriteBuffer h2_ `catch` \(_ :: IOException) -> pure ()
   -- 2. It closes the handle. This is redundant since dup2 takes care of that
   -- but even worse it is actively harmful! Once the handle has been closed
   -- another thread is free to reallocate it. This leads to dup2 failing with EBUSY
   -- if it happens just in the right moment.
   withHandle_' "hDuplicateTo" h1 m1 $ \h1_ -> do
     dupHandleTo path h1 Nothing h2_ h1_ (Just handleFinalizer)
hDuplicateTo' h1@(DuplexHandle path r1 w1) h2@(DuplexHandle _ r2 w2)  = do
 withHandle__' "hDuplicateTo" h2 w2  $ \w2_ -> do
   _ <- hClose_help w2_
   withHandle_' "hDuplicateTo" h1 w1 $ \w1_ -> do
     dupHandleTo path h1 Nothing w2_ w1_ (Just handleFinalizer)
 withHandle__' "hDuplicateTo" h2 r2  $ \r2_ -> do
   _ <- hClose_help r2_
   withHandle_' "hDuplicateTo" h1 r1 $ \r1_ -> do
     dupHandleTo path h1 (Just w1) r2_ r1_ Nothing
hDuplicateTo' h1 _ =
  ioe_dupHandlesNotCompatible h1

-- | This is copied unmodified from GHC since it is not exposed.
dupHandleTo :: FilePath
            -> Handle
            -> Maybe (MVar Handle__)
            -> Handle__
            -> Handle__
            -> Maybe HandleFinalizer
            -> IO Handle__
dupHandleTo filepath h other_side
            _hto_@Handle__{haDevice=devTo}
            h_@Handle__{haDevice=dev} mb_finalizer = do
  flushBuffer h_
  case cast devTo of
    Nothing   -> ioe_dupHandlesNotCompatible h
    Just dev' -> do
      _ <- IODevice.dup2 dev dev'
      FileHandle _ m <- dupHandle_ dev' filepath other_side h_ mb_finalizer
      takeMVar m

-- | This is copied unmodified from GHC since it is not exposed.
-- Note the beautiful inline comment!
dupHandle_ :: (IODevice dev, BufferedIO dev, Typeable dev) => dev
           -> FilePath
           -> Maybe (MVar Handle__)
           -> Handle__
           -> Maybe HandleFinalizer
           -> IO Handle
dupHandle_ new_dev filepath other_side _h_@Handle__{..} mb_finalizer = do
   -- XXX wrong!
  mb_codec <- if isJust haEncoder then fmap Just getLocaleEncoding else return Nothing
  mkHandle new_dev filepath haType True{-buffered-} mb_codec
      NewlineMode { inputNL = haInputNL, outputNL = haOutputNL }
      mb_finalizer other_side

-- | This is copied unmodified from GHC since it is not exposed.
ioe_dupHandlesNotCompatible :: Handle -> IO a
ioe_dupHandlesNotCompatible h =
   ioException (IOError (Just h) IllegalOperation "hDuplicateTo"
                "handles are incompatible" Nothing Nothing)
