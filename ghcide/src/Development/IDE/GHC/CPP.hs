-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE NondecreasingIndentation #-}

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Development.IDE.GHC.CPP(doCpp, addOptP)
where

import           Development.IDE.GHC.Compat      as Compat
import           Development.IDE.GHC.Compat.Util
import           GHC

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Driver.Pipeline             as Pipeline
import           GHC.Settings
#elif MIN_VERSION_ghc (8,10,0)
import qualified DriverPipeline                  as Pipeline
import           ToolSettings
#endif
#if MIN_VERSION_ghc(9,3,0)
import qualified GHC.Driver.Pipeline.Execute     as Pipeline
#endif

addOptP :: String -> DynFlags -> DynFlags
addOptP f = alterToolSettings $ \s -> s
          { toolSettings_opt_P             = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
  where
    fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss
    alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }

doCpp :: HscEnv -> Bool -> FilePath -> FilePath -> IO ()
doCpp env raw input_fn output_fn =
#if MIN_VERSION_ghc (9,2,0)
    Pipeline.doCpp (hsc_logger env) (hsc_tmpfs env) (hsc_dflags env) (hsc_unit_env env) raw input_fn output_fn
#else
    Pipeline.doCpp (hsc_dflags env) raw input_fn output_fn
#endif

