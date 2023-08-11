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

import           Control.Monad
-- 8.10 The import of ‘Control.Monad’ is redundant except perhaps to import instances from ‘Control.Monad’
import           Development.IDE.GHC.Compat      as Compat
import           Development.IDE.GHC.Compat.Util
import           GHC

#if MIN_VERSION_ghc (8,10,0) && !MIN_VERSION_ghc(9,0,0)
import qualified DriverPipeline                  as Pipeline
import           ToolSettings
#endif

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Driver.Pipeline             as Pipeline
import           GHC.Settings
#endif

#if MIN_VERSION_ghc(9,3,0)
import qualified GHC.Driver.Pipeline.Execute     as Pipeline
#endif

#if MIN_VERSION_ghc(9,5,0)
import qualified GHC.SysTools.Cpp                as Pipeline
#endif

addOptP :: String -> DynFlags -> DynFlags
addOptP f = alterToolSettings $ \s -> s
          { toolSettings_opt_P             = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
  where
    fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss
    alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }

doCpp :: HscEnv -> FilePath -> FilePath -> IO ()
doCpp env input_fn output_fn =
        -- See GHC commit a2f53ac8d968723417baadfab5be36a020ea6850
        -- this function/Pipeline.doCpp previously had a raw parameter
        -- always set to True that corresponded to these settings

#if MIN_VERSION_ghc(9,5,0)
    let cpp_opts = Pipeline.CppOpts
                 { cppUseCc = False
                 , cppLinePragmas = True
                 } in
#else
    let cpp_opts = True in
#endif

#if MIN_VERSION_ghc(9,2,0)
    Pipeline.doCpp (hsc_logger env) (hsc_tmpfs env) (hsc_dflags env) (hsc_unit_env env) cpp_opts input_fn output_fn
#else
    Pipeline.doCpp (hsc_dflags env) cpp_opts input_fn output_fn
#endif

