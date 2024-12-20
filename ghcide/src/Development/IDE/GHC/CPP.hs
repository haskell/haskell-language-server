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
import           GHC.Settings

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,5,0)
import qualified GHC.Driver.Pipeline.Execute     as Pipeline
#endif

#if MIN_VERSION_ghc(9,5,0)
import qualified GHC.SysTools.Cpp                as Pipeline
#endif

#if MIN_VERSION_ghc(9,11,0)
import qualified GHC.SysTools.Tasks              as Pipeline
#endif

#if MIN_VERSION_ghc(9,11,0)
import qualified GHC.SysTools.Tasks              as Pipeline
#endif

addOptP :: String -> DynFlags -> DynFlags
addOptP f = alterToolSettings $ \s -> s
          { toolSettings_opt_P             = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
  where
    fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss
    alterToolSettings g dynFlags = dynFlags { toolSettings = g (toolSettings dynFlags) }

doCpp :: HscEnv -> FilePath -> FilePath -> IO ()
doCpp env input_fn output_fn =
        -- See GHC commit a2f53ac8d968723417baadfab5be36a020ea6850
        -- this function/Pipeline.doCpp previously had a raw parameter
        -- always set to True that corresponded to these settings

#if MIN_VERSION_ghc(9,5,0)
    let cpp_opts = Pipeline.CppOpts
                 { cppLinePragmas = True
#if MIN_VERSION_ghc(9,11,0)
                 , sourceCodePreprocessor = Pipeline.SCPHsCpp
#elif MIN_VERSION_ghc(9,10,0)
                 , useHsCpp = True
#else
                 , cppUseCc = False
#endif
                 } in
#else
    let cpp_opts = True in
#endif

    Pipeline.doCpp (hsc_logger env) (hsc_tmpfs env) (hsc_dflags env) (hsc_unit_env env) cpp_opts input_fn output_fn

