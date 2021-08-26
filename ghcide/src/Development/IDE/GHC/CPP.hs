-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- Copied from https://github.com/ghc/ghc/blob/master/compiler/main/DriverPipeline.hs on 14 May 2019
-- Requested to be exposed at https://gitlab.haskell.org/ghc/ghc/merge_requests/944.
-- Update the above MR got merged to master on 31 May 2019. When it becomes avialable to ghc-lib, this file can be removed.

{- HLINT ignore -} -- since copied from upstream

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- GHC Driver
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Development.IDE.GHC.CPP(doCpp, addOptP)
where

import           GHC
import           Development.IDE.GHC.Compat as Compat
#if !MIN_VERSION_ghc(8,10,0)
import qualified Development.IDE.GHC.Compat.CPP as CPP
#endif

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Driver.Pipeline as Pipeline
import           GHC.SysTools as SysTools
import           GHC.Settings
import           GHC.Utils.Fingerprint
#else
#if MIN_VERSION_ghc (8,10,0)
import qualified DriverPipeline as Pipeline
import           ToolSettings
#else
import           DynFlags
#endif
#endif

addOptP :: String -> DynFlags -> DynFlags
#if MIN_VERSION_ghc (8,10,0)
addOptP f = alterToolSettings $ \s -> s
          { toolSettings_opt_P             = f : toolSettings_opt_P s
          , toolSettings_opt_P_fingerprint = fingerprintStrings (f : toolSettings_opt_P s)
          }
  where
    fingerprintStrings ss = fingerprintFingerprints $ map fingerprintString ss
    alterToolSettings f dynFlags = dynFlags { toolSettings = f (toolSettings dynFlags) }
#else
addOptP opt = onSettings (onOptP (opt:))
  where
    onSettings f x = x{settings = f $ settings x}
    onOptP f x = x{sOpt_P = f $ sOpt_P x}
#endif

doCpp :: HscEnv -> Bool -> FilePath -> FilePath -> IO ()
doCpp env raw input_fn output_fn =
#if MIN_VERSION_ghc (9,2,0)
    Pipeline.doCpp (hsc_logger env) (hsc_tmpfs env) (hsc_dflags env) (hsc_unit_env env) raw input_fn output_fn
#elif MIN_VERSION_ghc (8,10,0)
    Pipeline.doCpp (hsc_dflags env) raw input_fn output_fn
#else
    CPP.doCpp (hsc_dflags env) raw input_fn output_fn
#endif

