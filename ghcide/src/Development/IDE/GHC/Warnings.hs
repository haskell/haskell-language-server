-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Development.IDE.GHC.Warnings(withWarnings) where

import           Control.Concurrent.Strict
import qualified Data.Text                         as T

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Diagnostics


-- | Take a GHC monadic action (e.g. @typecheckModule pm@ for some
-- parsed module 'pm@') and produce a "decorated" action that will
-- harvest any warnings encountered executing the action. The 'phase'
-- argument classifies the context (e.g. "Parser", "Typechecker").
--
--   The ModSummary function is required because of
--   https://github.com/ghc/ghc/blob/5f1d949ab9e09b8d95319633854b7959df06eb58/compiler/main/GHC.hs#L623-L640
--   which basically says that log_action is taken from the ModSummary when GHC feels like it.
--   The given argument lets you refresh a ModSummary log_action
#if MIN_VERSION_ghc(9,3,0)
withWarnings :: T.Text -> ((HscEnv -> HscEnv) -> IO a) -> IO ([(Maybe DiagnosticReason, FileDiagnostic)], a)
#else
withWarnings :: T.Text -> ((HscEnv -> HscEnv) -> IO a) -> IO ([(WarnReason, FileDiagnostic)], a)
#endif
withWarnings diagSource action = do
  warnings <- newVar []
  let newAction :: DynFlags -> LogActionCompat
      newAction dynFlags logFlags wr _ loc prUnqual msg = do
        let wr_d = map (wr,) $ diagFromSDocErrMsg diagSource dynFlags (mkWarnMsg dynFlags wr logFlags loc prUnqual msg)
        modifyVar_ warnings $ return . (wr_d:)
      newLogger env = pushLogHook (const (logActionCompat (newAction (hsc_dflags env)))) (hsc_logger env)
  res <- action $ \env -> putLogHook (newLogger env) env
  warns <- readVar warnings
  return (reverse $ concat warns, res)
