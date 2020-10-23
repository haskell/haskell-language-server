-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.GHC.Warnings(withWarnings) where

import ErrUtils
import GhcPlugins as GHC hiding (Var)

import           Control.Concurrent.Extra
import qualified           Data.Text as T

import           Development.IDE.Types.Diagnostics
import           Development.IDE.GHC.Error


-- | Take a GHC monadic action (e.g. @typecheckModule pm@ for some
-- parsed module 'pm@') and produce a "decorated" action that will
-- harvest any warnings encountered executing the action. The 'phase'
-- argument classifies the context (e.g. "Parser", "Typechecker").
--
--   The ModSummary function is required because of
--   https://github.com/ghc/ghc/blob/5f1d949ab9e09b8d95319633854b7959df06eb58/compiler/main/GHC.hs#L623-L640
--   which basically says that log_action is taken from the ModSummary when GHC feels like it.
--   The given argument lets you refresh a ModSummary log_action
withWarnings :: T.Text -> ((ModSummary -> ModSummary) -> IO a) -> IO ([(WarnReason, FileDiagnostic)], a)
withWarnings diagSource action = do
  warnings <- newVar []
  let newAction :: DynFlags -> WarnReason -> Severity -> SrcSpan -> PprStyle -> SDoc -> IO ()
      newAction dynFlags wr _ loc style msg = do
        let wr_d = fmap (wr,) $ diagFromErrMsg diagSource dynFlags $ mkWarnMsg dynFlags loc (queryQual style) msg
        modifyVar_ warnings $ return . (wr_d:)
  res <- action $ \x -> x{ms_hspp_opts = (ms_hspp_opts x){log_action = newAction}}
  warns <- readVar warnings
  return (reverse $ concat warns, res)
