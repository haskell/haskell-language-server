-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE ExplicitNamespaces #-}

module Development.IDE.GHC.Warnings(withWarnings) where

import           Control.Concurrent.Strict
import           Data.List
import qualified Data.Text                         as T

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Diagnostics
import           Language.LSP.Types                (type (|?) (..))


-- | Take a GHC monadic action (e.g. @typecheckModule pm@ for some
-- parsed module 'pm@') and produce a "decorated" action that will
-- harvest any warnings encountered executing the action. The 'phase'
-- argument classifies the context (e.g. "Parser", "Typechecker").
--
--   The ModSummary function is required because of
--   https://github.com/ghc/ghc/blob/5f1d949ab9e09b8d95319633854b7959df06eb58/compiler/main/GHC.hs#L623-L640
--   which basically says that log_action is taken from the ModSummary when GHC feels like it.
--   The given argument lets you refresh a ModSummary log_action
withWarnings :: T.Text -> ((HscEnv -> HscEnv) -> IO a) -> IO ([(WarnReason, FileDiagnostic)], a)
withWarnings diagSource action = do
  warnings <- newVar []
  let newAction :: LogActionCompat
      newAction dynFlags wr _ loc prUnqual msg = do
        let wr_d = map ((wr,) . third3 (attachReason wr)) $ diagFromErrMsg diagSource dynFlags $ mkWarnMsg dynFlags loc prUnqual msg
        modifyVar_ warnings $ return . (wr_d:)
      newLogger env = pushLogHook (const (logActionCompat newAction)) (hsc_logger env)
  res <- action $ \env -> putLogHook (newLogger env) env
  warns <- readVar warnings
  return (reverse $ concat warns, res)
  where
    third3 :: (c -> d) -> (a, b, c) -> (a, b, d)
    third3 f (a, b, c) = (a, b, f c)

attachReason :: WarnReason -> Diagnostic -> Diagnostic
attachReason wr d = d{_code = InR <$> showReason wr}
 where
  showReason = \case
    NoReason       -> Nothing
    Reason flag    -> showFlag flag
    ErrReason flag -> showFlag =<< flag

showFlag :: WarningFlag -> Maybe T.Text
showFlag flag = ("-W" <>) . T.pack . flagSpecName <$> find ((== flag) . flagSpecFlag) wWarningFlags
