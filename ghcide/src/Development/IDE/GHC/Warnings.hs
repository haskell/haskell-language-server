-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP                #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Development.IDE.GHC.Warnings(withWarnings) where

import           Control.Concurrent.Strict
import           Control.Lens                      (over)
import qualified Data.Text                         as T

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Diagnostics

{-
 Note [withWarnings and its dangers]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    withWarnings collects warnings by registering a custom logger which extracts
    the SDocs of those warnings. If you receive warnings this way, you will not
    get them in a structured form. In the medium term we'd like to remove all
    uses of withWarnings to get structured messages everywhere we can.

    For the time being, withWarnings is no longer used for anything in the main
    typecheckModule codepath, but it is still used for bytecode/object code
    generation, as well as a few other places.

    I suspect some of these functions (e.g. codegen) will need deeper changes to
    be able to get diagnostics as a list, though I don't have great evidence for
    that atm. I haven't taken a look to see if those functions that are wrapped
    with this could produce diagnostics another way.

    It would be good for someone to take a look. What we've done so far gives us
    diagnostics for renaming and typechecking, and doesn't require us to copy
    too much code from GHC or make any deeper changes, and lets us get started
    with the bulk of the useful plugin work, but it would be good to have all
    diagnostics with structure be collected that way.
-}

-- | Take a GHC monadic action (e.g. @typecheckModule pm@ for some
-- parsed module 'pm@') and produce a "decorated" action that will
-- harvest any warnings encountered executing the action. The 'phase'
-- argument classifies the context (e.g. "Parser", "Typechecker").
--
--   The ModSummary function is required because of
--   https://github.com/ghc/ghc/blob/5f1d949ab9e09b8d95319633854b7959df06eb58/compiler/main/GHC.hs#L623-L640
--   which basically says that log_action is taken from the ModSummary when GHC feels like it.
--   The given argument lets you refresh a ModSummary log_action
--
-- Also, See Note [withWarnings and its dangers] for some commentary on this function.
withWarnings :: T.Text -> ((HscEnv -> HscEnv) -> IO a) -> IO ([(Maybe DiagnosticReason, FileDiagnostic)], a)
withWarnings diagSource action = do
  warnings <- newVar []
  let newAction :: DynFlags -> LogActionCompat
      newAction dynFlags logFlags wr _ loc prUnqual msg = do
        let wr_d = map ((wr,) . over fdLspDiagnosticL (attachReason wr)) $ diagFromSDocErrMsg diagSource dynFlags (mkWarnMsg dynFlags wr logFlags loc prUnqual msg)
        modifyVar_ warnings $ return . (wr_d:)
      newLogger env = pushLogHook (const (logActionCompat (newAction (hsc_dflags env)))) (hsc_logger env)
  res <- action $ \env -> putLogHook (newLogger env) env
  warns <- readVar warnings
  return (reverse $ concat warns, res)
