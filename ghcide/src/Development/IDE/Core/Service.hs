-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Service(
    getIdeOptions,
    IdeState, initialise, shutdown,
    runAction, runActions,
    runActionSync, runActionsSync,
    getFilesOfInterest, setFilesOfInterest, modifyFilesOfInterest,
    writeProfile,
    getDiagnostics, unsafeClearDiagnostics,
    ideLogger
    ) where

import           Control.Concurrent.Extra
import           Control.Monad.Except
import Development.IDE.Types.Options (IdeOptions(..))
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.OfInterest
import Development.IDE.Types.Logger
import           Development.Shake                        hiding (Diagnostic, Env, newCache)
import qualified Language.Haskell.LSP.Messages as LSP

import           Development.IDE.Core.Shake



newtype GlobalIdeOptions = GlobalIdeOptions IdeOptions
instance IsIdeGlobal GlobalIdeOptions

------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Rules ()
           -> (LSP.FromServerMessage -> IO ())
           -> Logger
           -> IdeOptions
           -> VFSHandle
           -> IO IdeState
initialise mainRule toDiags logger options vfs =
    shakeOpen
        toDiags
        logger
        (setProfiling options $
        shakeOptions { shakeThreads = optThreads options
                     , shakeFiles   = "/dev/null"
                     }) $ do
            addIdeGlobal $ GlobalIdeOptions options
            fileStoreRules vfs
            ofInterestRules
            mainRule

writeProfile :: IdeState -> FilePath -> IO ()
writeProfile = shakeProfile

setProfiling :: IdeOptions -> ShakeOptions -> ShakeOptions
setProfiling opts shakeOpts =
  maybe shakeOpts (\p -> shakeOpts { shakeReport = [p], shakeTimings = True }) (optShakeProfiling opts)

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- | Run a single action using the supplied service. See `runActions`
-- for more details.
runAction :: IdeState -> Action a -> IO a
runAction service action = head <$> runActions service [action]

-- | Run a list of actions in parallel using the supplied service.
-- This will return as soon as the results of the actions are
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runActions :: IdeState -> [Action a] -> IO [a]
runActions x acts = do
    var <- newBarrier
    _ <- shakeRun x acts (signalBarrier var)
    waitBarrier var

-- | This is a synchronous variant of `runAction`. See
-- `runActionsSync` of more details.
runActionSync :: IdeState -> Action a -> IO a
runActionSync s a = head <$> runActionsSync s [a]

-- | `runActionsSync` is similar to `runActions` but it will
-- wait for all rules (so in particular the `ofInterestRule`) to
-- finish running. This is mainly useful in tests, where you want
-- to wait for all rules to fire so you can check diagnostics.
runActionsSync :: IdeState -> [Action a] -> IO [a]
runActionsSync s acts = join $ shakeRun s acts (const $ pure ())

getIdeOptions :: Action IdeOptions
getIdeOptions = do
    GlobalIdeOptions x <- getIdeGlobalAction
    return x
