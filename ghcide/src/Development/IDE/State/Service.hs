-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.State.Service(
    Env(..),
    getServiceEnv,
    IdeState, initialise, shutdown,
    runAction, runActions,
    runActionSync, runActionsSync,
    setFilesOfInterest, modifyFilesOfInterest,
    writeProfile,
    getDiagnostics, unsafeClearDiagnostics,
    logDebug, logSeriousError
    ) where

import           Control.Concurrent.Extra
import           Control.Monad.Except
import Development.IDE.Types.Options (IdeOptions(..))
import           Development.IDE.State.FileStore
import qualified Development.IDE.Logger as Logger
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import qualified Data.Text as T
import Data.Tuple.Extra
import           Development.IDE.Functions.GHCError
import Development.IDE.Types.Diagnostics (NormalizedFilePath)
import           Development.Shake                        hiding (Diagnostic, Env, newCache)
import qualified Language.Haskell.LSP.Messages as LSP

import           UniqSupply

import           Development.IDE.State.Shake


-- | Environment threaded through the Shake actions.
data Env = Env
    { envOptions       :: IdeOptions
      -- ^ Compiler options.
    , envOfInterestVar :: Var (Set NormalizedFilePath)
      -- ^ The files of interest.
    , envUniqSupplyVar :: Var UniqSupply
      -- ^ The unique supply of names used by the compiler.
    }
instance IsIdeGlobal Env


mkEnv :: IdeOptions -> IO Env
mkEnv options = do
    ofInterestVar <- newVar Set.empty
    uniqSupplyVar <- mkSplitUniqSupply 'a' >>= newVar
    return Env
        { envOptions       = options
        , envOfInterestVar = ofInterestVar
        , envUniqSupplyVar = uniqSupplyVar
        }

getDiagnostics :: IdeState -> IO [FileDiagnostic]
getDiagnostics = getAllDiagnostics

unsafeClearDiagnostics :: IdeState -> IO ()
unsafeClearDiagnostics = unsafeClearAllDiagnostics


------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Rules ()
           -> (LSP.FromServerMessage -> IO ())
           -> Logger.Handle
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
            addIdeGlobal =<< liftIO (mkEnv options)
            fileStoreRules vfs
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

-- | Set the files-of-interest which will be built and kept-up-to-date.
setFilesOfInterest :: IdeState -> Set NormalizedFilePath -> IO ()
setFilesOfInterest state files = modifyFilesOfInterest state (const files)

modifyFilesOfInterest :: IdeState -> (Set NormalizedFilePath -> Set NormalizedFilePath) -> IO ()
modifyFilesOfInterest state f = do
    Env{..} <- getIdeGlobalState state
    files <- modifyVar envOfInterestVar $ pure . dupe . f
    logDebug state $ "Set files of interest to: " <> T.pack (show $ Set.toList files)
    void $ shakeRun state [] (const $ pure ())

getServiceEnv :: Action Env
getServiceEnv = getIdeGlobalAction
