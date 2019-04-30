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
    setFilesOfInterest,
    writeProfile,
    getDiagnostics, unsafeClearDiagnostics,
    logDebug, logInfo, logWarning, logError
    ) where

import           Control.Concurrent.Extra
import           Control.Monad.Except
import Development.IDE.Functions.Compile (CompileOpts(..))
import           Development.IDE.State.FileStore
import qualified Development.IDE.Logger as Logger
import Data.Maybe
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import qualified Data.Text as T
import           Development.IDE.Functions.GHCError
import           Development.Shake                        hiding (Diagnostic, Env, newCache)
import           Development.IDE.Types.LSP as Compiler

import           "ghc-lib-parser" UniqSupply

import           Development.IDE.State.Shake


-- | Environment threaded through the Shake actions.
data Env = Env
    { envOptions       :: CompileOpts
      -- ^ Compiler options.
    , envOfInterestVar :: Var (Set FilePath)
      -- ^ The files of interest.
    , envUniqSupplyVar :: Var UniqSupply
      -- ^ The unique supply of names used by the compiler.
    }
instance IsIdeGlobal Env


mkEnv :: CompileOpts -> IO Env
mkEnv options = do
    ofInterestVar <- newVar Set.empty
    uniqSupplyVar <- mkSplitUniqSupply 'a' >>= newVar
    return Env
        { envOptions       = options
        , envOfInterestVar = ofInterestVar
        , envUniqSupplyVar = uniqSupplyVar
        }

getDiagnostics :: IdeState -> IO [Diagnostic]
getDiagnostics = getAllDiagnostics

unsafeClearDiagnostics :: IdeState -> IO ()
unsafeClearDiagnostics = unsafeClearAllDiagnostics


------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Rules ()
           -> Maybe (Event -> IO ())
           -> Logger.Handle IO
           -> CompileOpts
           -> IO IdeState
initialise mainRule toDiags logger options =
    shakeOpen
        (fromMaybe (const $ pure ()) toDiags)
        logger
        (setProfiling options $
        shakeOptions { shakeThreads = optThreads options
                     , shakeFiles   = "/dev/null"
                     }) $ do
            addIdeGlobal =<< liftIO (mkEnv options)
            fileStoreRules
            mainRule

writeProfile :: IdeState -> FilePath -> IO ()
writeProfile = shakeProfile

setProfiling :: CompileOpts -> ShakeOptions -> ShakeOptions
setProfiling opts shakeOpts =
  maybe shakeOpts (\p -> shakeOpts { shakeReport = [p], shakeTimings = True }) (optShakeProfiling opts)

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- | Run a single action using the supplied service.
runAction :: IdeState -> Action a -> IO a
runAction service action = head <$> runActions service [action]

-- | Run a list of actions in parallel using the supplied service.
runActions :: IdeState -> [Action a] -> IO [a]
runActions x = join . shakeRun x


-- | Set the files-of-interest which will be built and kept-up-to-date.
setFilesOfInterest :: IdeState -> Set FilePath -> IO ()
setFilesOfInterest state files = do
    Env{..} <- getIdeGlobalState state
    -- update vars synchronously
    modifyVar_ envOfInterestVar $ const $ return files

    -- run shake to update results regarding the files of interest
    void $ shakeRun state []

getServiceEnv :: Action Env
getServiceEnv = getIdeGlobalAction

logDebug, logInfo, logWarning, logError :: IdeState -> T.Text -> IO ()
logDebug = shakeLogDebug
logInfo = shakeLogInfo
logWarning = shakeLogWarning
logError = shakeLogError
