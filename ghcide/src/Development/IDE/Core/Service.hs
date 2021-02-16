-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Service(
    getIdeOptions, getIdeOptionsIO,
    IdeState, initialise, shutdown,
    runAction,
    writeProfile,
    getDiagnostics,
    ideLogger,
    updatePositionMapping,
    ) where

import Development.IDE.Types.Options (IdeOptions(..))
import Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileStore  (fileStoreRules)
import           Development.IDE.Core.FileExists (fileExistsRules)
import           Development.IDE.Core.OfInterest
import Development.IDE.Types.Logger as Logger
import           Development.Shake
import qualified Language.LSP.Server as LSP
import qualified Language.LSP.Types as LSP
import           Ide.Plugin.Config

import           Development.IDE.Core.Shake
import Control.Monad


------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Rules ()
           -> Maybe (LSP.LanguageContextEnv Config)
           -> Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> VFSHandle
           -> HieDb
           -> IndexQueue
           -> IO IdeState
initialise mainRule lspEnv logger debouncer options vfs hiedb hiedbChan =
    shakeOpen
        lspEnv
        logger
        debouncer
        (optShakeProfiling options)
        (optReportProgress options)
        (optTesting options)
        hiedb
        hiedbChan
        vfs
        (optShakeOptions options)
          $ do
            addIdeGlobal $ GlobalIdeOptions options
            fileStoreRules vfs
            ofInterestRules
            fileExistsRules lspEnv vfs
            mainRule

writeProfile :: IdeState -> FilePath -> IO ()
writeProfile = shakeProfile

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- This will return as soon as the result of the action is
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runAction :: String -> IdeState -> Action a -> IO a
runAction herald ide act =
  join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Info act)
