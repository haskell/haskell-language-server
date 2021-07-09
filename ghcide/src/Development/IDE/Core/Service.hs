-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

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

import           Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileExists (fileExistsRules)
import           Development.IDE.Core.OfInterest
import           Development.IDE.Graph
import           Development.IDE.Types.Logger    as Logger
import           Development.IDE.Types.Options   (IdeOptions (..))
import           Ide.Plugin.Config
import qualified Language.LSP.Server             as LSP
import qualified Language.LSP.Types              as LSP

import           Control.Monad
import           Development.IDE.Core.Shake


------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Config
           -> Rules ()
           -> Maybe (LSP.LanguageContextEnv Config)
           -> Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> VFSHandle
           -> HieDb
           -> IndexQueue
           -> IO IdeState
initialise defaultConfig mainRule lspEnv logger debouncer options vfs hiedb hiedbChan =
    shakeOpen
        lspEnv
        defaultConfig
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
