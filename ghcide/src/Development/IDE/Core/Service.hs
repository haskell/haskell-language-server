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

import Data.Maybe
import Development.IDE.Types.Options (IdeOptions(..))
import Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileStore  (VFSHandle, fileStoreRules)
import           Development.IDE.Core.FileExists (fileExistsRules)
import           Development.IDE.Core.OfInterest
import Development.IDE.Types.Logger as Logger
import           Development.Shake
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import qualified Language.Haskell.LSP.Types.Capabilities as LSP

import           Development.IDE.Core.Shake
import Control.Monad



------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: LSP.ClientCapabilities
           -> Rules ()
           -> IO LSP.LspId
           -> (LSP.FromServerMessage -> IO ())
           -> WithProgressFunc
           -> WithIndefiniteProgressFunc
           -> Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> VFSHandle
           -> IO IdeState
initialise caps mainRule getLspId toDiags wProg wIndefProg logger debouncer options vfs =
    shakeOpen
        getLspId
        toDiags
        wProg
        wIndefProg
        caps
        logger
        debouncer
        (optShakeProfiling options)
        (optReportProgress options)
        (optTesting options)
        shakeOptions
          { shakeThreads = optThreads options
          , shakeFiles   = fromMaybe "/dev/null" (optShakeFiles options)
          } $ do
            addIdeGlobal $ GlobalIdeOptions options
            fileStoreRules vfs
            ofInterestRules
            fileExistsRules caps vfs
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
