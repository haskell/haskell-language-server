-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Service(
    getIdeOptions, getIdeOptionsIO,
    IdeState, initialise, shutdown,
    runAction,
    getDiagnostics,
    ideLogger,
    updatePositionMapping,
    Log(..),
    ) where

import           Control.Applicative              ((<|>))
import           Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileExists  (fileExistsRules)
import           Development.IDE.Core.OfInterest  hiding (Log, LogShake)
import           Development.IDE.Graph
import           Development.IDE.Types.Options    (IdeOptions (..))
import           Ide.Logger                       as Logger (Pretty (pretty),
                                                             Priority (Debug),
                                                             Recorder,
                                                             WithPriority,
                                                             cmapWithPrio)
import           Ide.Plugin.Config
import qualified Language.LSP.Protocol.Types      as LSP
import qualified Language.LSP.Server              as LSP

import           Control.Monad
import qualified Development.IDE.Core.FileExists  as FileExists
import qualified Development.IDE.Core.OfInterest  as OfInterest
import           Development.IDE.Core.Shake       hiding (Log)
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.Types.Monitoring (Monitoring)
import           Development.IDE.Types.Shake      (WithHieDb)
import           Ide.Types                        (IdePlugins)
import           System.Environment               (lookupEnv)

data Log
  = LogShake Shake.Log
  | LogOfInterest OfInterest.Log
  | LogFileExists FileExists.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake msg      -> pretty msg
    LogOfInterest msg -> pretty msg
    LogFileExists msg -> pretty msg

------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Recorder (WithPriority Log)
           -> Config
           -> IdePlugins IdeState
           -> Rules ()
           -> Maybe (LSP.LanguageContextEnv Config)
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> WithHieDb
           -> IndexQueue
           -> Monitoring
           -> FilePath
           -> IO IdeState
initialise recorder defaultConfig plugins mainRule lspEnv debouncer options withHieDb hiedbChan metrics rootDir = do
    shakeProfiling <- do
        let fromConf = optShakeProfiling options
        fromEnv <- lookupEnv "GHCIDE_BUILD_PROFILING"
        return $ fromConf <|> fromEnv
    shakeOpen
        (cmapWithPrio LogShake recorder)
        lspEnv
        defaultConfig
        plugins
        debouncer
        shakeProfiling
        (optReportProgress options)
        (optTesting options)
        withHieDb
        hiedbChan
        (optShakeOptions options)
        metrics
        (do
            addIdeGlobal $ GlobalIdeOptions options
            ofInterestRules (cmapWithPrio LogOfInterest recorder)
            fileExistsRules (cmapWithPrio LogFileExists recorder) lspEnv
            mainRule)
        rootDir

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- This will return as soon as the result of the action is
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runAction :: String -> IdeState -> Action a -> IO a
runAction herald ide act =
  join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Debug act)
