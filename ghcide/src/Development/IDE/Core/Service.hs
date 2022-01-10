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
    getDiagnostics,
    ideLogger,
    updatePositionMapping,
    Log
    ) where

import           Control.Applicative             ((<|>))
import           Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileExists (fileExistsRules)
import           Development.IDE.Core.OfInterest hiding (Log)
import           Development.IDE.Graph
import           Development.IDE.Types.Logger    as Logger
import           Development.IDE.Types.Options   (IdeOptions (..))
import           Ide.Plugin.Config
import qualified Language.LSP.Server             as LSP
import qualified Language.LSP.Types              as LSP

import           Control.Monad
import qualified Development.IDE.Core.FileExists as FileExists
import qualified Development.IDE.Core.OfInterest as OfInterest
import           Development.IDE.Core.Shake      hiding (Log)
import qualified Development.IDE.Core.Shake      as Shake
import           Development.IDE.Types.Shake     (WithHieDb)
import           Prettyprinter                   (Pretty (pretty))
import           System.Environment              (lookupEnv)


data Log
  = LogShake Shake.Log
  | LogOfInterest OfInterest.Log
  | LogFileExists FileExists.Log
  deriving Show

instance Pretty Log where
  pretty log = case log of
    LogShake shakeLog           -> pretty shakeLog
    LogOfInterest ofInterestLog -> pretty ofInterestLog
    LogFileExists fileExistsLog -> pretty fileExistsLog


------------------------------------------------------------
-- Exposed API

-- | Initialise the Compiler Service.
initialise :: Recorder Log
           -> Config
           -> Rules ()
           -> Maybe (LSP.LanguageContextEnv Config)
           -> Logger
           -> Debouncer LSP.NormalizedUri
           -> IdeOptions
           -> VFSHandle
           -> WithHieDb
           -> IndexQueue
           -> IO IdeState
initialise recorder defaultConfig mainRule lspEnv logger debouncer options vfs withHieDb hiedbChan = do
    shakeProfiling <- do
        let fromConf = optShakeProfiling options
        fromEnv <- lookupEnv "GHCIDE_BUILD_PROFILING"
        return $ fromConf <|> fromEnv
    shakeOpen
        (cmap LogShake recorder)
        lspEnv
        defaultConfig
        logger
        debouncer
        shakeProfiling
        (optReportProgress options)
        (optTesting options)
        withHieDb
        hiedbChan
        vfs
        (optShakeOptions options)
          $ do
            addIdeGlobal $ GlobalIdeOptions options
            ofInterestRules (cmap LogOfInterest recorder)
            fileExistsRules (cmap LogFileExists recorder) lspEnv vfs
            mainRule

-- | Shutdown the Compiler Service.
shutdown :: IdeState -> IO ()
shutdown = shakeShut

-- This will return as soon as the result of the action is
-- available.  There might still be other rules running at this point,
-- e.g., the ofInterestRule.
runAction :: String -> IdeState -> Action a -> IO a
runAction herald ide act =
  join $ shakeEnqueue (shakeExtras ide) (mkDelayedAction herald Logger.Info act)
