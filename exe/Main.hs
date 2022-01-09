-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Function                ((&))
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Development.IDE.Types.Logger (Priority (Debug, Info),
                                               WithPriority (WithPriority, priority),
                                               cfilter, cmap, setupHsLogger,
                                               withDefaultTextWithPriorityRecorderAndHandle)
import           Ide.Arguments                (Arguments (..),
                                               GhcideArguments (..),
                                               getArguments)
import           Ide.Main                     (defaultMain)
import qualified Ide.Main                     as IdeMain
import qualified Plugins
import qualified System.Log                   as HsLogger

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log
  deriving Show

logToTextWithPriority :: Log -> WithPriority Text
logToTextWithPriority = WithPriority Info . Text.pack . show

main :: IO ()
main = do
    args <- getArguments "haskell-language-server" (Plugins.idePlugins mempty False)

    let (hsLoggerMinLogLevel, minPriority, logFilePath, includeExamplePlugins) =
          case args of
            Ghcide GhcideArguments{ argsTesting, argsDebugOn, argsLogFile, argsExamplePlugin } ->
              let (minHsLoggerLogLevel, minPriority) =
                    if argsDebugOn || argsTesting then (HsLogger.DEBUG, Debug) else (HsLogger.INFO, Info)
              in (minHsLoggerLogLevel, minPriority, argsLogFile, argsExamplePlugin)
            _ -> (HsLogger.INFO, Info, Nothing, False)

    withDefaultTextWithPriorityRecorderAndHandle logFilePath $ \textWithPriorityRecorder handle -> do
      -- until the contravariant logging system is fully in place
      setupHsLogger (Just handle) ["hls", "hie-bios"] hsLoggerMinLogLevel
      let recorder =
            textWithPriorityRecorder
            & cfilter (\WithPriority{ priority } -> priority >= minPriority)
            & cmap logToTextWithPriority

      defaultMain (cmap LogIdeMain recorder) args (Plugins.idePlugins (cmap LogPlugins recorder) includeExamplePlugins)
