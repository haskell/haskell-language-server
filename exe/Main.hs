-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Function                ((&))
import           Data.Text                    (Text)
import           Development.IDE.Types.Logger (Priority (Debug, Info),
                                               WithPriority (WithPriority, priority),
                                               cfilter, cmap, setupHsLogger,
                                               withDefaultTextWithPriorityRecorderAndHandle)
import qualified Development.IDE.Types.Logger as Logger
import           Ide.Arguments                (Arguments (..),
                                               GhcideArguments (..),
                                               getArguments)
import           Ide.Main                     (defaultMain)
import qualified Ide.Main                     as IdeMain
import qualified Plugins
import           Prettyprinter                (Pretty (pretty))
import qualified Prettyprinter
import qualified Prettyprinter.Render.Text    as Prettyprinter
import qualified System.Log                   as HsLogger

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log
  deriving Show

instance Pretty Log where
  pretty log = case log of
    LogIdeMain ideMainLog -> pretty ideMainLog
    LogPlugins pluginsLog -> pretty pluginsLog

logToPriority :: Log -> Logger.Priority
logToPriority = \case
  LogIdeMain log -> IdeMain.logToPriority log
  LogPlugins log -> Plugins.logToPriority log

logToTextWithPriority :: Log -> WithPriority Text
logToTextWithPriority log = WithPriority priority text
  where
    priority = logToPriority log
    text = log
         & pretty
         & Prettyprinter.layoutPretty Prettyprinter.defaultLayoutOptions
         & Prettyprinter.renderStrict

main :: IO ()
main = do
    args <- getArguments "haskell-language-server" (Plugins.idePlugins mempty False)

    let (hsLoggerMinLogLevel, minPriority, logFilePath, includeExamplePlugins) =
          case args of
            Ghcide GhcideArguments{ argsTesting, argsDebugOn, argsLogFile, argsExamplePlugin } ->
              let (minHsLoggerLogLevel, minPriority) =
                    if argsDebugOn || argsTesting then (HsLogger.DEBUG, Info) else (HsLogger.INFO, Info)
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
