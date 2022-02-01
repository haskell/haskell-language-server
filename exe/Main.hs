-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Function                ((&))
import           Development.IDE.Types.Logger (Priority (Debug, Info),
                                               WithPriority (WithPriority, priority),
                                               cfilter, cmap,
                                               makeDefaultStderrRecorder,
                                               priorityToHsLoggerPriority,
                                               withDefaultRecorder)
import           Ide.Arguments                (Arguments (..),
                                               GhcideArguments (..),
                                               getArguments)
import           Ide.Main                     (defaultMain)
import qualified Ide.Main                     as IdeMain
import qualified Plugins
import           Prettyprinter                (Doc, Pretty (pretty))

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log
  deriving Show

instance Pretty Log where
  pretty log = case log of
    LogIdeMain ideMainLog -> pretty ideMainLog
    LogPlugins pluginsLog -> pretty pluginsLog

logToPriority :: Log -> Priority
logToPriority = \case
  LogIdeMain log -> IdeMain.logToPriority log
  LogPlugins log -> Plugins.logToPriority log

logToDocWithPriority :: Log -> WithPriority (Doc a)
logToDocWithPriority log = WithPriority (logToPriority log) (pretty log)

main :: IO ()
main = do
    -- plugin cli commands use stderr logger for now unless we change the args
    -- parser to get logging arguments first or do more complicated things
    stderrRecorder <- cmap logToDocWithPriority <$> makeDefaultStderrRecorder (priorityToHsLoggerPriority Info)
    args <- getArguments "haskell-language-server" (Plugins.idePlugins (cmap LogPlugins stderrRecorder) False)

    let (minPriority, logFilePath, includeExamplePlugins) =
          case args of
            Ghcide GhcideArguments{ argsTesting, argsDebugOn, argsLogFile, argsExamplePlugin } ->
              let minPriority = if argsDebugOn || argsTesting then Debug else Info
              in (minPriority, argsLogFile, argsExamplePlugin)
            _ -> (Info, Nothing, False)
    let hsLoggerMinPriority = priorityToHsLoggerPriority minPriority

    withDefaultRecorder logFilePath hsLoggerMinPriority $ \textWithPriorityRecorder -> do
      let recorder =
            textWithPriorityRecorder
            & cfilter (\WithPriority{ priority } -> priority >= minPriority)
            & cmap logToDocWithPriority

      defaultMain (cmap LogIdeMain recorder) args (Plugins.idePlugins (cmap LogPlugins recorder) includeExamplePlugins)
