-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Function                ((&))
import           Development.IDE.Types.Logger (Priority (Debug, Info, Error),
                                               WithPriority (WithPriority, priority),
                                               cfilter, cmapWithPrio,
                                               makeDefaultStderrRecorder,
                                               withDefaultRecorder, renderStrict, layoutPretty, defaultLayoutOptions, Doc)
import           Ide.Arguments                (Arguments (..),
                                               GhcideArguments (..),
                                               getArguments)
import           Ide.Main                     (defaultMain)
import qualified Ide.Main                     as IdeMain
import qualified Plugins
import           Prettyprinter                (Pretty (pretty))
import Development.IDE.Plugin.LSPWindowShowMessageRecorder (makeLspShowMessageRecorder)
import Data.Text (Text)
import Ide.PluginUtils (pluginDescToIdePlugins)

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log

instance Pretty Log where
  pretty log = case log of
    LogIdeMain ideMainLog -> pretty ideMainLog
    LogPlugins pluginsLog -> pretty pluginsLog

main :: IO ()
main = do
    -- plugin cli commands use stderr logger for now unless we change the args
    -- parser to get logging arguments first or do more complicated things
    pluginCliRecorder <- cmapWithPrio pretty <$> makeDefaultStderrRecorder Nothing Info
    args <- getArguments "haskell-language-server" (Plugins.idePlugins (cmapWithPrio LogPlugins pluginCliRecorder) False)
    (lspRecorder, lspRecorderPlugin) <- makeLspShowMessageRecorder

    let (minPriority, logFilePath, includeExamplePlugins) =
          case args of
            Ghcide GhcideArguments{ argsTesting, argsDebugOn, argsLogFile, argsExamplePlugin } ->
              let minPriority = if argsDebugOn || argsTesting then Debug else Info
              in (minPriority, argsLogFile, argsExamplePlugin)
            _ -> (Info, Nothing, False)

    withDefaultRecorder logFilePath Nothing minPriority $ \textWithPriorityRecorder -> do
      let
        recorder = cmapWithPrio pretty $ mconcat
            [textWithPriorityRecorder
                & cfilter (\WithPriority{ priority } -> priority >= minPriority)
            , lspRecorder
                & cfilter (\WithPriority{ priority } -> priority >= Error)
                & cmapWithPrio renderDoc
            ]
        plugins = Plugins.idePlugins (cmapWithPrio LogPlugins recorder) includeExamplePlugins

      defaultMain (cmapWithPrio LogIdeMain recorder) args (pluginDescToIdePlugins [lspRecorderPlugin] <> plugins)

renderDoc :: Doc a -> Text
renderDoc = renderStrict . layoutPretty defaultLayoutOptions
