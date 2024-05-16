-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Exception             (displayException)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bifunctor                (first)
import           Data.Function                 ((&))
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (catMaybes)
import           Data.Text                     (Text)
import qualified HlsPlugins                    as Plugins
import           Ide.Arguments                 (Arguments (..),
                                                GhcideArguments (..),
                                                getArguments)
import           Ide.Logger                    (Doc, Priority (Error, Info),
                                                Recorder,
                                                WithPriority (WithPriority, priority),
                                                cfilter, cmapWithPrio,
                                                defaultLayoutOptions,
                                                layoutPretty, logWith,
                                                makeDefaultStderrRecorder,
                                                renderStrict, withFileRecorder)
import qualified Ide.Logger                    as Logger
import           Ide.Main                      (defaultMain)
import qualified Ide.Main                      as IdeMain
import           Ide.PluginUtils               (pluginDescToIdePlugins)
import           Ide.Types                     (PluginDescriptor (pluginNotificationHandlers),
                                                defaultPluginDescriptor,
                                                mkPluginNotificationHandler)
import           Language.LSP.Protocol.Message as LSP
import           Language.LSP.Server           as LSP
import           Prettyprinter                 (Pretty (pretty), vcat, vsep)

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log

instance Pretty Log where
  pretty log = case log of
    LogIdeMain ideMainLog -> pretty ideMainLog
    LogPlugins pluginsLog -> pretty pluginsLog

main :: IO ()
main = do
    stderrRecorder <- makeDefaultStderrRecorder Nothing
    -- plugin cli commands use stderr logger for now unless we change the args
    -- parser to get logging arguments first or do more complicated things
    let pluginCliRecorder = cmapWithPrio pretty stderrRecorder
    args <- getArguments "haskell-language-server" (Plugins.idePlugins (cmapWithPrio LogPlugins pluginCliRecorder))

    -- Recorder that logs to the LSP client with logMessage
    (lspLogRecorder, cb1) <-
        Logger.withBacklog Logger.lspClientLogRecorder
        <&> first (cmapWithPrio renderDoc)
    -- Recorder that logs to the LSP client with showMessage
    (lspMessageRecorder, cb2) <-
        Logger.withBacklog Logger.lspClientMessageRecorder
        <&> first (cmapWithPrio renderDoc)
    -- Recorder that logs Error severity logs to the client with showMessage and some extra text
    let lspErrorMessageRecorder = lspMessageRecorder
            & cfilter (\WithPriority{ priority } -> priority >= Error)
            & cmapWithPrio (\msg -> vsep
                ["Error condition, please check your setup and/or the [issue tracker](" <> issueTrackerUrl <> "): "
                , msg
                ])
    -- This plugin just installs a handler for the `initialized` notification, which then
    -- picks up the LSP environment and feeds it to our recorders
    let lspRecorderPlugin = (defaultPluginDescriptor "LSPRecorderCallback" "Internal plugin")
          { pluginNotificationHandlers = mkPluginNotificationHandler LSP.SMethod_Initialized $ \_ _ _ _ -> do
              env <- LSP.getLspEnv
              liftIO $ (cb1 <> cb2) env
          }

    let (minPriority, logFilePath, logStderr, logClient) =
          case args of
            Ghcide GhcideArguments{ argsLogLevel, argsLogFile, argsLogStderr, argsLogClient} ->
              (argsLogLevel, argsLogFile, argsLogStderr, argsLogClient)
            _ -> (Info, Nothing, True, False)

    -- Adapter for withFileRecorder to handle the case where we don't want to log to a file
    let withLogFileRecorder action = case logFilePath of
            Just p -> withFileRecorder p Nothing $ \case
                Left e -> do
                    let exceptionMessage = pretty $ displayException e
                    let message = vcat [exceptionMessage, "Couldn't open log file; not logging to it."]
                    logWith stderrRecorder Error message
                    action Nothing
                Right r -> action (Just r)
            Nothing -> action Nothing

    withLogFileRecorder $ \logFileRecorder -> do
      let
        lfr = logFileRecorder
        ser = if logStderr then Just stderrRecorder else Nothing
        lemr = Just lspErrorMessageRecorder
        llr = if logClient then Just lspLogRecorder else Nothing
        recorder :: Recorder (WithPriority Log) =
            [lfr, ser, lemr, llr]
              & catMaybes
              & mconcat
              & cmapWithPrio pretty
              & cfilter (\WithPriority{ priority } -> priority >= minPriority)
        plugins = Plugins.idePlugins (cmapWithPrio LogPlugins recorder)

      defaultMain
        (cmapWithPrio LogIdeMain recorder)
        args
        (plugins <> pluginDescToIdePlugins [lspRecorderPlugin])

renderDoc :: Doc a -> Text
renderDoc d = renderStrict $ layoutPretty defaultLayoutOptions d

issueTrackerUrl :: Doc a
issueTrackerUrl = "https://github.com/haskell/haskell-language-server/issues"
