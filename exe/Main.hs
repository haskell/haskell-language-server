-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import           Control.Exception             (displayException)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bifunctor                (first)
import           Data.Function                 ((&))
import           Data.Functor                  ((<&>))
import           Data.Maybe                    (catMaybes)
import           Data.Text                     (Text)
import           Development.IDE.Types.Logger  (Doc, Priority (Error, Info),
                                                Recorder,
                                                WithPriority (WithPriority, priority),
                                                cfilter, cmapWithPrio,
                                                defaultLayoutOptions,
                                                layoutPretty, logWith,
                                                makeDefaultStderrRecorder,
                                                renderStrict, withFileRecorder)
import qualified Development.IDE.Types.Logger  as Logger
import qualified HlsPlugins                    as Plugins
import           Ide.Arguments                 (Arguments (..),
                                                GhcideArguments (..),
                                                getArguments)
import           Ide.Main                      (defaultMain)
import qualified Ide.Main                      as IdeMain
import           Ide.PluginUtils               (pluginDescToIdePlugins)
import           Ide.Types                     (PluginDescriptor (pluginNotificationHandlers),
                                                defaultPluginDescriptor,
                                                mkPluginNotificationHandler)
import           Language.LSP.Protocol.Message as LSP
import           Language.LSP.Server           as LSP
import           Prettyprinter                 (Pretty (pretty), vcat, vsep)

import Data.Time
import GHC.Debug.Stub
import qualified GHC.Debug.Client as Client
import qualified GHC.Debug.Snapshot as Client
import GHC.Debug.Convention (socketDirectory, snapshotDirectory)
import Data.Maybe
import System.Environment
import System.Posix.Process
import System.FilePath
import System.Exit
import Control.Concurrent

data Log
  = LogIdeMain IdeMain.Log
  | LogPlugins Plugins.Log

instance Pretty Log where
  pretty log = case log of
    LogIdeMain ideMainLog -> pretty ideMainLog
    LogPlugins pluginsLog -> pretty pluginsLog

with_ghc_debug main = do
  defaultSocketPath <- getDefaultSocketPath
  socketPath <- fromMaybe defaultSocketPath <$> lookupEnv "GHC_DEBUG_SOCKET"
  withGhcDebugUnix socketPath (main socketPath)
  where
  getDefaultSocketPath = do
      socketOverride <- fromMaybe "" <$> lookupEnv "GHC_DEBUG_SOCKET"
      if not (null socketOverride)
      then return socketOverride
      else do
          dir <- socketDirectory
          name <- getProgName
          pid <- show <$> getProcessID
          let socketName = pid ++ "-" ++ name
          return (dir </> socketName)

takeSnapshot :: FilePath -> LanguageContextEnv config -> IO () -> IO ()
takeSnapshot socket env snapshot = do
  snapshotDir <- snapshotDirectory
  snapshotPath <- formatTime defaultTimeLocale (snapshotDir </> takeFileName socket ++ "-snapshot-%Y-%m-%d-%H%M%S") <$> getZonedTime
  pid <- forkProcess $ do
    Client.withDebuggeeConnect socket $ \debugee -> do
      Client.fork debugee
      Client.makeSnapshot debugee snapshotPath
      Client.resume debugee
    exitWith ExitSuccess
  void $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ "Taking snapshot..."
  void $ forkIO $ do
    getProcessStatus True True pid >>= \case
      Just (Exited ExitSuccess) -> do
        void $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $ ShowMessageParams MtInfo $ "Saved snapshot to " <> T.pack snapshotPath
      status -> do
        void $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $ ShowMessageParams MtError $ "Snapshot process exited with " <> T.pack (show status)
    snapshot

main :: IO ()
<<<<<<< HEAD
main = do
    stderrRecorder <- makeDefaultStderrRecorder Nothing
||||||| parent of 4fd8b9134 (ghc-debug)
main = do
=======
main = with_ghc_debug $ \socket -> do
>>>>>>> 4fd8b9134 (ghc-debug)
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
    let lspRecorderPlugin = (defaultPluginDescriptor "LSPRecorderCallback")
          { pluginNotificationHandlers = mkPluginNotificationHandler LSP.SMethod_Initialized $ \_ _ _ _ -> do
              env <- LSP.getLspEnv
              liftIO $ (cb1 <> cb2) env

              let snapshot = void
                           $ LSP.sendRequest SWindowShowMessageRequest (ShowMessageRequestParams MtInfo "Take Snapshot?" (Just [MessageActionItem "yes"]))
                           $ \case
                              Right (Just (MessageActionItem "yes")) -> liftIO (takeSnapshot socket env (LSP.runLspT env snapshot))
                              _ -> snapshot
              snapshot

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
