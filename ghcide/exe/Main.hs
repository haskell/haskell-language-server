-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           Arguments                                (Arguments (..),
                                                           getArguments)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Default                             (def)
import           Data.Function                            ((&))
import           Data.Version                             (showVersion)
import           Development.GitRev                       (gitHash)
import           Development.IDE.Core.Rules               (mainRule)
import qualified Development.IDE.Core.Rules               as Rules
import           Development.IDE.Core.Tracing             (withTelemetryRecorder)
import qualified Development.IDE.Main                     as IDEMain
import qualified Development.IDE.Monitoring.OpenTelemetry as OpenTelemetry
import qualified Development.IDE.Plugin.HLS.GhcIde        as GhcIde
import           Development.IDE.Types.Options
import           Ide.Logger                               (LoggingColumn (..),
                                                           Pretty (pretty),
                                                           Priority (Debug, Error, Info),
                                                           WithPriority (WithPriority, priority),
                                                           cfilter,
                                                           cmapWithPrio,
                                                           defaultLayoutOptions,
                                                           layoutPretty,
                                                           makeDefaultStderrRecorder,
                                                           renderStrict)
import qualified Ide.Logger                               as Logger
import           Ide.Plugin.Config                        (Config (checkParents, checkProject))
import           Ide.PluginUtils                          (pluginDescToIdePlugins)
import           Ide.Types                                (PluginDescriptor (pluginNotificationHandlers),
                                                           defaultPluginDescriptor,
                                                           mkPluginNotificationHandler)
import           Language.LSP.Protocol.Message            as LSP
import           Language.LSP.Server                      as LSP
import           Paths_ghcide                             (version)
import qualified System.Directory.Extra                   as IO
import           System.Environment                       (getExecutablePath)
import           System.Exit                              (exitSuccess)
import           System.Info                              (compilerVersion)
import           System.IO                                (hPutStrLn, stderr)

data Log
  = LogIDEMain IDEMain.Log
  | LogRules Rules.Log
  | LogGhcIde GhcIde.Log

instance Pretty Log where
  pretty = \case
    LogIDEMain log -> pretty log
    LogRules log   -> pretty log
    LogGhcIde log  -> pretty log

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x                  -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> showVersion compilerVersion
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

main :: IO ()
main = withTelemetryRecorder $ \telemetryRecorder -> do
    -- stderr recorder just for plugin cli commands
    pluginCliRecorder <-
      cmapWithPrio pretty
      <$> makeDefaultStderrRecorder (Just [ThreadIdColumn, PriorityColumn, DataColumn])

    let hlsPlugins = pluginDescToIdePlugins (GhcIde.descriptors (cmapWithPrio LogGhcIde pluginCliRecorder))
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments hlsPlugins

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- if user uses --cwd option we need to make this path absolute (and set the current directory to it)
    argsCwd <- case argsCwd of
      Nothing   -> IO.getCurrentDirectory
      Just root -> IO.setCurrentDirectory root >> IO.getCurrentDirectory

    let minPriority = if argsVerbose then Debug else Info

    docWithPriorityRecorder <- makeDefaultStderrRecorder (Just [PriorityColumn, DataColumn])

    (lspLogRecorder, cb1) <- Logger.withBacklog Logger.lspClientLogRecorder
    (lspMessageRecorder, cb2) <- Logger.withBacklog Logger.lspClientMessageRecorder
    -- This plugin just installs a handler for the `initialized` notification, which then
    -- picks up the LSP environment and feeds it to our recorders
    let lspRecorderPlugin = (defaultPluginDescriptor "LSPRecorderCallback" "Internal plugin")
          { pluginNotificationHandlers = mkPluginNotificationHandler LSP.SMethod_Initialized $ \_ _ _ _ -> do
              env <- LSP.getLspEnv
              liftIO $ (cb1 <> cb2) env
          }

    let docWithFilteredPriorityRecorder =
          (docWithPriorityRecorder & cfilter (\WithPriority{ priority } -> priority >= minPriority)) <>
          (lspLogRecorder & cmapWithPrio (renderStrict . layoutPretty defaultLayoutOptions)
                          & cfilter (\WithPriority{ priority } -> priority >= minPriority)) <>
          (lspMessageRecorder & cmapWithPrio (renderStrict . layoutPretty defaultLayoutOptions)
                              & cfilter (\WithPriority{ priority } -> priority >= Error)) <>
          telemetryRecorder

    let recorder = docWithFilteredPriorityRecorder
                 & cmapWithPrio pretty

    let arguments =
          if argsTesting
          then IDEMain.testing (cmapWithPrio LogIDEMain recorder) argsCwd hlsPlugins
          else IDEMain.defaultArguments (cmapWithPrio LogIDEMain recorder) argsCwd hlsPlugins

    IDEMain.defaultMain (cmapWithPrio LogIDEMain recorder) arguments
        { IDEMain.argsProjectRoot = argsCwd
        , IDEMain.argCommand = argsCommand
        , IDEMain.argsHlsPlugins = IDEMain.argsHlsPlugins arguments <> pluginDescToIdePlugins [lspRecorderPlugin]

        , IDEMain.argsRules = do
            mainRule (cmapWithPrio LogRules recorder) def

        , IDEMain.argsThreads = case argsThreads of 0 -> Nothing ; i -> Just (fromIntegral i)

        , IDEMain.argsIdeOptions = \config sessionLoader ->
            let defOptions = IDEMain.argsIdeOptions arguments config sessionLoader
            in defOptions
                { optShakeProfiling = argsShakeProfiling
                , optCheckParents = pure $ checkParents config
                , optCheckProject = pure $ checkProject config
                , optRunSubset = not argsConservativeChangeTracking
                , optVerifyCoreFile = argsVerifyCoreFile
                }
        , IDEMain.argsMonitoring = OpenTelemetry.monitoring
        }
