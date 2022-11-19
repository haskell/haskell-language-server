-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           Arguments                                (Arguments (..),
                                                           getArguments)
import           Control.Monad.Extra                      (unless)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Default                             (def)
import           Data.Function                            ((&))
import           Data.Version                             (showVersion)
import           Development.GitRev                       (gitHash)
import           Development.IDE                          (action)
import           Development.IDE.Core.OfInterest          (kick)
import           Development.IDE.Core.Rules               (mainRule)
import qualified Development.IDE.Core.Rules               as Rules
import           Development.IDE.Core.Tracing             (withTelemetryLogger)
import qualified Development.IDE.Main                     as IDEMain
import qualified Development.IDE.Monitoring.EKG           as EKG
import qualified Development.IDE.Monitoring.OpenTelemetry as OpenTelemetry
import qualified Development.IDE.Plugin.HLS.GhcIde        as GhcIde
import           Development.IDE.Types.Logger             (Logger (Logger),
                                                           LoggingColumn (DataColumn, PriorityColumn),
                                                           Pretty (pretty),
                                                           Priority (Debug, Error, Info),
                                                           WithPriority (WithPriority, priority),
                                                           cfilter,
                                                           cmapWithPrio,
                                                           defaultLayoutOptions,
                                                           layoutPretty,
                                                           makeDefaultStderrRecorder,
                                                           renderStrict)
import qualified Development.IDE.Types.Logger             as Logger
import           Development.IDE.Types.Options
import           GHC.Stack                                (emptyCallStack)
import           Ide.Plugin.Config                        (Config (checkParents, checkProject))
import           Ide.PluginUtils                          (pluginDescToIdePlugins)
import           Ide.Types                                (PluginDescriptor (pluginNotificationHandlers),
                                                           defaultPluginDescriptor,
                                                           mkPluginNotificationHandler)
import           Language.LSP.Server                      as LSP
import           Language.LSP.Types                       as LSP
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
main = withTelemetryLogger $ \telemetryLogger -> do
    -- stderr recorder just for plugin cli commands
    pluginCliRecorder <-
      cmapWithPrio pretty
      <$> makeDefaultStderrRecorder (Just [PriorityColumn, DataColumn]) Info

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

    docWithPriorityRecorder <- makeDefaultStderrRecorder (Just [PriorityColumn, DataColumn]) minPriority

    (lspLogRecorder, cb1) <- Logger.withBacklog Logger.lspClientLogRecorder
    (lspMessageRecorder, cb2) <- Logger.withBacklog Logger.lspClientMessageRecorder
    -- This plugin just installs a handler for the `initialized` notification, which then
    -- picks up the LSP environment and feeds it to our recorders
    let lspRecorderPlugin = (defaultPluginDescriptor "LSPRecorderCallback")
          { pluginNotificationHandlers = mkPluginNotificationHandler LSP.SInitialized $ \_ _ _ _ -> do
              env <- LSP.getLspEnv
              liftIO $ (cb1 <> cb2) env
          }

    let docWithFilteredPriorityRecorder =
          (docWithPriorityRecorder & cfilter (\WithPriority{ priority } -> priority >= minPriority)) <>
          (lspLogRecorder & cmapWithPrio (renderStrict . layoutPretty defaultLayoutOptions)
                          & cfilter (\WithPriority{ priority } -> priority >= minPriority)) <>
          (lspMessageRecorder & cmapWithPrio (renderStrict . layoutPretty defaultLayoutOptions)
                              & cfilter (\WithPriority{ priority } -> priority >= Error))

    -- exists so old-style logging works. intended to be phased out
    let logger = Logger $ \p m -> Logger.logger_ docWithFilteredPriorityRecorder (WithPriority p emptyCallStack (pretty m))

    let recorder = docWithFilteredPriorityRecorder
                 & cmapWithPrio pretty

    let arguments =
          if argsTesting
          then IDEMain.testing (cmapWithPrio LogIDEMain recorder) logger hlsPlugins
          else IDEMain.defaultArguments (cmapWithPrio LogIDEMain recorder) logger hlsPlugins

    IDEMain.defaultMain (cmapWithPrio LogIDEMain recorder) arguments
        { IDEMain.argsProjectRoot = Just argsCwd
        , IDEMain.argCommand = argsCommand
        , IDEMain.argsLogger = IDEMain.argsLogger arguments <> pure telemetryLogger
        , IDEMain.argsHlsPlugins = IDEMain.argsHlsPlugins arguments <> pluginDescToIdePlugins [lspRecorderPlugin]

        , IDEMain.argsRules = do
            -- install the main and ghcide-plugin rules
            mainRule (cmapWithPrio LogRules recorder) def
            -- install the kick action, which triggers a typecheck on every
            -- Shake database restart, i.e. on every user edit.
            unless argsDisableKick $
                action kick

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
        , IDEMain.argsMonitoring = OpenTelemetry.monitoring <> EKG.monitoring logger argsMonitoringPort
        }
