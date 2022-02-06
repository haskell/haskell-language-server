-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           Arguments                         (Arguments (..),
                                                    getArguments)
import           Control.Monad.Extra               (unless)
import           Data.Default                      (def)
import           Data.Function                     ((&))
import           Data.Version                      (showVersion)
import           Development.GitRev                (gitHash)
import           Development.IDE                   (action)
import           Development.IDE.Core.OfInterest   (kick)
import           Development.IDE.Core.Rules        (mainRule)
import qualified Development.IDE.Core.Rules        as Rules
import           Development.IDE.Core.Tracing      (withTelemetryLogger)
import           Development.IDE.Graph             (ShakeOptions (shakeThreads))
import qualified Development.IDE.Main              as IDEMain
import qualified Development.IDE.Plugin.HLS.GhcIde as GhcIde
import           Development.IDE.Types.Logger      (Doc, Logger (Logger),
                                                    LoggingColumn (DataColumn, PriorityColumn),
                                                    Pretty (pretty),
                                                    Priority (Debug, Info),
                                                    Recorder (Recorder),
                                                    WithPriority (WithPriority, priority),
                                                    cfilter, cmapWithPrio,
                                                    makeDefaultStderrRecorder)
import qualified Development.IDE.Types.Logger      as Logger
import           Development.IDE.Types.Options
import           Ide.Plugin.Config                 (Config (checkParents, checkProject))
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Paths_ghcide                      (version)
import qualified System.Directory.Extra            as IO
import           System.Environment                (getExecutablePath)
import           System.Exit                       (exitSuccess)
import           System.IO                         (hPutStrLn, stderr)
import           System.Info                       (compilerVersion)

data Log
  = LogIDEMain IDEMain.Log
  | LogRules Rules.Log
  | LogGhcIde GhcIde.Log

instance Pretty Log where
  pretty = \case
    LogIDEMain log -> pretty log
    LogRules log   -> pretty log
    LogGhcIde log  -> pretty log

logToDoc :: Log -> Doc a
logToDoc = pretty

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
      cmapWithPrio logToDoc
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

    let docWithFilteredPriorityRecorder@Recorder{ logger_ } =
          docWithPriorityRecorder
          & cfilter (\WithPriority{ priority } -> priority >= minPriority)

    -- hack so old-school logging still works
    let logger = Logger $ \p m -> logger_ (WithPriority p (pretty m))

    let recorder = docWithFilteredPriorityRecorder
                 & cmapWithPrio logToDoc

    let arguments =
          if argsTesting
          then IDEMain.testing (cmapWithPrio LogIDEMain recorder) logger
          else IDEMain.defaultArguments (cmapWithPrio LogIDEMain recorder) logger

    IDEMain.defaultMain (cmapWithPrio LogIDEMain recorder) arguments
        { IDEMain.argsProjectRoot = Just argsCwd
        , IDEMain.argCommand = argsCommand
        , IDEMain.argsLogger = IDEMain.argsLogger arguments <> pure telemetryLogger

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
                , optOTMemoryProfiling = IdeOTMemoryProfiling argsOTMemoryProfiling
                , optShakeOptions = (optShakeOptions defOptions){shakeThreads = argsThreads}
                , optCheckParents = pure $ checkParents config
                , optCheckProject = pure $ checkProject config
                , optRunSubset = not argsConservativeChangeTracking
                }
        }
