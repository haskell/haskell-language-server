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
import           Development.IDE                   (Priority (Debug, Info),
                                                    action)
import           Development.IDE.Core.OfInterest   (kick)
import           Development.IDE.Core.Rules        (mainRule)
import qualified Development.IDE.Core.Rules        as Rules
import           Development.IDE.Core.Tracing      (withTelemetryLogger)
import           Development.IDE.Graph             (ShakeOptions (shakeThreads))
import qualified Development.IDE.Main              as IDEMain
import qualified Development.IDE.Plugin.HLS.GhcIde as GhcIde
import           Development.IDE.Types.Logger      (WithPriority (WithPriority, priority),
                                                    cfilter, cmap,
                                                    makeDefaultStderrRecorder)
import qualified Development.IDE.Types.Logger      as Logger
import           Development.IDE.Types.Options
import           Ide.Plugin.Config                 (Config (checkParents, checkProject))
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Paths_ghcide                      (version)
import           Prettyprinter                     (Doc, Pretty (pretty))
import qualified System.Directory.Extra            as IO
import           System.Environment                (getExecutablePath)
import           System.Exit                       (exitSuccess)
import           System.IO                         (hPutStrLn, stderr)
import           System.Info                       (compilerVersion)
import qualified System.Log                        as HsLogger

data Log
  = LogIDEMain IDEMain.Log
  | LogRules Rules.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogIDEMain log -> pretty log
    LogRules log   -> pretty log

logToPriority :: Log -> Logger.Priority
logToPriority = \case
  LogIDEMain log -> IDEMain.logToPriority log
  LogRules log   -> Rules.logToPriority log

logToDocWithPriority :: Log -> WithPriority (Doc a)
logToDocWithPriority log = WithPriority (logToPriority log) (pretty log)

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
    let hlsPlugins = pluginDescToIdePlugins (GhcIde.descriptors mempty)
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments hlsPlugins

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- if user uses --cwd option we need to make this path absolute (and set the current directory to it)
    argsCwd <- case argsCwd of
      Nothing   -> IO.getCurrentDirectory
      Just root -> IO.setCurrentDirectory root >> IO.getCurrentDirectory

    let (hsLoggerMinPriority, minPriority) = if argsVerbose then (HsLogger.DEBUG, Debug) else (HsLogger.INFO, Info)

    defaultRecorder <- makeDefaultStderrRecorder hsLoggerMinPriority

    let recorder = defaultRecorder
                 & cfilter (\WithPriority{ priority } -> priority >= minPriority)
                 & cmap logToDocWithPriority

    let arguments =
          if argsTesting
          then IDEMain.testing (cmap LogIDEMain recorder)
          else IDEMain.defaultArguments (cmap LogIDEMain recorder) minPriority

    IDEMain.defaultMain (cmap LogIDEMain recorder) arguments
        { IDEMain.argsProjectRoot = Just argsCwd
        , IDEMain.argCommand = argsCommand
        , IDEMain.argsLogger = IDEMain.argsLogger arguments <> pure telemetryLogger

        , IDEMain.argsRules = do
            -- install the main and ghcide-plugin rules
            mainRule (cmap LogRules recorder) def
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
