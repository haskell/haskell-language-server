-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP                 #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ide.Main(defaultMain, runLspMode, Log) where

import           Control.Monad.Extra
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Default
import           Data.List                     (sort)
import qualified Data.Text                     as T
import           Development.IDE.Core.Rules    hiding (Log)
import           Development.IDE.Core.Tracing  (withTelemetryLogger)
import           Development.IDE.Graph         (ShakeOptions (shakeThreads))
import           Development.IDE.Main          (isLSP)
import qualified Development.IDE.Main          as IDEMain
import qualified Development.IDE.Session       as Session
import           Development.IDE.Types.Logger  as G
import qualified Development.IDE.Types.Options as Ghcide
import           Ide.Arguments
import           Ide.Logger
import           Ide.Plugin.ConfigUtils        (pluginsToDefaultConfig,
                                                pluginsToVSCodeExtensionSchema)
import           Ide.Types                     (IdePlugins, PluginId (PluginId),
                                                ipMap)
import           Ide.Version
import           Prettyprinter                 (Pretty, pretty, (<+>))
import qualified Prettyprinter
import qualified System.Directory.Extra        as IO

data Log
  = LogVersion !String
  | LogDirectory !FilePath
  | LogLspStart !GhcideArguments ![PluginId]
  -- hPutStrLn stderr "Starting (haskell-language-server)LSP server..."
  -- hPutStrLn stderr $ "  with arguments: " <> show ghcideArgs
  -- hPutStrLn stderr $ "  with plugins: " <> show (map fst $ ipMap idePlugins)
  -- hPutStrLn stderr $ "  in directory: " <> dir
  | LogIDEMain IDEMain.Log
  deriving Show

instance Pretty Log where
  pretty log = case log of
    LogVersion version -> pretty version
    LogDirectory path -> "Directory:" <+> pretty path
    LogLspStart ghcideArgs pluginIds ->
      Prettyprinter.nest 2 $
        Prettyprinter.vsep
          [ "Starting (haskell-language-server) LSP server..."
          , pretty ghcideArgs
          , "PluginIds:" <+> pretty pluginIds ]
    LogIDEMain iDEMainLog -> pretty iDEMainLog

defaultMain :: Recorder Log -> Arguments -> IdePlugins IdeState -> IO ()
defaultMain recorder args idePlugins = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work

    hlsVer <- haskellLanguageServerVersion
    case args of
        ProbeToolsMode -> do
            programsOfInterest <- findProgramVersions
            putStrLn hlsVer
            putStrLn "Tool versions found on the $PATH"
            putStrLn $ showProgramVersionOfInterest programsOfInterest

        VersionMode PrintVersion ->
            putStrLn hlsVer

        VersionMode PrintNumericVersion ->
            putStrLn haskellLanguageServerNumericVersion

        ListPluginsMode -> do
            let pluginNames = sort
                    $ map ((\(PluginId t) -> T.unpack t) . fst)
                    $ ipMap idePlugins
            mapM_ putStrLn pluginNames

        BiosMode PrintCradleType -> do
            dir <- IO.getCurrentDirectory
            hieYaml <- Session.findCradle def dir
            cradle <- Session.loadCradle def hieYaml dir
            print cradle

        Ghcide ghcideArgs -> do
            {- see WARNING above -}
            logWith recorder $ LogVersion hlsVer
            runLspMode recorder ghcideArgs idePlugins

        VSCodeExtensionSchemaMode -> do
          LBS.putStrLn $ A.encodePretty $ pluginsToVSCodeExtensionSchema idePlugins

        DefaultConfigurationMode -> do
          LBS.putStrLn $ A.encodePretty $ pluginsToDefaultConfig idePlugins

-- ---------------------------------------------------------------------

hlsLogger :: G.Logger
hlsLogger = G.Logger $ \pri txt ->
    case pri of
      G.Telemetry -> logm     (T.unpack txt)
      G.Debug     -> debugm   (T.unpack txt)
      G.Info      -> logm     (T.unpack txt)
      G.Warning   -> warningm (T.unpack txt)
      G.Error     -> errorm   (T.unpack txt)

-- ---------------------------------------------------------------------

runLspMode :: Recorder Log -> GhcideArguments -> IdePlugins IdeState -> IO ()
runLspMode recorder ghcideArgs@GhcideArguments{..} idePlugins = withTelemetryLogger $ \telemetryLogger -> do
    let log = logWith recorder
    whenJust argsCwd IO.setCurrentDirectory
    dir <- IO.getCurrentDirectory
    log $ LogDirectory dir

    when (isLSP argsCommand) $ do
        log $ LogLspStart ghcideArgs (map fst $ ipMap idePlugins)

    IDEMain.defaultMain (cmap LogIDEMain recorder) (IDEMain.defaultArguments (cmap LogIDEMain recorder) Info)
      { IDEMain.argCommand = argsCommand
      , IDEMain.argsHlsPlugins = idePlugins
      , IDEMain.argsLogger = pure hlsLogger <> pure telemetryLogger
      , IDEMain.argsThreads = if argsThreads == 0 then Nothing else Just $ fromIntegral argsThreads
      , IDEMain.argsIdeOptions = \_config sessionLoader ->
        let defOptions = Ghcide.defaultIdeOptions sessionLoader
        in defOptions
            { Ghcide.optShakeProfiling = argsShakeProfiling
            , Ghcide.optTesting = Ghcide.IdeTesting argsTesting
            , Ghcide.optShakeOptions = (Ghcide.optShakeOptions defOptions)
                {shakeThreads = argsThreads}
            }
      }
