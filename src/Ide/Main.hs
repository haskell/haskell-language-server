-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP                 #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Ide.Main(defaultMain, runLspMode, Log(..)) where

import           Control.Monad.Extra
import qualified Data.Aeson.Encode.Pretty      as A
import           Data.Coerce                   (coerce)
import           Data.Default
import           Data.List                     (sort)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Text.Lazy.Encoding       (decodeUtf8)
import qualified Data.Text.Lazy.IO             as LT
import           Development.IDE.Core.Rules    hiding (Log, logToPriority)
import           Development.IDE.Core.Tracing  (withTelemetryLogger)
import           Development.IDE.Main          (isLSP)
import qualified Development.IDE.Main          as IDEMain
import qualified Development.IDE.Session       as Session
import           Development.IDE.Types.Logger  as G
import qualified Development.IDE.Types.Options as Ghcide
import           GHC.Stack                     (emptyCallStack)
import qualified HIE.Bios.Environment          as HieBios
import           HIE.Bios.Types                hiding (Log)
import           Ide.Arguments
import           Ide.Plugin.ConfigUtils        (pluginsToDefaultConfig,
                                                pluginsToVSCodeExtensionSchema)
import           Ide.Types                     (IdePlugins, PluginId (PluginId),
                                                ipMap, pluginId)
import           Ide.Version
import           System.Directory
import qualified System.Directory.Extra        as IO
import           System.FilePath

data Log
  = LogVersion !String
  | LogDirectory !FilePath
  | LogLspStart !GhcideArguments ![PluginId]
  | LogIDEMain IDEMain.Log
  | LogOther T.Text
  deriving Show

instance Pretty Log where
  pretty log = case log of
    LogVersion version -> pretty version
    LogDirectory path -> "Directory:" <+> pretty path
    LogLspStart ghcideArgs pluginIds ->
      nest 2 $
        vsep
          [ "Starting (haskell-language-server) LSP server..."
          , viaShow ghcideArgs
          , "PluginIds:" <+> pretty (coerce @_ @[Text] pluginIds) ]
    LogIDEMain iDEMainLog -> pretty iDEMainLog
    LogOther t -> pretty t

defaultMain :: Recorder (WithPriority Log) -> Arguments -> IdePlugins IdeState -> IO ()
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
                    $ map ((\(PluginId t) -> T.unpack t) . pluginId)
                    $ ipMap idePlugins
            mapM_ putStrLn pluginNames

        BiosMode PrintCradleType -> do
            dir <- IO.getCurrentDirectory
            hieYaml <- Session.findCradle def dir
            cradle <- Session.loadCradle def hieYaml dir
            print cradle

        Ghcide ghcideArgs -> do
            {- see WARNING above -}
            logWith recorder Info $ LogVersion hlsVer
            runLspMode recorder ghcideArgs idePlugins

        VSCodeExtensionSchemaMode -> do
          LT.putStrLn $ decodeUtf8 $ encodePrettySorted $ pluginsToVSCodeExtensionSchema idePlugins
        DefaultConfigurationMode -> do
          LT.putStrLn $ decodeUtf8 $ encodePrettySorted $ pluginsToDefaultConfig idePlugins
        PrintLibDir -> do
          d <- getCurrentDirectory
          let initialFp = d </> "a"
          hieYaml <- Session.findCradle def initialFp
          cradle <- Session.loadCradle def hieYaml d
          (CradleSuccess libdir) <- HieBios.getRuntimeGhcLibDir cradle
          putStr libdir
  where
    encodePrettySorted = A.encodePretty' A.defConfig
      { A.confCompare = compare
      }

-- ---------------------------------------------------------------------

runLspMode :: Recorder (WithPriority Log) -> GhcideArguments -> IdePlugins IdeState -> IO ()
runLspMode recorder ghcideArgs@GhcideArguments{..} idePlugins = withTelemetryLogger $ \telemetryLogger -> do
    let log = logWith recorder
    whenJust argsCwd IO.setCurrentDirectory
    dir <- IO.getCurrentDirectory
    log Info $ LogDirectory dir

    when (isLSP argsCommand) $ do
        log Info $ LogLspStart ghcideArgs (map pluginId $ ipMap idePlugins)

    -- exists so old-style logging works. intended to be phased out
    let logger = Logger $ \p m -> logger_ recorder (WithPriority p emptyCallStack $ LogOther m)
        args = (if argsTesting then IDEMain.testing else IDEMain.defaultArguments)
                    (cmapWithPrio LogIDEMain recorder) logger idePlugins

    IDEMain.defaultMain (cmapWithPrio LogIDEMain recorder) args
      { IDEMain.argCommand = argsCommand
      , IDEMain.argsLogger = pure logger <> pure telemetryLogger
      , IDEMain.argsThreads = if argsThreads == 0 then Nothing else Just $ fromIntegral argsThreads
      , IDEMain.argsIdeOptions = \config sessionLoader ->
        let defOptions = IDEMain.argsIdeOptions args config sessionLoader
        in defOptions { Ghcide.optShakeProfiling = argsShakeProfiling }
      }
