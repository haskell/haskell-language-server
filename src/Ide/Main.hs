-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ide.Main(defaultMain, runLspMode) where

import Control.Monad.Extra
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Development.IDE.Core.Rules
import Development.IDE.Session (setInitialDynFlags, getHieDbLoc, runWithDb)
import Development.IDE.Types.Logger as G
import qualified Language.Haskell.LSP.Core as LSP
import Ide.Arguments
import Ide.Logger
import Ide.Version
import Ide.Types (IdePlugins, ipMap)
import qualified System.Directory.Extra as IO
import System.Exit
import System.IO
import qualified System.Log.Logger as L
import HieDb.Run
import qualified Development.IDE.Main as Main
import qualified Development.IDE.Types.Options as Ghcide
import Development.Shake (ShakeOptions(shakeThreads))

defaultMain :: Arguments -> IdePlugins IdeState -> IO ()
defaultMain args idePlugins = do
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

        DbCmd opts cmd -> do
          dir <- IO.getCurrentDirectory
          dbLoc <- getHieDbLoc dir
          hPutStrLn stderr $ "Using hiedb at: " ++ dbLoc
          mlibdir <- setInitialDynFlags
          case mlibdir of
            Nothing -> exitWith $ ExitFailure 1
            Just libdir ->
              runCommand libdir opts{database = dbLoc} cmd

        LspMode lspArgs -> do
            {- see WARNING above -}
            hPutStrLn stderr hlsVer
            runLspMode lspArgs idePlugins

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

runLspMode :: LspArguments -> IdePlugins IdeState -> IO ()
runLspMode lspArgs@LspArguments{..} idePlugins = do
    whenJust argsCwd IO.setCurrentDirectory
    dir <- IO.getCurrentDirectory
    dbLoc <- getHieDbLoc dir
    LSP.setupLogger argsLogFile ["hls", "hie-bios"]
      $ if argsDebugOn then L.DEBUG else L.INFO

    when argLSP $ do
        hPutStrLn stderr "Starting (haskell-language-server)LSP server..."
        hPutStrLn stderr $ "  with arguments: " <> show lspArgs
        hPutStrLn stderr $ "  with plugins: " <> show (Map.keys $ ipMap idePlugins)
        hPutStrLn stderr $ "  in directory: " <> dir
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"

    runWithDb dbLoc $ \hiedb hiechan ->
        Main.defaultMain (Main.defArguments hiedb hiechan)
          { Main.argFiles = if argLSP then Nothing else Just []
          , Main.argsHlsPlugins = idePlugins
          , Main.argsLogger = hlsLogger
          , Main.argsIdeOptions = \_config sessionLoader ->
            let defOptions = Ghcide.defaultIdeOptions sessionLoader
            in defOptions
                { Ghcide.optShakeProfiling = argsShakeProfiling
                , Ghcide.optTesting = Ghcide.IdeTesting argsTesting
                , Ghcide.optShakeOptions = (Ghcide.optShakeOptions defOptions)
                    {shakeThreads = argsThreads}
                }
          }
