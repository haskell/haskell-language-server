-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           Arguments                         (Arguments (..),
                                                    getArguments)
import           Control.Concurrent.Extra          (newLock, withLock)
import           Control.Monad.Extra               (unless, when, whenJust)
import qualified Data.Aeson.Encode.Pretty          as A
import           Data.Default                      (Default (def))
import           Data.List.Extra                   (upper)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import           Data.Text.Lazy.Encoding           (decodeUtf8)
import qualified Data.Text.Lazy.IO                 as LT
import           Data.Version                      (showVersion)
import           Development.GitRev                (gitHash)
import           Development.IDE                   (Logger (Logger),
                                                    Priority (Info), action)
import           Development.IDE.Core.OfInterest   (kick)
import           Development.IDE.Core.Rules        (mainRule)
import           Development.IDE.Graph             (ShakeOptions (shakeThreads))
import qualified Development.IDE.Main              as Main
import qualified Development.IDE.Plugin.HLS.GhcIde as GhcIde
import qualified Development.IDE.Plugin.Test       as Test
import           Development.IDE.Types.Options
import           Ide.Plugin.Config                 (Config (checkParents, checkProject))
import           Ide.Plugin.ConfigUtils            (pluginsToDefaultConfig,
                                                    pluginsToVSCodeExtensionSchema)
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Paths_ghcide                      (version)
import qualified System.Directory.Extra            as IO
import           System.Environment                (getExecutablePath)
import           System.Exit                       (exitSuccess)
import           System.IO                         (hPutStrLn, stderr)
import           System.Info                       (compilerVersion)

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
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    let hlsPlugins = pluginDescToIdePlugins GhcIde.descriptors

    when argsVSCodeExtensionSchema $ do
      LT.putStrLn $ decodeUtf8 $ A.encodePretty $ pluginsToVSCodeExtensionSchema hlsPlugins
      exitSuccess

    when argsDefaultConfig $ do
      LT.putStrLn $ decodeUtf8 $ A.encodePretty $ pluginsToDefaultConfig hlsPlugins
      exitSuccess

    whenJust argsCwd IO.setCurrentDirectory

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger = Logger $ \pri msg -> when (pri >= logLevel) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg
        logLevel = if argsVerbose then minBound else Info

    Main.defaultMain def
        {Main.argCommand = argsCommand

        ,Main.argsLogger = pure logger

        ,Main.argsRules = do
            -- install the main and ghcide-plugin rules
            mainRule
            -- install the kick action, which triggers a typecheck on every
            -- Shake database restart, i.e. on every user edit.
            unless argsDisableKick $
                action kick

        ,Main.argsHlsPlugins =
            pluginDescToIdePlugins $
            GhcIde.descriptors
            ++ [Test.blockCommandDescriptor "block-command" | argsTesting]

        ,Main.argsGhcidePlugin = if argsTesting
            then Test.plugin
            else mempty

        ,Main.argsIdeOptions = \config  sessionLoader ->
            let defOptions = defaultIdeOptions sessionLoader
            in defOptions
                { optShakeProfiling = argsShakeProfiling
                , optOTMemoryProfiling = IdeOTMemoryProfiling argsOTMemoryProfiling
                , optTesting = IdeTesting argsTesting
                , optShakeOptions = (optShakeOptions defOptions){shakeThreads = argsThreads}
                , optCheckParents = pure $ checkParents config
                , optCheckProject = pure $ checkProject config
                }
        }
