-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import           Arguments                         (Arguments (..),
                                                    getArguments)
import           Control.Monad.Extra               (unless, whenJust)
import           Data.Default                      (Default (def))
import           Data.Version                      (showVersion)
import           Development.GitRev                (gitHash)
import           Development.IDE                   (action)
import           Development.IDE.Core.OfInterest   (kick)
import           Development.IDE.Core.Rules        (mainRule)
import           Development.IDE.Graph             (ShakeOptions (shakeThreads))
import qualified Development.IDE.Main              as Main
import qualified Development.IDE.Plugin.HLS.GhcIde as GhcIde
import qualified Development.IDE.Plugin.Test       as Test
import           Development.IDE.Types.Options
import           Ide.Plugin.Config                 (Config (checkParents, checkProject))
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
    let hlsPlugins = pluginDescToIdePlugins GhcIde.descriptors
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments hlsPlugins

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    whenJust argsCwd IO.setCurrentDirectory

    let arguments = if argsTesting then Main.testing else def

    Main.defaultMain arguments
        {Main.argCommand = argsCommand

        ,Main.argsRules = do
            -- install the main and ghcide-plugin rules
            mainRule
            -- install the kick action, which triggers a typecheck on every
            -- Shake database restart, i.e. on every user edit.
            unless argsDisableKick $
                action kick

        ,Main.argsThreads = case argsThreads of 0 -> Nothing ; i -> Just (fromIntegral i)

        ,Main.argsIdeOptions = \config sessionLoader ->
            let defOptions = Main.argsIdeOptions arguments config sessionLoader
            in defOptions
                { optShakeProfiling = argsShakeProfiling
                , optOTMemoryProfiling = IdeOTMemoryProfiling argsOTMemoryProfiling
                , optShakeOptions = (optShakeOptions defOptions){shakeThreads = argsThreads}
                , optCheckParents = pure $ checkParents config
                , optCheckProject = pure $ checkProject config
                , optRunSubset = not argsConservativeChangeTracking
                }
        }
