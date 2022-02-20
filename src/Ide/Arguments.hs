-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above

module Ide.Arguments
  ( Arguments(..)
  , GhcideArguments(..)
  , PrintVersion(..)
  , BiosAction(..)
  , getArguments
  , haskellLanguageServerVersion
  , haskellLanguageServerNumericVersion
  ) where

import           Data.Version
import           Development.IDE               (IdeState)
import           Development.IDE.Main          (Command (..), commandP)
import           GitHash                       (giHash, tGitInfoCwdTry)
import           Ide.Types                     (IdePlugins)
import           Options.Applicative
import           Paths_haskell_language_server
import           System.Environment

-- ---------------------------------------------------------------------

data Arguments
  = VersionMode PrintVersion
  | ProbeToolsMode
  | ListPluginsMode
  | BiosMode BiosAction
  | Ghcide GhcideArguments
  | VSCodeExtensionSchemaMode
  | DefaultConfigurationMode
  | PrintLibDir

data GhcideArguments = GhcideArguments
    {argsCommand            :: Command
    ,argsCwd                :: Maybe FilePath
    ,argsShakeProfiling     :: Maybe FilePath
    ,argsTesting            :: Bool
    ,argsExamplePlugin      :: Bool
    -- These next two are for compatibility with existing hie clients, allowing
    -- them to just change the name of the exe and still work.
    , argsDebugOn           :: Bool
    , argsLogFile           :: Maybe String
    , argsThreads           :: Int
    , argsProjectGhcVersion :: Bool
    } deriving Show

data PrintVersion
  = PrintVersion
  | PrintNumericVersion
  deriving (Show, Eq, Ord)

data BiosAction
  = PrintCradleType
  deriving (Show, Eq, Ord)

getArguments :: String -> IdePlugins IdeState -> IO Arguments
getArguments exeName plugins = execParser opts
  where
    opts = info ((
      VersionMode <$> printVersionParser exeName
      <|> probeToolsParser exeName
      <|> listPluginsParser
      <|> BiosMode <$> biosParser
      <|> Ghcide <$> arguments plugins
      <|> flag' PrintLibDir (long "print-libdir" <> help "Print project GHCs libdir")
      )
      <**> helper)
      ( fullDesc
     <> progDesc "Used as a test bed to check your IDE Client will work"
     <> header (exeName ++ " - GHC Haskell LSP server"))

printVersionParser :: String -> Parser PrintVersion
printVersionParser exeName =
  flag' PrintVersion
    (long "version" <> help ("Show " ++ exeName  ++ " and GHC versions"))
  <|>
  flag' PrintNumericVersion
    (long "numeric-version" <> help ("Show numeric version of " ++ exeName))

biosParser :: Parser BiosAction
biosParser =
  flag' PrintCradleType
    (long "print-cradle" <> help "Print the project cradle type")

probeToolsParser :: String -> Parser Arguments
probeToolsParser exeName =
  flag' ProbeToolsMode
    (long "probe-tools" <> help ("Show " ++ exeName  ++ " version and other tools of interest"))

listPluginsParser :: Parser Arguments
listPluginsParser =
  flag' ListPluginsMode
    (long "list-plugins" <> help "List all available plugins")

arguments :: IdePlugins IdeState -> Parser GhcideArguments
arguments plugins = GhcideArguments
      <$> (commandP plugins <|> lspCommand <|> checkCommand)
      <*> optional (strOption $ long "cwd" <> metavar "DIR"
                  <> help "Change to this directory")
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR"
                  <> help "Dump profiling reports to this directory")
      <*> switch (long "test"
                  <> help "Enable additional lsp messages used by the testsuite")
      <*> switch (long "example"
                  <> help "Include the Example Plugin. For Plugin devs only")

      <*> switch
           ( long "debug"
          <> short 'd'
          <> help "Generate debug output"
           )
      <*> optional (strOption
           ( long "logfile"
          <> short 'l'
          <> metavar "LOGFILE"
          <> help "File to log to, defaults to stdout"
           ))
      <*> option auto
           (short 'j'
          <> help "Number of threads (0: automatic)"
          <> metavar "NUM"
          <> value 0
          <> showDefault
           )
      <*> switch (long "project-ghc-version"
                  <> help "Work out the project GHC version and print it")
    where
        lspCommand = LSP <$ flag' True (long "lsp" <> help "Start talking to an LSP server")
        checkCommand = Check <$> many (argument str (metavar "FILES/DIRS..."))

-- ---------------------------------------------------------------------

haskellLanguageServerNumericVersion :: String
haskellLanguageServerNumericVersion = showVersion version

haskellLanguageServerVersion :: IO String
haskellLanguageServerVersion = do
  path <- getExecutablePath
  let gi = $$tGitInfoCwdTry
      gitHashSection = case gi of
        Right gi -> " (GIT hash: " <> giHash gi <> ")"
        Left _   -> ""
  return $ "haskell-language-server version: " <> haskellLanguageServerNumericVersion
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

