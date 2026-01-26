-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Ide.Logger                    (Priority (..))
import           Ide.Types                     (IdePlugins)
import           Options.Applicative
import           Paths_haskell_language_server
import           System.Directory              (getCurrentDirectory)
import           System.Environment

-- ---------------------------------------------------------------------

data Arguments
  = VersionMode PrintVersion
  | ProbeToolsMode
  | ListPluginsMode
  | BiosMode BiosAction
  | Ghcide GhcideArguments
  | VSCodeExtensionSchemaMode
  | PluginsCustomConfigMarkdownReferenceMode
  | DefaultConfigurationMode
  | PrintLibDir

data GhcideArguments = GhcideArguments
    { argsCommand                 :: Command
    , argsCwd                     :: Maybe FilePath
    , argsShakeProfiling          :: Maybe FilePath
    , argsTesting                 :: Bool
    , argsExamplePlugin           :: Bool
    , argsLogLevel                :: Priority
    , argsLogFile                 :: Maybe String
    -- ^ the minimum log level to show
    , argsLogStderr               :: Bool
    , argsLogClient               :: Bool
    , argsThreads                 :: Int
    , argsProjectGhcVersion       :: Bool
    , argsInitialWorkingDirectory :: FilePath
    } deriving Show

data PrintVersion
  = PrintVersion
  | PrintNumericVersion
  deriving (Show, Eq, Ord)

data BiosAction
  = PrintCradleType
  deriving (Show, Eq, Ord)

getArguments :: String -> IdePlugins IdeState -> IO Arguments
getArguments exeName plugins = do
    cwd <- getCurrentDirectory
    execParser (opts cwd)
  where
    opts cwd = info ((
      VersionMode <$> printVersionParser exeName
      <|> probeToolsParser exeName
      <|> hsubparser
        (  command "vscode-extension-schema" extensionSchemaCommand
        <> command "generate-default-config" generateDefaultConfigCommand
        <> command "plugins-custom-config-markdown-reference" pluginsCustomConfigMarkdownReferenceCommand
        )
      <|> listPluginsParser
      <|> BiosMode <$> biosParser
      <|> Ghcide <$> arguments cwd plugins
      <|> flag' PrintLibDir (long "print-libdir" <> help "Print project GHCs libdir")
      )
      <**> helper)
      ( fullDesc
     <> progDesc "Used as a test bed to check your IDE Client will work"
     <> header (exeName ++ " - GHC Haskell LSP server"))

    extensionSchemaCommand =
        info (pure VSCodeExtensionSchemaMode)
             (fullDesc <> progDesc "Print generic config schema for plugins (used in the package.json of haskell vscode extension)")
    generateDefaultConfigCommand =
        info (pure DefaultConfigurationMode)
             (fullDesc <> progDesc "Print config supported by the server with default values")
    pluginsCustomConfigMarkdownReferenceCommand =
        info (pure PluginsCustomConfigMarkdownReferenceMode)
            (fullDesc <> progDesc "Print markdown reference for plugins custom config")

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

arguments :: FilePath -> IdePlugins IdeState -> Parser GhcideArguments
arguments cwd plugins = GhcideArguments
      <$> (commandP plugins <|> lspCommand <|> checkCommand)
      <*> optional (strOption $ long "cwd" <> metavar "DIR"
                  <> help "Change to this directory")
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR"
                  <> help "Dump profiling reports to this directory")
      <*> switch (long "test"
                  <> help "Enable additional lsp messages used by the testsuite")
      <*> switch (long "example"
                  <> help "Include the Example Plugin. For Plugin devs only")

      <*>
        (option @Priority auto
          (long "log-level"
          <> help "Only show logs at or above this log level"
          <> metavar "LOG_LEVEL"
          <> value Info
          <> showDefault
          )
        <|>
        flag' Debug
          (long "debug"
          <> short 'd'
          <> help "Sets the log level to Debug, alias for '--log-level Debug'"
          )
        )
      -- This option is a little inconsistent with the other log options, since
      -- it's not a boolean and there is no way to turn it off. That's okay
      -- since the default is off.
      <*> (optional (strOption
           ( long "log-file"
          <> metavar "LOGFILE"
          <> help "Send logs to a file"
           )) <|> (optional (strOption
           ( long "logfile"
          <> short 'l'
          <> metavar "LOGFILE"
          <> help "Send logs to a file"
          -- deprecated alias so users don't need to update their CLI calls
          -- immediately
          <> internal
           )))
          )
      -- Boolean option so we can toggle the default in a consistent way
      <*> option auto
           ( long "log-stderr"
          <> help "Send logs to stderr"
          <> metavar "BOOL"
          <> value True
          <> showDefault
           )
      -- Boolean option so we can toggle the default in a consistent way
      <*> option auto
           ( long "log-client"
          <> help "Send logs to the client using the window/logMessage LSP method"
          <> metavar "BOOL"
          -- This is off by default, since some clients will show duplicate logs
          -- if we log both to stderr and the client
          <> value False
          <> showDefault
           )
      <*> option auto
           (short 'j'
          <> help "Number of threads (0: automatic)"
          <> metavar "NUM"
          <> value 0
          <> showDefault
           )
      <*> switch (long "project-ghc-version"
                  <> help "Work out the project GHC version and print it")
      <*> pure cwd
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

