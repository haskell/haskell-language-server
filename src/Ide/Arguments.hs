-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above

module Ide.Arguments
  ( Arguments(..)
  , LspArguments(..)
  , PrintVersion(..)
  , getArguments
  , haskellLanguageServerVersion
  , haskellLanguageServerNumericVersion
  ) where

import Data.Version
import Development.GitRev
import Options.Applicative
import Paths_haskell_language_server
import System.Environment
import HieDb.Run

-- ---------------------------------------------------------------------

data Arguments
  = VersionMode PrintVersion
  | ProbeToolsMode
  | DbCmd Options Command
  | LspMode LspArguments

data LspArguments = LspArguments
    {argLSP :: Bool
    ,argsCwd :: Maybe FilePath
    ,argFiles :: [FilePath]
    ,argsShakeProfiling :: Maybe FilePath
    ,argsTesting :: Bool
    ,argsExamplePlugin :: Bool
    -- These next two are for compatibility with existing hie clients, allowing
    -- them to just change the name of the exe and still work.
    , argsDebugOn       :: Bool
    , argsLogFile       :: Maybe String
    , argsThreads       :: Int
    , argsProjectGhcVersion :: Bool
    } deriving Show

data PrintVersion
  = PrintVersion
  | PrintNumericVersion
  deriving (Show, Eq, Ord)

getArguments :: String -> IO Arguments
getArguments exeName = execParser opts
  where
    hieInfo = fullDesc <> progDesc "Query .hie files"
    opts = info ((
      VersionMode <$> printVersionParser exeName
      <|> probeToolsParser exeName
      <|> hsubparser (command "hiedb" (info (DbCmd <$> optParser "" True <*> cmdParser <**> helper) hieInfo))
      <|> LspMode <$> arguments)
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

probeToolsParser :: String -> Parser Arguments
probeToolsParser exeName =
  flag' ProbeToolsMode
    (long "probe-tools" <> help ("Show " ++ exeName  ++ " version and other tools of interest"))

arguments :: Parser LspArguments
arguments = LspArguments
      <$> switch (long "lsp" <> help "Start talking to an LSP server")
      <*> optional (strOption $ long "cwd" <> metavar "DIR"
                  <> help "Change to this directory")
      <*> many (argument str (metavar "FILES/DIRS..."))
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

-- ---------------------------------------------------------------------

haskellLanguageServerNumericVersion :: String
haskellLanguageServerNumericVersion = showVersion version

haskellLanguageServerVersion :: IO String
haskellLanguageServerVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "haskell-language-server version: " <> haskellLanguageServerNumericVersion
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

