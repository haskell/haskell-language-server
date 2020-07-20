-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above

module Arguments
  ( Arguments(..)
  , getArguments
  , haskellLanguageServerVersion
  ) where

import Data.Version
import Development.GitRev
import Options.Applicative
import Paths_haskell_language_server
import System.Environment

-- ---------------------------------------------------------------------

data Arguments = Arguments
    {argLSP :: Bool
    ,argsCwd :: Maybe FilePath
    ,argFiles :: [FilePath]
    ,argsVersion :: Bool
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

getArguments :: String -> IO Arguments
getArguments exeName = execParser opts
  where
    opts = info (arguments exeName <**> helper)
      ( fullDesc
     <> progDesc "Used as a test bed to check your IDE Client will work"
     <> header (exeName ++ " - GHC Haskell LSP server"))

arguments :: String -> Parser Arguments
arguments exeName = Arguments
      <$> switch (long "lsp" <> help "Start talking to an LSP server")
      <*> optional (strOption $ long "cwd" <> metavar "DIR"
                  <> help "Change to this directory")
      <*> many (argument str (metavar "FILES/DIRS..."))
      <*> switch (long "version"
                  <> help ("Show " ++ exeName  ++ " and GHC versions"))
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

haskellLanguageServerVersion :: IO String
haskellLanguageServerVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "haskell-language-server version: " <> showVersion version
             <> " (GHC: " <> VERSION_ghc
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

