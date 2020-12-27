-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Arguments(Arguments(..), getArguments) where

import Options.Applicative


data Arguments = Arguments
    {argLSP :: Bool
    ,argsCwd :: Maybe FilePath
    ,argFiles :: [FilePath]
    ,argsVersion :: Bool
    ,argsShakeProfiling :: Maybe FilePath
    ,argsOTMemoryProfiling :: Bool
    ,argsTesting :: Bool
    ,argsDisableKick :: Bool
    ,argsThreads :: Int
    ,argsVerbose :: Bool
    }

getArguments :: IO Arguments
getArguments = execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc
     <> progDesc "Used as a test bed to check your IDE will work"
     <> header "ghcide - the core of a Haskell IDE")

arguments :: Parser Arguments
arguments = Arguments
      <$> switch (long "lsp" <> help "Start talking to an LSP server")
      <*> optional (strOption $ long "cwd" <> metavar "DIR" <> help "Change to this directory")
      <*> many (argument str (metavar "FILES/DIRS..."))
      <*> switch (long "version" <> help "Show ghcide and GHC versions")
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR" <> help "Dump profiling reports to this directory")
      <*> switch (long "ot-memory-profiling" <> help "Record OpenTelemetry info to the eventlog. Needs the -l RTS flag to have an effect")
      <*> switch (long "test" <> help "Enable additional lsp messages used by the testsuite")
      <*> switch (long "test-no-kick" <> help "Disable kick. Useful for testing cancellation")
      <*> option auto (short 'j' <> help "Number of threads (0: automatic)" <> metavar "NUM" <> value 0 <> showDefault)
      <*> switch (long "verbose" <> help "Include internal events in logging output")
