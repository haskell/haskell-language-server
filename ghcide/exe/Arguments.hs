-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Arguments(Arguments(..), getArguments) where

import           Development.IDE      (IdeState)
import           Development.IDE.Main (Command (..), commandP)
import           Ide.Types            (IdePlugins)
import           Options.Applicative

data Arguments = Arguments
    {argsCwd               :: Maybe FilePath
    ,argsVersion           :: Bool
    ,argsShakeProfiling    :: Maybe FilePath
    ,argsOTMemoryProfiling :: Bool
    ,argsTesting           :: Bool
    ,argsDisableKick       :: Bool
    ,argsThreads           :: Int
    ,argsVerbose           :: Bool
    ,argsCommand           :: Command
    }

getArguments :: IdePlugins IdeState -> IO Arguments
getArguments plugins = execParser opts
  where
    opts = info (arguments plugins <**> helper)
      ( fullDesc
     <> header "ghcide - the core of a Haskell IDE")

arguments :: IdePlugins IdeState -> Parser Arguments
arguments plugins = Arguments
      <$> optional (strOption $ long "cwd" <> metavar "DIR" <> help "Change to this directory")
      <*> switch (long "version" <> help "Show ghcide and GHC versions")
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR" <> help "Dump profiling reports to this directory")
      <*> switch (long "ot-memory-profiling" <> help "Record OpenTelemetry info to the eventlog. Needs the -l RTS flag to have an effect")
      <*> switch (long "test" <> help "Enable additional lsp messages used by the testsuite")
      <*> switch (long "test-no-kick" <> help "Disable kick. Useful for testing cancellation")
      <*> option auto (short 'j' <> help "Number of threads (0: automatic)" <> metavar "NUM" <> value 0 <> showDefault)
      <*> switch (long "verbose" <> help "Include internal events in logging output")
      <*> (commandP plugins <|> lspCommand <|> checkCommand)
      where
          checkCommand = Check <$> many (argument str (metavar "FILES/DIRS..."))
          lspCommand = LSP <$ flag' True (long "lsp" <> help "Start talking to an LSP client")
