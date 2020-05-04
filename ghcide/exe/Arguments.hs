-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Arguments(Arguments, Arguments'(..), getArguments, IdeCmd(..)) where

import Options.Applicative
import HieDb.Run

type Arguments = Arguments' IdeCmd

data IdeCmd = Typecheck [FilePath] | DbCmd Options Command | LSP

data Arguments' a = Arguments
    {argLSP :: Bool
    ,argsCwd :: Maybe FilePath
    ,argsVersion :: Bool
    ,argsShakeProfiling :: Maybe FilePath
    ,argsOTMemoryProfiling :: Bool
    ,argsTesting :: Bool
    ,argsDisableKick :: Bool
    ,argsThreads :: Int
    ,argsVerbose :: Bool
    ,argFilesOrCmd :: a
    }

getArguments :: IO Arguments
getArguments = execParser opts
  where
    opts = info (arguments <**> helper)
      ( fullDesc
     <> header "ghcide - the core of a Haskell IDE")

arguments :: Parser Arguments
arguments = Arguments
      <$> switch (long "lsp" <> help "Start talking to an LSP client")
      <*> optional (strOption $ long "cwd" <> metavar "DIR" <> help "Change to this directory")
      <*> switch (long "version" <> help "Show ghcide and GHC versions")
      <*> optional (strOption $ long "shake-profiling" <> metavar "DIR" <> help "Dump profiling reports to this directory")
      <*> switch (long "ot-memory-profiling" <> help "Record OpenTelemetry info to the eventlog. Needs the -l RTS flag to have an effect")
      <*> switch (long "test" <> help "Enable additional lsp messages used by the testsuite")
      <*> switch (long "test-no-kick" <> help "Disable kick. Useful for testing cancellation")
      <*> option auto (short 'j' <> help "Number of threads (0: automatic)" <> metavar "NUM" <> value 0 <> showDefault)
      <*> switch (long "verbose" <> help "Include internal events in logging output")
      <*> ( hsubparser (command "typecheck" (info (Typecheck <$> fileCmd) fileInfo)
                   <> command "hiedb" (info (DbCmd <$> optParser "" True <*> cmdParser <**> helper) hieInfo)
                   <> command "lsp" (info (pure LSP <**> helper) lspInfo)  )
         <|> Typecheck <$> fileCmd )
  where
    fileCmd = many (argument str (metavar "FILES/DIRS..."))
    lspInfo = fullDesc <> progDesc "Start talking to an LSP client"
    fileInfo = fullDesc <> progDesc "Used as a test bed to check your IDE will work"
    hieInfo = fullDesc <> progDesc "Query .hie files"
