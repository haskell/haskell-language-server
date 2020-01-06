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
