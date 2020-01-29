{-# LANGUAGE CPP #-}
module Ide.Options where

import           Options.Applicative.Simple

data GlobalOpts = GlobalOpts
  { optDebugOn       :: Bool
  , optLogFile       :: Maybe String
  , optLsp           :: Bool
  , projectRoot      :: Maybe String
  , optBiosVerbose   :: Bool
  , optCaptureFile   :: Maybe FilePath
  , optExamplePlugin :: Bool
  , optDryRun        :: Bool
  , optFiles         :: [FilePath]
  } deriving (Show)

globalOptsParser :: Parser GlobalOpts
globalOptsParser = GlobalOpts
  <$> switch
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
  <*> flag False True
       ( long "lsp"
       <> help "Start HIE as an LSP server. Otherwise it dumps debug info to stdout")
  <*> optional (strOption
       ( long "project-root"
      <> short 'r'
      <> metavar "PROJECTROOT"
      <> help "Root directory of project, defaults to cwd"))
  <*> (switch
          ( long "bios-verbose"
          <> help "enable verbose logging for hie-bios"
          )
       <|>
       switch
          ( long "vomit"
          <> help "(deprecated) enable verbose logging for hie-bios"
          )
      )
  <*> optional (strOption
       ( long "capture"
      <> short 'c'
      <> metavar "CAPTUREFILE"
      <> help "File to capture the session to"
       ))
  <*> switch
       ( long "example"
       <> help "Enable Example2 plugin. Useful for developers only")
  <*> flag False True
     (  long "dry-run"
     <> help "Perform a dry-run of loading files. Only searches for Haskell source files to load. Does nothing if run as LSP server."
     )
  <*> many
     ( argument str
       (  metavar "FILES..."
       <> help "Directories and Filepaths to load. Does nothing if run as LSP server.")
     )
