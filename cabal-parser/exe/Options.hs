module Options
  ( Options(..)
  , Command(..)
  , ParseOptions(..)
  , opts
  ) where

import           Options.Applicative

data Options = Options
  { optCommand :: Command
  }
  deriving (Show, Eq, Ord)

data Command
  = Parse ParseOptions
  deriving (Show, Eq, Ord)

data ParseOptions = ParseOptions
  { parseFiles :: [FilePath]
  , silent     :: Bool
  , showAst    :: Bool
  }
  deriving (Show, Eq, Ord)

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
  <> progDesc "Command Line Interface for cabal-parse"
  <> header "Parse your cabal files"
  )

options :: Parser Options
options =
  Options
    <$> parseCommands

parseCommands :: Parser Command
parseCommands =
  subparser
    ( command "parse" (info (Parse <$> parseCommand) $ progDesc "Check cabal-parse can parse your cabal file")
    )

parseCommand :: Parser ParseOptions
parseCommand =
  ParseOptions
    <$> some (argument str (metavar "FILES"))
    <*> flag False True
       (  long "silent"
       <> help "Don't show parse errors on stdout"
       )
    <*> flag False True
       (  long "ast"
       <> help "Show the parsed AST in a human readable form"
       )
