-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}

-- | Options
module Development.IDE.Types.Options
  ( IdeOptions(..)
  , IdePreprocessedSource(..)
  , IdeReportProgress(..)
  , IdeDefer(..)
  , clientSupportsProgress
  , IdePkgLocationOptions(..)
  , defaultIdeOptions
  ) where

import Development.Shake
import Development.IDE.GHC.Util
import           GHC hiding (parseModule, typecheckModule)
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import qualified Data.Text as T

data IdeOptions = IdeOptions
  { optPreprocessor :: GHC.ParsedSource -> IdePreprocessedSource
    -- ^ Preprocessor to run over all parsed source trees, generating a list of warnings
    --   and a list of errors, along with a new parse tree.
  , optGhcSession :: Action (FilePath -> Action HscEnvEq)
    -- ^ Setup a GHC session for a given file, e.g. @Foo.hs@.
    --   For the same 'ComponentOptions' from hie-bios, the resulting function will be applied once per file.
    --   It is desirable that many files get the same 'HscEnvEq', so that more IDE features work.
  , optPkgLocationOpts :: IdePkgLocationOptions
    -- ^ How to locate source and @.hie@ files given a module name.
  , optExtensions :: [String]
    -- ^ File extensions to search for code, defaults to Haskell sources (including @.hs@)

  , optThreads :: Int
    -- ^ Number of threads to use. Use 0 for number of threads on the machine.
  , optShakeFiles :: Maybe FilePath
  -- ^ Directory where the shake database should be stored. For ghcide this is always set to `Nothing` for now
  -- meaning we keep everything in memory but the daml CLI compiler uses this for incremental builds.
  , optShakeProfiling :: Maybe FilePath
    -- ^ Set to 'Just' to create a directory of profiling reports.
  , optTesting :: Bool
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
  , optReportProgress :: IdeReportProgress
    -- ^ Whether to report progress during long operations.
  , optLanguageSyntax :: String
    -- ^ the ```language to use
  , optNewColonConvention :: Bool
    -- ^ whether to use new colon convention
  , optKeywords :: [T.Text]
    -- ^ keywords used for completions. These are customizable
    -- since DAML has a different set of keywords than Haskell.
  , optDefer :: IdeDefer
    -- ^ Whether to defer type errors, typed holes and out of scope
    --   variables. Deferral allows the IDE to continue to provide
    --   features such as diagnostics and go-to-definition, in
    --   situations in which they would become unavailable because of
    --   the presence of type errors, holes or unbound variables.
  }

data IdePreprocessedSource = IdePreprocessedSource
  { preprocWarnings :: [(GHC.SrcSpan, String)]
    -- ^ Warnings emitted by the preprocessor.
  , preprocErrors :: [(GHC.SrcSpan, String)]
    -- ^ Errors emitted by the preprocessor.
  , preprocSource :: GHC.ParsedSource
    -- ^ New parse tree emitted by the preprocessor.
  }

newtype IdeReportProgress = IdeReportProgress Bool
newtype IdeDefer          = IdeDefer          Bool

clientSupportsProgress :: LSP.ClientCapabilities -> IdeReportProgress
clientSupportsProgress caps = IdeReportProgress $ Just True ==
    (LSP._workDoneProgress =<< LSP._window (caps :: LSP.ClientCapabilities))

defaultIdeOptions :: Action (FilePath -> Action HscEnvEq) -> IdeOptions
defaultIdeOptions session = IdeOptions
    {optPreprocessor = IdePreprocessedSource [] []
    ,optGhcSession = session
    ,optExtensions = ["hs", "lhs"]
    ,optPkgLocationOpts = defaultIdePkgLocationOptions
    ,optThreads = 0
    ,optShakeFiles = Nothing
    ,optShakeProfiling = Nothing
    ,optReportProgress = IdeReportProgress False
    ,optLanguageSyntax = "haskell"
    ,optNewColonConvention = False
    ,optKeywords = haskellKeywords
    ,optDefer = IdeDefer True
    ,optTesting = False
    }


-- | The set of options used to locate files belonging to external packages.
data IdePkgLocationOptions = IdePkgLocationOptions
  { optLocateHieFile :: PackageConfig -> Module -> IO (Maybe FilePath)
  -- ^ Locate the HIE file for the given module. The PackageConfig can be
  -- used to lookup settings like importDirs.
  , optLocateSrcFile :: PackageConfig -> Module -> IO (Maybe FilePath)
  -- ^ Locate the source file for the given module. The PackageConfig can be
  -- used to lookup settings like importDirs. For DAML, we place them in the package DB.
  -- For cabal this could point somewhere in ~/.cabal/packages.
  }

defaultIdePkgLocationOptions :: IdePkgLocationOptions
defaultIdePkgLocationOptions = IdePkgLocationOptions f f
    where f _ _ = return Nothing

-- | From https://wiki.haskell.org/Keywords
haskellKeywords :: [T.Text]
haskellKeywords =
  [ "as"
  , "case", "of"
  , "class", "instance", "type"
  , "data", "family", "newtype"
  , "default"
  , "deriving"
  , "do", "mdo", "proc", "rec"
  , "forall"
  , "foreign"
  , "hiding"
  , "if", "then", "else"
  , "import", "qualified", "hiding"
  , "infix", "infixl", "infixr"
  , "let", "in", "where"
  , "module"
  ]
