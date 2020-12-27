-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{- HLINT ignore "Avoid restricted extensions" -}

-- | Options
module Development.IDE.Types.Options
  ( IdeOptions(..)
  , IdePreprocessedSource(..)
  , IdeReportProgress(..)
  , IdeDefer(..)
  , IdeTesting(..)
  , IdeOTMemoryProfiling(..)
  , clientSupportsProgress
  , IdePkgLocationOptions(..)
  , defaultIdeOptions
  , IdeResult
  , IdeGhcSession(..)
  , LspConfig(..)
  , defaultLspConfig
  , CheckProject(..)
  , CheckParents(..)
  , OptHaddockParse(..)
  ) where

import Development.Shake
import Development.IDE.GHC.Util
import           GHC hiding (parseModule, typecheckModule)
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified Language.Haskell.LSP.Types.Capabilities as LSP
import qualified Data.Text as T
import Development.IDE.Types.Diagnostics
import Control.DeepSeq (NFData(..))
import Data.Aeson
import GHC.Generics

data IdeGhcSession = IdeGhcSession
  { loadSessionFun :: FilePath -> IO (IdeResult HscEnvEq, [FilePath])
  -- ^ Returns the Ghc session and the cradle dependencies
  , sessionVersion :: !Int
  -- ^ Used as Shake key, versions must be unique and not reused
  }

instance Show IdeGhcSession where show _ = "IdeGhcSession"
instance NFData IdeGhcSession where rnf !_ = ()

data IdeOptions = IdeOptions
  { optPreprocessor :: GHC.ParsedSource -> IdePreprocessedSource
    -- ^ Preprocessor to run over all parsed source trees, generating a list of warnings
    --   and a list of errors, along with a new parse tree.
  , optGhcSession :: Action IdeGhcSession
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
  , optOTMemoryProfiling :: IdeOTMemoryProfiling
    -- ^ Whether to record profiling information with OpenTelemetry. You must
    --   also enable the -l RTS flag for this to have any effect
  , optTesting :: IdeTesting
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
  , optCheckProject :: CheckProject
    -- ^ Whether to typecheck the entire project on load
  , optCheckParents :: CheckParents
    -- ^ When to typecheck reverse dependencies of a file
  , optHaddockParse :: OptHaddockParse
    -- ^ Whether to return result of parsing module with Opt_Haddock.
    --   Otherwise, return the result of parsing without Opt_Haddock, so
    --   that the parsed module contains the result of Opt_KeepRawTokenStream,
    --   which might be necessary for hlint.
  , optCustomDynFlags :: DynFlags -> DynFlags
    -- ^ Will be called right after setting up a new cradle,
    --   allowing to customize the Ghc options used
  }

data OptHaddockParse = HaddockParse | NoHaddockParse
  deriving (Eq,Ord,Show,Enum)

newtype CheckProject = CheckProject { shouldCheckProject :: Bool }
  deriving stock (Eq, Ord, Show)
  deriving newtype (FromJSON,ToJSON)
data CheckParents
    -- Note that ordering of constructors is meaningful and must be monotonically
    -- increasing in the scenarios where parents are checked
    = NeverCheck
    | CheckOnClose
    | CheckOnSaveAndClose
    | AlwaysCheck
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LspConfig
  = LspConfig
  { checkParents :: CheckParents
  , checkProject :: CheckProject
  } deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultLspConfig :: LspConfig
defaultLspConfig = LspConfig CheckOnSaveAndClose (CheckProject True)

data IdePreprocessedSource = IdePreprocessedSource
  { preprocWarnings :: [(GHC.SrcSpan, String)]
    -- ^ Warnings emitted by the preprocessor.
  , preprocErrors :: [(GHC.SrcSpan, String)]
    -- ^ Errors emitted by the preprocessor.
  , preprocSource :: GHC.ParsedSource
    -- ^ New parse tree emitted by the preprocessor.
  }

newtype IdeReportProgress    = IdeReportProgress Bool
newtype IdeDefer             = IdeDefer          Bool
newtype IdeTesting           = IdeTesting        Bool
newtype IdeOTMemoryProfiling = IdeOTMemoryProfiling    Bool

clientSupportsProgress :: LSP.ClientCapabilities -> IdeReportProgress
clientSupportsProgress caps = IdeReportProgress $ Just True ==
    (LSP._workDoneProgress =<< LSP._window (caps :: LSP.ClientCapabilities))

defaultIdeOptions :: Action IdeGhcSession -> IdeOptions
defaultIdeOptions session = IdeOptions
    {optPreprocessor = IdePreprocessedSource [] []
    ,optGhcSession = session
    ,optExtensions = ["hs", "lhs"]
    ,optPkgLocationOpts = defaultIdePkgLocationOptions
    ,optThreads = 0
    ,optShakeFiles = Nothing
    ,optShakeProfiling = Nothing
    ,optOTMemoryProfiling = IdeOTMemoryProfiling False
    ,optReportProgress = IdeReportProgress False
    ,optLanguageSyntax = "haskell"
    ,optNewColonConvention = False
    ,optKeywords = haskellKeywords
    ,optDefer = IdeDefer True
    ,optTesting = IdeTesting False
    ,optCheckProject = checkProject defaultLspConfig
    ,optCheckParents = checkParents defaultLspConfig
    ,optHaddockParse = HaddockParse
    ,optCustomDynFlags = id
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
