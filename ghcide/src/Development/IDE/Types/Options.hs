-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | Options
{-# LANGUAGE RankNTypes #-}
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
  , OptHaddockParse(..)
  , ProgressReportingStyle(..)
  ,optShakeFiles) where

import qualified Data.Text                         as T
import           Data.Typeable
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Types.Diagnostics
import           Development.Shake
import           GHC                               hiding (parseModule,
                                                    typecheckModule)
import           GhcPlugins                        as GHC hiding (fst3, (<>))
import           Ide.Plugin.Config
import qualified Language.LSP.Types.Capabilities   as LSP

data IdeOptions = IdeOptions
  { optPreprocessor       :: GHC.ParsedSource -> IdePreprocessedSource
    -- ^ Preprocessor to run over all parsed source trees, generating a list of warnings
    --   and a list of errors, along with a new parse tree.
  , optGhcSession         :: Action IdeGhcSession
    -- ^ Setup a GHC session for a given file, e.g. @Foo.hs@.
    --   For the same 'ComponentOptions' from hie-bios, the resulting function will be applied once per file.
    --   It is desirable that many files get the same 'HscEnvEq', so that more IDE features work.
  , optPkgLocationOpts    :: IdePkgLocationOptions
    -- ^ How to locate source and @.hie@ files given a module name.
  , optExtensions         :: [String]
    -- ^ File extensions to search for code, defaults to Haskell sources (including @.hs@)
  , optShakeProfiling     :: Maybe FilePath
    -- ^ Set to 'Just' to create a directory of profiling reports.
  , optOTMemoryProfiling  :: IdeOTMemoryProfiling
    -- ^ Whether to record profiling information with OpenTelemetry. You must
    --   also enable the -l RTS flag for this to have any effect
  , optTesting            :: IdeTesting
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
  , optReportProgress     :: IdeReportProgress
    -- ^ Whether to report progress during long operations.
  , optLanguageSyntax     :: String
    -- ^ the ```language to use
  , optNewColonConvention :: Bool
    -- ^ whether to use new colon convention
  , optKeywords           :: [T.Text]
    -- ^ keywords used for completions. These are customizable
    -- since DAML has a different set of keywords than Haskell.
  , optDefer              :: IdeDefer
    -- ^ Whether to defer type errors, typed holes and out of scope
    --   variables. Deferral allows the IDE to continue to provide
    --   features such as diagnostics and go-to-definition, in
    --   situations in which they would become unavailable because of
    --   the presence of type errors, holes or unbound variables.
  , optCheckProject       :: IO Bool
    -- ^ Whether to typecheck the entire project on load
  , optCheckParents       :: IO CheckParents
    -- ^ When to typecheck reverse dependencies of a file
  , optHaddockParse       :: OptHaddockParse
    -- ^ Whether to return result of parsing module with Opt_Haddock.
    --   Otherwise, return the result of parsing without Opt_Haddock, so
    --   that the parsed module contains the result of Opt_KeepRawTokenStream,
    --   which might be necessary for hlint.
  , optCustomDynFlags     :: DynFlags -> DynFlags
    -- ^ Will be called right after setting up a new cradle,
    --   allowing to customize the Ghc options used
  , optShakeOptions       :: ShakeOptions
  , optSkipProgress       :: forall a. Typeable a => a -> Bool
      -- ^ Predicate to select which rule keys to exclude from progress reporting.
  , optProgressStyle      :: ProgressReportingStyle
  }

optShakeFiles :: IdeOptions -> Maybe FilePath
optShakeFiles opts
  | value == defValue = Nothing
  | otherwise = Just value
  where
    value = shakeFiles (optShakeOptions opts)
    defValue = shakeFiles (optShakeOptions $ defaultIdeOptions undefined)
data OptHaddockParse = HaddockParse | NoHaddockParse
  deriving (Eq,Ord,Show,Enum)

data IdePreprocessedSource = IdePreprocessedSource
  { preprocWarnings :: [(GHC.SrcSpan, String)]
    -- ^ Warnings emitted by the preprocessor.
  , preprocErrors   :: [(GHC.SrcSpan, String)]
    -- ^ Errors emitted by the preprocessor.
  , preprocSource   :: GHC.ParsedSource
    -- ^ New parse tree emitted by the preprocessor.
  }

newtype IdeReportProgress    = IdeReportProgress Bool
newtype IdeDefer             = IdeDefer          Bool
newtype IdeTesting           = IdeTesting        Bool
newtype IdeOTMemoryProfiling = IdeOTMemoryProfiling    Bool

data ProgressReportingStyle
    = Percentage -- ^ Report using the LSP @_percentage@ field
    | Explicit   -- ^ Report using explicit 123/456 text
    | NoProgress -- ^ Do not report any percentage


clientSupportsProgress :: LSP.ClientCapabilities -> IdeReportProgress
clientSupportsProgress caps = IdeReportProgress $ Just True ==
    (LSP._workDoneProgress =<< LSP._window (caps :: LSP.ClientCapabilities))

defaultIdeOptions :: Action IdeGhcSession -> IdeOptions
defaultIdeOptions session = IdeOptions
    {optPreprocessor = IdePreprocessedSource [] []
    ,optGhcSession = session
    ,optExtensions = ["hs", "lhs"]
    ,optPkgLocationOpts = defaultIdePkgLocationOptions
    ,optShakeOptions = shakeOptions
        {shakeThreads = 0
        ,shakeFiles = "/dev/null"
        }
    ,optShakeProfiling = Nothing
    ,optOTMemoryProfiling = IdeOTMemoryProfiling False
    ,optReportProgress = IdeReportProgress False
    ,optLanguageSyntax = "haskell"
    ,optNewColonConvention = False
    ,optKeywords = haskellKeywords
    ,optDefer = IdeDefer True
    ,optTesting = IdeTesting False
    ,optCheckProject = pure True
    ,optCheckParents = pure CheckOnSaveAndClose
    ,optHaddockParse = HaddockParse
    ,optCustomDynFlags = id
    ,optSkipProgress = defaultSkipProgress
    ,optProgressStyle = Explicit
    }

defaultSkipProgress :: Typeable a => a -> Bool
defaultSkipProgress key = case () of
    -- don't do progress for GetFileContents as it's cheap
    _ | Just GetFileContents <- cast key        -> True
    -- don't do progress for GetFileExists, as there are lots of redundant nodes
    -- (normally there is one node per file, but this is not the case for GetFileExists)
    _ | Just GetFileExists <- cast key          -> True
    -- don't do progress for GetModificationTime as there are lot of redundant nodes
    -- (for the interface files)
    _ | Just GetModificationTime_{} <- cast key -> True
    _                                           -> False


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
