-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}

-- | Options
module Development.IDE.Types.Options
  ( IdeOptions(..)
  , IdeReportProgress(..)
  , clientSupportsProgress
  , IdePkgLocationOptions(..)
  , defaultIdeOptions
  ) where

import Data.Maybe
import Development.Shake
import           GHC hiding (parseModule, typecheckModule)
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified Language.Haskell.LSP.Types.Capabilities as LSP

data IdeOptions = IdeOptions
  { optPreprocessor :: GHC.ParsedSource -> ([(GHC.SrcSpan, String)], GHC.ParsedSource)
  , optGhcSession :: Action HscEnv
  -- ^ Setup a GHC session using a given package state. If a `ParsedModule` is supplied,
  -- the import path should be setup for that module.
  , optPkgLocationOpts :: IdePkgLocationOptions
  , optExtensions :: [String]

  , optThreads :: Int
  , optShakeProfiling :: Maybe FilePath
  , optReportProgress :: IdeReportProgress
  , optLanguageSyntax :: String -- ^ the ```language to use
  , optNewColonConvention :: Bool -- ^ whether to use new colon convention
  }

newtype IdeReportProgress = IdeReportProgress Bool

clientSupportsProgress :: LSP.ClientCapabilities -> IdeReportProgress
clientSupportsProgress caps = IdeReportProgress $ fromMaybe False $
    LSP._progress =<< LSP._window (caps :: LSP.ClientCapabilities)

defaultIdeOptions :: Action HscEnv -> IdeOptions
defaultIdeOptions session = IdeOptions
    {optPreprocessor = (,) []
    ,optGhcSession = session
    ,optExtensions = ["hs"]
    ,optPkgLocationOpts = defaultIdePkgLocationOptions
    ,optThreads = 0
    ,optShakeProfiling = Nothing
    ,optReportProgress = IdeReportProgress False
    ,optLanguageSyntax = "haskell"
    ,optNewColonConvention = False
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
