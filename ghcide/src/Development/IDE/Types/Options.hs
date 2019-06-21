-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}

-- | Options
module Development.IDE.Types.Options
  ( IdeOptions(..)
  , IdePkgLocationOptions(..)
  , defaultIdeOptions
  ) where

import Development.Shake
import           GHC hiding (parseModule, typecheckModule)
import           GhcPlugins                     as GHC hiding (fst3, (<>))


data IdeOptions = IdeOptions
  { optPreprocessor :: GHC.ParsedSource -> ([(GHC.SrcSpan, String)], GHC.ParsedSource)
  , optGhcSession :: Action HscEnv
  -- ^ Setup a GHC session using a given package state. If a `ParsedModule` is supplied,
  -- the import path should be setup for that module.
  , optPkgLocationOpts :: IdePkgLocationOptions
  , optWriteIface :: Bool
  , optExtensions :: [String]

  , optThreads :: Int
  , optShakeProfiling :: Maybe FilePath
  , optLanguageSyntax :: String -- ^ the ```language to use
  , optNewColonConvention :: Bool -- ^ whether to use new colon convention
  }

defaultIdeOptions :: Action HscEnv -> IdeOptions
defaultIdeOptions session = IdeOptions
    {optPreprocessor = (,) []
    ,optWriteIface = False
    ,optGhcSession = session
    ,optExtensions = ["hs"]
    ,optPkgLocationOpts = defaultIdePkgLocationOptions
    ,optThreads = 0
    ,optShakeProfiling = Nothing
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
