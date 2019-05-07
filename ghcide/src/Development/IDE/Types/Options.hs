-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}

-- | Options
module Development.IDE.Types.Options
  ( IdeOptions(..)
  ) where

import Development.IDE.UtilGHC
import           GHC hiding (parseModule, typecheckModule)
import           GhcPlugins                     as GHC hiding (fst3, (<>))


data IdeOptions = IdeOptions
  { optPreprocessor :: GHC.ParsedSource -> ([(GHC.SrcSpan, String)], GHC.ParsedSource)
  , optRunGhcSession :: forall a. Maybe ParsedModule -> PackageDynFlags -> Ghc a -> IO a
  -- ^ Setup a GHC session using a given package state. If a `ParsedModule` is supplied,
  -- the import path should be setup for that module.
  , optWriteIface :: Bool

  , optMbPackageName :: Maybe String

  , optPackageDbs :: [FilePath]
  , optHideAllPkgs :: Bool
  , optPackageImports :: [(String, ModRenaming)]

  , optThreads :: Int
  , optShakeProfiling :: Maybe FilePath
  }
