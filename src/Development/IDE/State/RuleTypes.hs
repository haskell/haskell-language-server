-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.State.RuleTypes(
    module Development.IDE.State.RuleTypes
    ) where

import           Control.DeepSeq
import           Development.IDE.Functions.Compile             (TcModuleResult, GhcModule, LoadPackageResult(..))
import qualified Development.IDE.Functions.Compile             as Compile
import           Development.IDE.Functions.FindImports         (Import(..))
import           Development.IDE.Functions.DependencyInformation
import           Data.Binary                              (Binary)
import qualified Data.Binary                              as Binary
import           Data.Hashable
import           Data.Typeable
import           Development.Shake                        hiding (Env, newCache)
import           GHC.Generics                             (Generic)

import           "ghc-lib" GHC
import           "ghc-lib-parser" Module

import           Development.IDE.State.Shake
import           Development.IDE.Types.SpanInfo


-- NOTATION
--   Foo+ means Foo for the dependencies
--   Foo* means Foo for me and Foo+

-- | Kick off things
type instance RuleResult OfInterest = IdeResult ()

-- | The parse tree for the file using GetFileContents
type instance RuleResult GetParsedModule = IdeResult ParsedModule

-- | The dependency information produced by following the imports recursively.
-- This rule will succeed even if there is an error, e.g., a module could not be located,
-- a module could not be parsed or an import cycle.
type instance RuleResult GetDependencyInformation = IdeResult DependencyInformation

-- | Transitive module and pkg dependencies based on the information produced by GetDependencyInformation.
-- This rule is also responsible for calling ReportImportCycles for each file in the transitive closure.
type instance RuleResult GetDependencies = IdeResult TransitiveDependencies

-- | The type checked version of this file, requires TypeCheck+
type instance RuleResult TypeCheck = IdeResult TcModuleResult

-- | The result of loading a module from a package.
type instance RuleResult LoadPackage = IdeResult LoadPackageResult

-- | Information about what spans occur where, requires TypeCheck
type instance RuleResult GetSpanInfo = IdeResult [SpanInfo]

-- | Convert to Core, requires TypeCheck*
type instance RuleResult GenerateCore = IdeResult GhcModule

-- | We capture the subset of `DynFlags` that is computed by package initialization in a rule to
-- make session initialization cheaper by reusing it.
type instance RuleResult GeneratePackageState = IdeResult Compile.PackageState

-- | Resolve the imports in a module to the list of either external packages or absolute file paths
-- for modules in the same package.
type instance RuleResult GetLocatedImports = IdeResult [(Located ModuleName, Maybe Import)]

-- | This rule is used to report import cycles. It depends on GetDependencyInformation.
-- We cannot report the cycles directly from GetDependencyInformation since
-- we can only report diagnostics for the current file.
type instance RuleResult ReportImportCycles = IdeResult ()


data OfInterest = OfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Binary   OfInterest
instance Hashable OfInterest
instance NFData   OfInterest

data GetParsedModule = GetParsedModule
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GetParsedModule
instance Hashable GetParsedModule
instance NFData   GetParsedModule

data GetLocatedImports = GetLocatedImports
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GetLocatedImports
instance Hashable GetLocatedImports
instance NFData   GetLocatedImports

data GetDependencyInformation = GetDependencyInformation
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GetDependencyInformation
instance Hashable GetDependencyInformation
instance NFData   GetDependencyInformation

data ReportImportCycles = ReportImportCycles
    deriving (Eq, Show, Typeable, Generic)
instance Binary   ReportImportCycles
instance Hashable ReportImportCycles
instance NFData   ReportImportCycles

data GetDependencies = GetDependencies
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GetDependencies
instance Hashable GetDependencies
instance NFData   GetDependencies

data TypeCheck = TypeCheck
    deriving (Eq, Show, Typeable, Generic)
instance Binary   TypeCheck
instance Hashable TypeCheck
instance NFData   TypeCheck

data LoadPackage = LoadPackage InstalledUnitId
    deriving (Eq, Show, Typeable, Generic)
instance Binary   LoadPackage
instance Hashable LoadPackage
instance NFData   LoadPackage

data GetSpanInfo = GetSpanInfo
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GetSpanInfo
instance Hashable GetSpanInfo
instance NFData   GetSpanInfo

data GenerateCore = GenerateCore
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GenerateCore
instance Hashable GenerateCore
instance NFData   GenerateCore

data GeneratePackageState = GeneratePackageState [FilePath] Bool [(String, [(String, String)])]
    deriving (Eq, Show, Typeable, Generic)
instance Binary   GeneratePackageState
instance Hashable GeneratePackageState
instance NFData   GeneratePackageState

------------------------------------------------------------
-- Orphan Instances

instance NFData (GenLocated SrcSpan ModuleName) where
    rnf = rwhnf

instance Show TcModuleResult where
    show = show . pm_mod_summary . tm_parsed_module . Compile.tmrModule

instance NFData TcModuleResult where
    rnf = rwhnf

instance Show ModSummary where
    show = show . ms_mod

instance Show ParsedModule where
    show = show . pm_mod_summary

instance NFData ModSummary where
    rnf = rwhnf

instance NFData ParsedModule where
    rnf = rwhnf

instance NFData SpanInfo where
    rnf = rwhnf

instance NFData Import where
  rnf = rwhnf

instance Binary InstalledUnitId where
  get = fmap stringToInstalledUnitId Binary.get
  put = Binary.put . installedUnitIdString

instance Hashable InstalledUnitId where
  hashWithSalt salt = hashWithSalt salt . installedUnitIdString

instance Show LoadPackageResult where
  show = installedUnitIdString . lprInstalledUnitId

instance NFData LoadPackageResult where
    rnf = rwhnf
