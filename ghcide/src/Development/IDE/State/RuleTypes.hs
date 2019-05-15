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
import qualified Development.IDE.UtilGHC as Compile
import           Development.IDE.Functions.FindImports         (Import(..))
import           Development.IDE.Functions.DependencyInformation
import           Data.Hashable
import           Data.Typeable
import           Development.Shake                        hiding (Env, newCache)
import           GHC.Generics                             (Generic)

import           GHC
import Development.IDE.Compat
import           Module

import           Development.IDE.Types.SpanInfo


-- NOTATION
--   Foo+ means Foo for the dependencies
--   Foo* means Foo for me and Foo+

-- | Kick off things
type instance RuleResult OfInterest = ()

-- | The parse tree for the file using GetFileContents
type instance RuleResult GetParsedModule = ParsedModule

-- | The dependency information produced by following the imports recursively.
-- This rule will succeed even if there is an error, e.g., a module could not be located,
-- a module could not be parsed or an import cycle.
type instance RuleResult GetDependencyInformation = DependencyInformation

-- | Transitive module and pkg dependencies based on the information produced by GetDependencyInformation.
-- This rule is also responsible for calling ReportImportCycles for each file in the transitive closure.
type instance RuleResult GetDependencies = TransitiveDependencies

-- | The type checked version of this file, requires TypeCheck+
type instance RuleResult TypeCheck = TcModuleResult

-- | The result of loading a module from a package.
type instance RuleResult LoadPackage = LoadPackageResult

-- | Information about what spans occur where, requires TypeCheck
type instance RuleResult GetSpanInfo = [SpanInfo]

-- | Convert to Core, requires TypeCheck*
type instance RuleResult GenerateCore = GhcModule

-- | We capture the subset of `DynFlags` that is computed by package initialization in a rule to
-- make session initialization cheaper by reusing it.
type instance RuleResult LoadPackageState = Compile.PackageDynFlags

-- | Resolve the imports in a module to the list of either external packages or absolute file paths
-- for modules in the same package.
type instance RuleResult GetLocatedImports = [(Located ModuleName, Maybe Import)]

-- | This rule is used to report import cycles. It depends on GetDependencyInformation.
-- We cannot report the cycles directly from GetDependencyInformation since
-- we can only report diagnostics for the current file.
type instance RuleResult ReportImportCycles = ()

-- | Read the given HIE file.
type instance RuleResult GetHieFile = HieFile


data OfInterest = OfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable OfInterest
instance NFData   OfInterest

data GetParsedModule = GetParsedModule
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetParsedModule
instance NFData   GetParsedModule

data GetLocatedImports = GetLocatedImports
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetLocatedImports
instance NFData   GetLocatedImports

data GetDependencyInformation = GetDependencyInformation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencyInformation
instance NFData   GetDependencyInformation

data ReportImportCycles = ReportImportCycles
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ReportImportCycles
instance NFData   ReportImportCycles

data GetDependencies = GetDependencies
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencies
instance NFData   GetDependencies

data TypeCheck = TypeCheck
    deriving (Eq, Show, Typeable, Generic)
instance Hashable TypeCheck
instance NFData   TypeCheck

data LoadPackage = LoadPackage InstalledUnitId
    deriving (Eq, Show, Typeable, Generic)
instance Hashable LoadPackage
instance NFData   LoadPackage

data GetSpanInfo = GetSpanInfo
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetSpanInfo
instance NFData   GetSpanInfo

data GenerateCore = GenerateCore
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateCore
instance NFData   GenerateCore

data LoadPackageState = LoadPackageState
    deriving (Eq, Show, Typeable, Generic)
instance Hashable LoadPackageState
instance NFData   LoadPackageState

-- Note that we embed the filepath here instead of using the filepath associated with Shake keys.
-- Otherwise we will garbage collect the result since files in package dependencies will not be declared reachable.
data GetHieFile = GetHieFile FilePath
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHieFile
instance NFData   GetHieFile

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

instance Hashable InstalledUnitId where
  hashWithSalt salt = hashWithSalt salt . installedUnitIdString

instance Show LoadPackageResult where
  show = installedUnitIdString . lprInstalledUnitId

instance NFData LoadPackageResult where
    rnf = rwhnf

instance Show HieFile where
    show = show . hie_module

instance NFData HieFile where
    rnf = rwhnf
