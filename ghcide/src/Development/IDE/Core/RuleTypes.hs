-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.RuleTypes(
    module Development.IDE.Core.RuleTypes
    ) where

import           Control.DeepSeq
import Data.Binary
import           Development.IDE.Import.DependencyInformation
import Development.IDE.GHC.Util
import           Data.Hashable
import           Data.Typeable
import qualified Data.Set as S
import           Development.Shake
import           GHC.Generics                             (Generic)

import           GHC
import Module (InstalledUnitId)
import HscTypes (CgGuts, Linkable, HomeModInfo, ModDetails)
import Development.IDE.GHC.Compat

import           Development.IDE.Spans.Type
import           Development.IDE.Import.FindImports (ArtifactsLocation)


-- NOTATION
--   Foo+ means Foo for the dependencies
--   Foo* means Foo for me and Foo+

-- | The parse tree for the file using GetFileContents
type instance RuleResult GetParsedModule = ParsedModule

-- | The dependency information produced by following the imports recursively.
-- This rule will succeed even if there is an error, e.g., a module could not be located,
-- a module could not be parsed or an import cycle.
type instance RuleResult GetDependencyInformation = DependencyInformation

-- | Transitive module and pkg dependencies based on the information produced by GetDependencyInformation.
-- This rule is also responsible for calling ReportImportCycles for each file in the transitive closure.
type instance RuleResult GetDependencies = TransitiveDependencies

-- | Contains the typechecked module and the OrigNameCache entry for
-- that module.
data TcModuleResult = TcModuleResult
    { tmrModule     :: TypecheckedModule
    , tmrModInfo    :: HomeModInfo
    }
instance Show TcModuleResult where
    show = show . pm_mod_summary . tm_parsed_module . tmrModule

instance NFData TcModuleResult where
    rnf = rwhnf

-- | The type checked version of this file, requires TypeCheck+
type instance RuleResult TypeCheck = TcModuleResult

-- | Information about what spans occur where, requires TypeCheck
type instance RuleResult GetSpanInfo = SpansInfo

-- | Convert to Core, requires TypeCheck*
type instance RuleResult GenerateCore = (SafeHaskellMode, CgGuts, ModDetails)

-- | Generate byte code for template haskell.
type instance RuleResult GenerateByteCode = Linkable

-- | A GHC session that we reuse.
type instance RuleResult GhcSession = HscEnvEq

-- | Resolve the imports in a module to the file path of a module
-- in the same package or the package id of another package.
type instance RuleResult GetLocatedImports = ([(Located ModuleName, Maybe ArtifactsLocation)], S.Set InstalledUnitId)

-- | This rule is used to report import cycles. It depends on GetDependencyInformation.
-- We cannot report the cycles directly from GetDependencyInformation since
-- we can only report diagnostics for the current file.
type instance RuleResult ReportImportCycles = ()

-- | Read the given HIE file.
type instance RuleResult GetHieFile = HieFile


data GetParsedModule = GetParsedModule
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetParsedModule
instance NFData   GetParsedModule
instance Binary   GetParsedModule

data GetLocatedImports = GetLocatedImports
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetLocatedImports
instance NFData   GetLocatedImports
instance Binary   GetLocatedImports

data GetDependencyInformation = GetDependencyInformation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencyInformation
instance NFData   GetDependencyInformation
instance Binary   GetDependencyInformation

data ReportImportCycles = ReportImportCycles
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ReportImportCycles
instance NFData   ReportImportCycles
instance Binary   ReportImportCycles

data GetDependencies = GetDependencies
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencies
instance NFData   GetDependencies
instance Binary   GetDependencies

data TypeCheck = TypeCheck
    deriving (Eq, Show, Typeable, Generic)
instance Hashable TypeCheck
instance NFData   TypeCheck
instance Binary   TypeCheck

data GetSpanInfo = GetSpanInfo
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetSpanInfo
instance NFData   GetSpanInfo
instance Binary   GetSpanInfo

data GenerateCore = GenerateCore
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateCore
instance NFData   GenerateCore
instance Binary   GenerateCore

data GenerateByteCode = GenerateByteCode
   deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateByteCode
instance NFData   GenerateByteCode
instance Binary   GenerateByteCode

data GhcSession = GhcSession
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSession
instance NFData   GhcSession
instance Binary   GhcSession

-- Note that we embed the filepath here instead of using the filepath associated with Shake keys.
-- Otherwise we will garbage collect the result since files in package dependencies will not be declared reachable.
data GetHieFile = GetHieFile FilePath
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHieFile
instance NFData   GetHieFile
instance Binary   GetHieFile
