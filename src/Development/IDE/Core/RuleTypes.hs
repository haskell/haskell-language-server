-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.RuleTypes(
    module Development.IDE.Core.RuleTypes
    ) where

import           Control.DeepSeq
import Data.Aeson.Types (Value)
import Data.Binary
import           Development.IDE.Import.DependencyInformation
import Development.IDE.GHC.Compat hiding (HieFileResult)
import Development.IDE.GHC.Util
import Development.IDE.Core.Shake (KnownTargets)
import           Data.Hashable
import           Data.Typeable
import qualified Data.Set as S
import qualified Data.Map as M
import           Development.Shake
import           GHC.Generics                             (Generic)

import Module (InstalledUnitId)
import HscTypes (ModGuts, hm_iface, HomeModInfo, hm_linkable)

import           Development.IDE.Spans.Common
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Import.FindImports (ArtifactsLocation)
import Data.ByteString (ByteString)
import Language.Haskell.LSP.Types (NormalizedFilePath)
import TcRnMonad (TcGblEnv)
import qualified Data.ByteString.Char8 as BS

data LinkableType = ObjectLinkable | BCOLinkable
  deriving (Eq,Ord,Show)

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

type instance RuleResult GetModuleGraph = DependencyInformation

data GetKnownTargets = GetKnownTargets
  deriving (Show, Generic, Eq, Ord)
instance Hashable GetKnownTargets
instance NFData   GetKnownTargets
instance Binary   GetKnownTargets
type instance RuleResult GetKnownTargets = KnownTargets

-- | Convert to Core, requires TypeCheck*
type instance RuleResult GenerateCore = ModGuts

data GenerateCore = GenerateCore
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateCore
instance NFData   GenerateCore
instance Binary   GenerateCore

data GetImportMap = GetImportMap
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetImportMap
instance NFData   GetImportMap
instance Binary   GetImportMap

type instance RuleResult GetImportMap = ImportMap
newtype ImportMap = ImportMap
  { importMap :: M.Map ModuleName NormalizedFilePath -- ^ Where are the modules imported by this file located?
  } deriving stock Show
    deriving newtype NFData

-- | Contains the typechecked module and the OrigNameCache entry for
-- that module.
data TcModuleResult = TcModuleResult
    { tmrParsed :: ParsedModule
    , tmrRenamed :: RenamedSource
    , tmrTypechecked :: TcGblEnv
    , tmrDeferedError :: !Bool -- ^ Did we defer any type errors for this module?
    }
instance Show TcModuleResult where
    show = show . pm_mod_summary . tmrParsed

instance NFData TcModuleResult where
    rnf = rwhnf

tmrModSummary :: TcModuleResult -> ModSummary
tmrModSummary = pm_mod_summary . tmrParsed

data HiFileResult = HiFileResult
    { hirModSummary :: !ModSummary
    -- Bang patterns here are important to stop the result retaining
    -- a reference to a typechecked module
    , hirHomeMod :: !HomeModInfo
    -- ^ Includes the Linkable iff we need object files
    }

hiFileFingerPrint :: HiFileResult -> ByteString
hiFileFingerPrint hfr = ifaceBS <> linkableBS
  where
    ifaceBS = fingerprintToBS . getModuleHash . hirModIface $ hfr -- will always be two bytes
    linkableBS = case hm_linkable $ hirHomeMod hfr of
      Nothing -> ""
      Just l -> BS.pack $ show $ linkableTime l

hirModIface :: HiFileResult -> ModIface
hirModIface = hm_iface . hirHomeMod

instance NFData HiFileResult where
    rnf = rwhnf

instance Show HiFileResult where
    show = show . hirModSummary

-- | Save the uncompressed AST here, we compress it just before writing to disk
data HieAstResult
  = HAR
  { hieModule :: Module
  , hieAst :: !(HieASTs Type)
  , refMap :: RefMap
  -- ^ Lazy because its value only depends on the hieAst, which is bundled in this type
  -- Lazyness can't cause leaks here because the lifetime of `refMap` will be the same
  -- as that of `hieAst`
  }
 
instance NFData HieAstResult where
    rnf (HAR m hf _rm) = rnf m `seq` rwhnf hf
 
instance Show HieAstResult where
    show = show . hieModule

-- | The type checked version of this file, requires TypeCheck+
type instance RuleResult TypeCheck = TcModuleResult

-- | The uncompressed HieAST
type instance RuleResult GetHieAst = HieAstResult

-- | A IntervalMap telling us what is in scope at each point
type instance RuleResult GetBindings = Bindings

data DocAndKindMap = DKMap {getDocMap :: !DocMap, getKindMap :: !KindMap}
instance NFData DocAndKindMap where
    rnf (DKMap a b) = rwhnf a `seq` rwhnf b

instance Show DocAndKindMap where
    show = const "docmap"

type instance RuleResult GetDocMap = DocAndKindMap

-- | A GHC session that we reuse.
type instance RuleResult GhcSession = HscEnvEq

-- | A GHC session preloaded with all the dependencies
type instance RuleResult GhcSessionDeps = HscEnvEq

-- | Resolve the imports in a module to the file path of a module
-- in the same package or the package id of another package.
type instance RuleResult GetLocatedImports = ([(Located ModuleName, Maybe ArtifactsLocation)], S.Set InstalledUnitId)

-- | This rule is used to report import cycles. It depends on GetDependencyInformation.
-- We cannot report the cycles directly from GetDependencyInformation since
-- we can only report diagnostics for the current file.
type instance RuleResult ReportImportCycles = ()

-- | Read the module interface file from disk. Throws an error for VFS files.
--   This is an internal rule, use 'GetModIface' instead.
type instance RuleResult GetModIfaceFromDisk = HiFileResult

-- | Get a module interface details, either from an interface file or a typechecked module
type instance RuleResult GetModIface = HiFileResult

-- | Get a module interface details, without the Linkable
-- For better early cuttoff
type instance RuleResult GetModIfaceWithoutLinkable = HiFileResult

data FileOfInterestStatus = OnDisk | Modified
  deriving (Eq, Show, Typeable, Generic)
instance Hashable FileOfInterestStatus
instance NFData   FileOfInterestStatus
instance Binary   FileOfInterestStatus

data IsFileOfInterestResult = NotFOI | IsFOI FileOfInterestStatus
  deriving (Eq, Show, Typeable, Generic)
instance Hashable IsFileOfInterestResult
instance NFData   IsFileOfInterestResult
instance Binary   IsFileOfInterestResult

type instance RuleResult IsFileOfInterest = IsFileOfInterestResult

-- | Generate a ModSummary that has enough information to be used to get .hi and .hie files.
-- without needing to parse the entire source
type instance RuleResult GetModSummary = (ModSummary,[LImportDecl GhcPs])

-- | Generate a ModSummary with the timestamps elided,
--   for more successful early cutoff
type instance RuleResult GetModSummaryWithoutTimestamps = (ModSummary,[LImportDecl GhcPs])

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

-- | Does this module need to be compiled?
type instance RuleResult NeedsCompilation = Bool

data NeedsCompilation = NeedsCompilation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable NeedsCompilation
instance NFData   NeedsCompilation
instance Binary   NeedsCompilation

data GetDependencyInformation = GetDependencyInformation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencyInformation
instance NFData   GetDependencyInformation
instance Binary   GetDependencyInformation

data GetModuleGraph = GetModuleGraph
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModuleGraph
instance NFData   GetModuleGraph
instance Binary   GetModuleGraph

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

data GetDocMap = GetDocMap
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDocMap
instance NFData   GetDocMap
instance Binary   GetDocMap

data GetHieAst = GetHieAst
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHieAst
instance NFData   GetHieAst
instance Binary   GetHieAst

data GetBindings = GetBindings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetBindings
instance NFData   GetBindings
instance Binary   GetBindings

data GhcSession = GhcSession
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSession
instance NFData   GhcSession
instance Binary   GhcSession

data GhcSessionDeps = GhcSessionDeps deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSessionDeps
instance NFData   GhcSessionDeps
instance Binary   GhcSessionDeps

data GetModIfaceFromDisk = GetModIfaceFromDisk
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIfaceFromDisk
instance NFData   GetModIfaceFromDisk
instance Binary   GetModIfaceFromDisk

data GetModIface = GetModIface
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIface
instance NFData   GetModIface
instance Binary   GetModIface

data GetModIfaceWithoutLinkable = GetModIfaceWithoutLinkable
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIfaceWithoutLinkable
instance NFData   GetModIfaceWithoutLinkable
instance Binary   GetModIfaceWithoutLinkable

data IsFileOfInterest = IsFileOfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable IsFileOfInterest
instance NFData   IsFileOfInterest
instance Binary   IsFileOfInterest

data GetModSummaryWithoutTimestamps = GetModSummaryWithoutTimestamps
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModSummaryWithoutTimestamps
instance NFData   GetModSummaryWithoutTimestamps
instance Binary   GetModSummaryWithoutTimestamps

data GetModSummary = GetModSummary
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModSummary
instance NFData   GetModSummary
instance Binary   GetModSummary

-- | Get the vscode client settings stored in the ide state
data GetClientSettings = GetClientSettings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetClientSettings
instance NFData   GetClientSettings
instance Binary   GetClientSettings

type instance RuleResult GetClientSettings = Hashed (Maybe Value)
