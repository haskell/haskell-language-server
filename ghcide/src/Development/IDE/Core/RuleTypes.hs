-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.RuleTypes(
    module Development.IDE.Core.RuleTypes
    ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson.Types                             (Value)
import           Data.Hashable
import qualified Data.Map                                     as M
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Development.IDE.GHC.Compat                   hiding
                                                              (HieFileResult)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Util
import           Development.IDE.Graph
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.HscEnvEq               (HscEnvEq)
import           Development.IDE.Types.KnownTargets
import           GHC.Generics                                 (Generic)

import qualified Data.Binary                                  as B
import           Data.ByteString                              (ByteString)
import qualified Data.ByteString.Lazy                         as LBS
import           Data.Text                                    (Text)
import           Data.Time
import           Development.IDE.Import.FindImports           (ArtifactsLocation)
import           Development.IDE.Spans.Common
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.Diagnostics
import           GHC.Serialized                               (Serialized)
import           Language.LSP.Types                           (NormalizedFilePath)

data LinkableType = ObjectLinkable | BCOLinkable
  deriving (Eq,Ord,Show, Generic)
instance Hashable LinkableType
instance NFData   LinkableType

-- | Encode the linkable into an ordered bytestring.
--   This is used to drive an ordered "newness" predicate in the
--   'NeedsCompilation' build rule.
encodeLinkableType :: Maybe LinkableType -> ByteString
encodeLinkableType Nothing               = "0"
encodeLinkableType (Just BCOLinkable)    = "1"
encodeLinkableType (Just ObjectLinkable) = "2"

-- NOTATION
--   Foo+ means Foo for the dependencies
--   Foo* means Foo for me and Foo+

-- | The parse tree for the file using GetFileContents
type instance RuleResult GetParsedModule = ParsedModule

-- | The parse tree for the file using GetFileContents,
-- all comments included using Opt_KeepRawTokenStream
type instance RuleResult GetParsedModuleWithComments = ParsedModule

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
type instance RuleResult GetKnownTargets = KnownTargets

-- | Convert to Core, requires TypeCheck*
type instance RuleResult GenerateCore = ModGuts

data GenerateCore = GenerateCore
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GenerateCore
instance NFData   GenerateCore

data GetImportMap = GetImportMap
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetImportMap
instance NFData   GetImportMap

type instance RuleResult GetImportMap = ImportMap
newtype ImportMap = ImportMap
  { importMap :: M.Map ModuleName NormalizedFilePath -- ^ Where are the modules imported by this file located?
  } deriving stock Show
    deriving newtype NFData

data Splices = Splices
    { exprSplices :: [(LHsExpr GhcTc, LHsExpr GhcPs)]
    , patSplices  :: [(LHsExpr GhcTc, LPat GhcPs)]
    , typeSplices :: [(LHsExpr GhcTc, LHsType GhcPs)]
    , declSplices :: [(LHsExpr GhcTc, [LHsDecl GhcPs])]
    , awSplices   :: [(LHsExpr GhcTc, Serialized)]
    }

instance Semigroup Splices where
    Splices e p t d aw <> Splices e' p' t' d' aw' =
        Splices
            (e <> e')
            (p <> p')
            (t <> t')
            (d <> d')
            (aw <> aw')

instance Monoid Splices where
    mempty = Splices mempty mempty mempty mempty mempty

instance NFData Splices where
    rnf Splices {..} =
        liftRnf rwhnf exprSplices `seq`
        liftRnf rwhnf patSplices `seq`
        liftRnf rwhnf typeSplices `seq` liftRnf rwhnf declSplices `seq` ()

-- | Contains the typechecked module and the OrigNameCache entry for
-- that module.
data TcModuleResult = TcModuleResult
    { tmrParsed          :: ParsedModule
    , tmrRenamed         :: RenamedSource
    , tmrTypechecked     :: TcGblEnv
    , tmrTopLevelSplices :: Splices
    -- ^ Typechecked splice information
    , tmrDeferedError    :: !Bool
    -- ^ Did we defer any type errors for this module?
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
    , hirHomeMod    :: !HomeModInfo
    -- ^ Includes the Linkable iff we need object files
    , hirIfaceFp    :: ByteString
    -- ^ Fingerprint for the ModIface
    , hirLinkableFp :: ByteString
    -- ^ Fingerprint for the Linkable
    }

hiFileFingerPrint :: HiFileResult -> ByteString
hiFileFingerPrint HiFileResult{..} = hirIfaceFp <> hirLinkableFp

mkHiFileResult :: ModSummary -> HomeModInfo -> HiFileResult
mkHiFileResult hirModSummary hirHomeMod = HiFileResult{..}
  where
    hirIfaceFp = fingerprintToBS . getModuleHash . hm_iface $ hirHomeMod -- will always be two bytes
    hirLinkableFp = case hm_linkable hirHomeMod of
      Nothing -> ""
      Just (linkableTime -> l)  -> LBS.toStrict $
        B.encode (fromEnum $ utctDay l, fromEnum $ utctDayTime l)

hirModIface :: HiFileResult -> ModIface
hirModIface = hm_iface . hirHomeMod

instance NFData HiFileResult where
    rnf = rwhnf

instance Show HiFileResult where
    show = show . hirModSummary

-- | Save the uncompressed AST here, we compress it just before writing to disk
data HieAstResult
  = forall a. HAR
  { hieModule :: Module
  , hieAst    :: !(HieASTs a)
  , refMap    :: RefMap a
  -- ^ Lazy because its value only depends on the hieAst, which is bundled in this type
  -- Lazyness can't cause leaks here because the lifetime of `refMap` will be the same
  -- as that of `hieAst`
  , typeRefs  :: M.Map Name [RealSrcSpan]
  -- ^ type references in this file
  , hieKind   :: !(HieKind a)
  -- ^ Is this hie file loaded from the disk, or freshly computed?
  }

data HieKind a where
  HieFromDisk :: !HieFile -> HieKind TypeIndex
  HieFresh :: HieKind Type

instance NFData (HieKind a) where
    rnf (HieFromDisk hf) = rnf hf
    rnf HieFresh         = ()

instance NFData HieAstResult where
    rnf (HAR m hf _rm _tr kind) = rnf m `seq` rwhnf hf `seq` rnf kind

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

-- | Resolve the imports in a module to the file path of a module in the same package
type instance RuleResult GetLocatedImports = [(Located ModuleName, Maybe ArtifactsLocation)]

-- | This rule is used to report import cycles. It depends on GetDependencyInformation.
-- We cannot report the cycles directly from GetDependencyInformation since
-- we can only report diagnostics for the current file.
type instance RuleResult ReportImportCycles = ()

-- | Read the module interface file from disk. Throws an error for VFS files.
--   This is an internal rule, use 'GetModIface' instead.
type instance RuleResult GetModIfaceFromDisk = HiFileResult

-- | GetModIfaceFromDisk and index the `.hie` file into the database.
--   This is an internal rule, use 'GetModIface' instead.
type instance RuleResult GetModIfaceFromDiskAndIndex = HiFileResult

-- | Get a module interface details, either from an interface file or a typechecked module
type instance RuleResult GetModIface = HiFileResult

-- | Get a module interface details, without the Linkable
-- For better early cuttoff
type instance RuleResult GetModIfaceWithoutLinkable = HiFileResult

-- | Get the contents of a file, either dirty (if the buffer is modified) or Nothing to mean use from disk.
type instance RuleResult GetFileContents = (FileVersion, Maybe Text)

type instance RuleResult GetFileExists = Bool

type instance RuleResult AddWatchedFile = Bool


-- The Shake key type for getModificationTime queries
newtype GetModificationTime = GetModificationTime_
    { missingFileDiagnostics :: Bool
      -- ^ If false, missing file diagnostics are not reported
    }
    deriving (Generic)

instance Show GetModificationTime where
    show _ = "GetModificationTime"

instance Eq GetModificationTime where
    -- Since the diagnostics are not part of the answer, the query identity is
    -- independent from the 'missingFileDiagnostics' field
    _ == _ = True

instance Hashable GetModificationTime where
    -- Since the diagnostics are not part of the answer, the query identity is
    -- independent from the 'missingFileDiagnostics' field
    hashWithSalt salt _ = salt

instance NFData   GetModificationTime

pattern GetModificationTime :: GetModificationTime
pattern GetModificationTime = GetModificationTime_ {missingFileDiagnostics=True}

-- | Get the modification time of a file.
type instance RuleResult GetModificationTime = FileVersion

data FileVersion
    = VFSVersion !Int
    | ModificationTime !POSIXTime
    deriving (Show, Generic)

instance NFData FileVersion

vfsVersion :: FileVersion -> Maybe Int
vfsVersion (VFSVersion i)     = Just i
vfsVersion ModificationTime{} = Nothing

data GetFileContents = GetFileContents
    deriving (Eq, Show, Generic)
instance Hashable GetFileContents
instance NFData   GetFileContents

data GetFileExists = GetFileExists
    deriving (Eq, Show, Typeable, Generic)

instance NFData   GetFileExists
instance Hashable GetFileExists

data FileOfInterestStatus
  = OnDisk
  | Modified { firstOpen :: !Bool -- ^ was this file just opened
             }
  deriving (Eq, Show, Typeable, Generic)
instance Hashable FileOfInterestStatus
instance NFData   FileOfInterestStatus

data IsFileOfInterestResult = NotFOI | IsFOI FileOfInterestStatus
  deriving (Eq, Show, Typeable, Generic)
instance Hashable IsFileOfInterestResult
instance NFData   IsFileOfInterestResult

type instance RuleResult IsFileOfInterest = IsFileOfInterestResult

data ModSummaryResult = ModSummaryResult
  { msrModSummary  :: !ModSummary
  , msrImports     :: [LImportDecl GhcPs]
  , msrFingerprint :: !Fingerprint
  }

instance Show ModSummaryResult where
    show _ = "<ModSummaryResult>"
instance NFData ModSummaryResult where
    rnf ModSummaryResult{..} =
        rnf msrModSummary `seq` rnf msrImports `seq` rnf msrFingerprint

-- | Generate a ModSummary that has enough information to be used to get .hi and .hie files.
-- without needing to parse the entire source
type instance RuleResult GetModSummary = ModSummaryResult

-- | Generate a ModSummary with the timestamps and preprocessed content elided, for more successful early cutoff
type instance RuleResult GetModSummaryWithoutTimestamps = ModSummaryResult

data GetParsedModule = GetParsedModule
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetParsedModule
instance NFData   GetParsedModule

data GetParsedModuleWithComments = GetParsedModuleWithComments
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetParsedModuleWithComments
instance NFData   GetParsedModuleWithComments

data GetLocatedImports = GetLocatedImports
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetLocatedImports
instance NFData   GetLocatedImports

-- | Does this module need to be compiled?
type instance RuleResult NeedsCompilation = Maybe LinkableType

data NeedsCompilation = NeedsCompilation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable NeedsCompilation
instance NFData   NeedsCompilation

data GetDependencyInformation = GetDependencyInformation
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDependencyInformation
instance NFData   GetDependencyInformation

data GetModuleGraph = GetModuleGraph
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModuleGraph
instance NFData   GetModuleGraph

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

data GetDocMap = GetDocMap
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetDocMap
instance NFData   GetDocMap

data GetHieAst = GetHieAst
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHieAst
instance NFData   GetHieAst

data GetBindings = GetBindings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetBindings
instance NFData   GetBindings

data GhcSession = GhcSession
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSession
instance NFData   GhcSession

data GhcSessionDeps = GhcSessionDeps deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSessionDeps
instance NFData   GhcSessionDeps

data GetModIfaceFromDisk = GetModIfaceFromDisk
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIfaceFromDisk
instance NFData   GetModIfaceFromDisk

data GetModIfaceFromDiskAndIndex = GetModIfaceFromDiskAndIndex
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIfaceFromDiskAndIndex
instance NFData   GetModIfaceFromDiskAndIndex

data GetModIface = GetModIface
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIface
instance NFData   GetModIface

data GetModIfaceWithoutLinkable = GetModIfaceWithoutLinkable
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModIfaceWithoutLinkable
instance NFData   GetModIfaceWithoutLinkable

data IsFileOfInterest = IsFileOfInterest
    deriving (Eq, Show, Typeable, Generic)
instance Hashable IsFileOfInterest
instance NFData   IsFileOfInterest

data GetModSummaryWithoutTimestamps = GetModSummaryWithoutTimestamps
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModSummaryWithoutTimestamps
instance NFData   GetModSummaryWithoutTimestamps

data GetModSummary = GetModSummary
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetModSummary
instance NFData   GetModSummary

-- | Get the vscode client settings stored in the ide state
data GetClientSettings = GetClientSettings
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetClientSettings
instance NFData   GetClientSettings

type instance RuleResult GetClientSettings = Hashed (Maybe Value)

data AddWatchedFile = AddWatchedFile deriving (Eq, Show, Typeable, Generic)
instance Hashable AddWatchedFile
instance NFData   AddWatchedFile


-- A local rule type to get caching. We want to use newCache, but it has
-- thread killed exception issues, so we lift it to a full rule.
-- https://github.com/digital-asset/daml/pull/2808#issuecomment-529639547
type instance RuleResult GhcSessionIO = IdeGhcSession

data IdeGhcSession = IdeGhcSession
  { loadSessionFun :: FilePath -> IO (IdeResult HscEnvEq, [FilePath])
  -- ^ Returns the Ghc session and the cradle dependencies
  , sessionVersion :: !Int
  -- ^ Used as Shake key, versions must be unique and not reused
  }

instance Show IdeGhcSession where show _ = "IdeGhcSession"
instance NFData IdeGhcSession where rnf !_ = ()

data GhcSessionIO = GhcSessionIO deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSessionIO
instance NFData   GhcSessionIO

makeLensesWith
    (lensRules & lensField .~ mappingNamer (pure . (++ "L")))
    ''Splices
