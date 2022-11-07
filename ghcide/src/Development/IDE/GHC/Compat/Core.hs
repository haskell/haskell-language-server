{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns   #-}
-- TODO: remove
{-# OPTIONS -Wno-dodgy-imports -Wno-unused-imports #-}

-- | Compat Core module that handles the GHC module hierarchy re-organisation
-- by re-exporting everything we care about.
--
-- This module provides no other compat mechanisms, except for simple
-- backward-compatible pattern synonyms.
module Development.IDE.GHC.Compat.Core (
    -- * Session
    DynFlags,
    extensions,
    extensionFlags,
    targetPlatform,
    packageFlags,
    generalFlags,
    warningFlags,
    topDir,
    hiDir,
    tmpDir,
    importPaths,
    useColor,
    canUseColor,
    useUnicode,
    objectDir,
    flagsForCompletion,
    setImportPaths,
    outputFile,
    pluginModNames,
    refLevelHoleFits,
    maxRefHoleFits,
    maxValidHoleFits,
    setOutputFile,
    CommandLineOption,
#if !MIN_VERSION_ghc(9,2,0)
    staticPlugins,
#endif
    sPgm_F,
    settings,
    gopt,
    gopt_set,
    gopt_unset,
    wopt,
    wopt_set,
    xFlags,
    xopt,
    xopt_unset,
    xopt_set,
    FlagSpec(..),
    WarningFlag(..),
    GeneralFlag(..),
    PackageFlag,
    PackageArg(..),
    ModRenaming(..),
    pattern ExposePackage,
    parseDynamicFlagsCmdLine,
    parseDynamicFilePragma,
#if !MIN_VERSION_ghc(9,3,0)
    WarnReason(..),
#endif
    wWarningFlags,
    updOptLevel,
    -- slightly unsafe
    setUnsafeGlobalDynFlags,
    -- * Linear Haskell
#if !MIN_VERSION_ghc(9,0,0)
    Scaled,
    unrestricted,
#endif
    scaledThing,
    -- * Interface Files
    IfaceExport,
    IfaceTyCon(..),
    ModIface,
    ModIface_(..),
    HscSource(..),
    WhereFrom(..),
    loadInterface,
#if !MIN_VERSION_ghc(9,3,0)
    SourceModified(..),
#endif
    loadModuleInterface,
    RecompileRequired(..),
    mkPartialIface,
    mkFullIface,
    checkOldIface,
#if MIN_VERSION_ghc(9,0,0)
    IsBootInterface(..),
#else
    pattern IsBoot,
    pattern NotBoot,
#endif
    -- * Fixity
    LexicalFixity(..),
    Fixity (..),
    mi_fix,
    defaultFixity,
    lookupFixityRn,
    -- * ModSummary
    ModSummary(..),
    -- * HomeModInfo
    HomeModInfo(..),
    -- * ModGuts
    ModGuts(..),
    CgGuts(..),
    -- * ModDetails
    ModDetails(..),
    -- * HsExpr,
#if !MIN_VERSION_ghc(9,2,0)
    pattern HsLet,
    pattern LetStmt,
#endif
    -- * Var
    Type (
      TyCoRep.TyVarTy,
      TyCoRep.AppTy,
      TyCoRep.TyConApp,
      TyCoRep.ForAllTy,
      -- Omitted on purpose
      -- pattern Synonym right below it
      -- TyCoRep.FunTy,
      TyCoRep.LitTy,
      TyCoRep.CastTy,
      TyCoRep.CoercionTy
      ),
    pattern FunTy,
    pattern ConPatIn,
    conPatDetails,
    mapConPatDetail,
#if !MIN_VERSION_ghc(9,2,0)
    Development.IDE.GHC.Compat.Core.splitForAllTyCoVars,
#endif
    mkVisFunTys,
    Development.IDE.GHC.Compat.Core.mkInfForAllTys,
    -- * Specs
    ImpDeclSpec(..),
    ImportSpec(..),
    -- * SourceText
    SourceText(..),
#if !MIN_VERSION_ghc(9,2,0)
    rationalFromFractionalLit,
#endif
    -- * Name
    tyThingParent_maybe,
    -- * Ways
    Way,
    wayGeneralFlags,
    wayUnsetGeneralFlags,
    -- * AvailInfo
    Avail.AvailInfo,
    pattern AvailName,
    pattern AvailFL,
    pattern AvailTC,
    Avail.availName,
    Avail.availNames,
    Avail.availNamesWithSelectors,
    Avail.availsToNameSet,
    -- * TcGblEnv
    TcGblEnv(..),
    -- * Parsing and LExer types
    HsModule(..),
    GHC.ParsedSource,
    GHC.RenamedSource,
    -- * Compilation Main
    HscEnv,
    GHC.runGhc,
    unGhc,
    Session(..),
    modifySession,
    getSession,
    GHC.setSessionDynFlags,
    getSessionDynFlags,
    GhcMonad,
    Ghc,
    runHsc,
    compileFile,
    Phase(..),
    hscDesugar,
    hscGenHardCode,
    hscInteractive,
    hscSimplify,
    hscTypecheckRename,
    Development.IDE.GHC.Compat.Core.makeSimpleDetails,
    -- * Typecheck utils
    Development.IDE.GHC.Compat.Core.tcSplitForAllTyVars,
    Development.IDE.GHC.Compat.Core.tcSplitForAllTyVarBinder_maybe,
    typecheckIface,
    Development.IDE.GHC.Compat.Core.mkIfaceTc,
    Development.IDE.GHC.Compat.Core.mkBootModDetailsTc,
    Development.IDE.GHC.Compat.Core.initTidyOpts,
    hscUpdateHPT,
    driverNoStop,
    tidyProgram,
    ImportedModsVal(..),
    importedByUser,
    GHC.TypecheckedSource,
    -- * Source Locations
    HasSrcSpan,
    SrcLoc.Located,
    SrcLoc.unLoc,
    getLoc,
    getLocA,
    locA,
    noLocA,
    unLocA,
    LocatedAn,
#if MIN_VERSION_ghc(9,2,0)
    GHC.AnnListItem(..),
    GHC.NameAnn(..),
#else
    AnnListItem,
    NameAnn,
#endif
    SrcLoc.RealLocated,
    SrcLoc.GenLocated(..),
    SrcLoc.SrcSpan(SrcLoc.UnhelpfulSpan),
    SrcLoc.RealSrcSpan,
    pattern RealSrcSpan,
    SrcLoc.RealSrcLoc,
    pattern RealSrcLoc,
    SrcLoc.SrcLoc(SrcLoc.UnhelpfulLoc),
    BufSpan,
#if MIN_VERSION_ghc(9,2,0)
    SrcSpanAnn',
    GHC.SrcAnn,
#endif
    SrcLoc.leftmost_smallest,
    SrcLoc.containsSpan,
    SrcLoc.mkGeneralSrcSpan,
    SrcLoc.mkRealSrcSpan,
    SrcLoc.mkRealSrcLoc,
    SrcLoc.getRealSrcSpan,
    SrcLoc.realSrcLocSpan,
    SrcLoc.realSrcSpanStart,
    SrcLoc.realSrcSpanEnd,
    isSubspanOfA,
    SrcLoc.isSubspanOf,
    SrcLoc.wiredInSrcSpan,
    SrcLoc.mkSrcSpan,
    SrcLoc.srcSpanStart,
    SrcLoc.srcSpanStartLine,
    SrcLoc.srcSpanStartCol,
    SrcLoc.srcSpanEnd,
    SrcLoc.srcSpanEndLine,
    SrcLoc.srcSpanEndCol,
    SrcLoc.srcSpanFile,
    SrcLoc.srcLocCol,
    SrcLoc.srcLocFile,
    SrcLoc.srcLocLine,
    SrcLoc.noSrcSpan,
    SrcLoc.noSrcLoc,
    SrcLoc.noLoc,
    SrcLoc.mapLoc,
    -- * Finder
    FindResult(..),
    mkHomeModLocation,
    addBootSuffixLocnOut,
    findObjectLinkableMaybe,
    InstalledFindResult(..),
    -- * Module and Package
    ModuleOrigin(..),
    PackageName(..),
    -- * Linker
    Unlinked(..),
    Linkable(..),
    unload,
    initDynLinker,
    -- * Hooks
    Hooks,
    runMetaHook,
    MetaHook,
    MetaRequest(..),
    metaRequestE,
    metaRequestP,
    metaRequestT,
    metaRequestD,
    metaRequestAW,
    -- * HPT
    addToHpt,
    addListToHpt,
    -- * Driver-Make
    Target(..),
    TargetId(..),
    mkModuleGraph,
    -- * GHCi
    initObjLinker,
    loadDLL,
    InteractiveImport(..),
    GHC.getContext,
    GHC.setContext,
    GHC.parseImportDecl,
    GHC.runDecls,
    Warn(..),
    -- * ModLocation
    GHC.ModLocation,
    Module.ml_hs_file,
    Module.ml_obj_file,
    Module.ml_hi_file,
    Module.ml_hie_file,
    -- * DataCon
    DataCon.dataConExTyCoVars,
    -- * Role
    Role(..),
    -- * Panic
    Plain.PlainGhcException,
    panic,
    panicDoc,
    -- * Other
    GHC.CoreModule(..),
    GHC.SafeHaskellMode(..),
    pattern GRE,
    gre_name,
    gre_imp,
    gre_lcl,
    gre_par,
#if MIN_VERSION_ghc(9,2,0)
    collectHsBindsBinders,
#endif
    -- * Util Module re-exports
#if MIN_VERSION_ghc(9,0,0)
    module GHC.Builtin.Names,
    module GHC.Builtin.Types,
    module GHC.Builtin.Types.Prim,
    module GHC.Builtin.Utils,
    module GHC.Core.Class,
    module GHC.Core.Coercion,
    module GHC.Core.ConLike,
    module GHC.Core.DataCon,
    module GHC.Core.FamInstEnv,
    module GHC.Core.InstEnv,
    module GHC.Types.Unique.FM,
#if !MIN_VERSION_ghc(9,2,0)
    module GHC.Core.Ppr.TyThing,
#endif
    module GHC.Core.PatSyn,
    module GHC.Core.Predicate,
    module GHC.Core.TyCon,
    module GHC.Core.TyCo.Ppr,
    module GHC.Core.Type,
    module GHC.Core.Unify,
    module GHC.Core.Utils,

    module GHC.HsToCore.Docs,
    module GHC.HsToCore.Expr,
    module GHC.HsToCore.Monad,

    module GHC.Iface.Syntax,

#if MIN_VERSION_ghc(9,2,0)
    module GHC.Hs.Decls,
    module GHC.Hs.Expr,
    module GHC.Hs.Doc,
    module GHC.Hs.Extension,
    module GHC.Hs.ImpExp,
    module GHC.Hs.Pat,
    module GHC.Hs.Type,
    module GHC.Hs.Utils,
    module Language.Haskell.Syntax,
#endif

    module GHC.Rename.Names,
    module GHC.Rename.Splice,

    module GHC.Tc.Instance.Family,
    module GHC.Tc.Module,
    module GHC.Tc.Types,
    module GHC.Tc.Types.Evidence,
    module GHC.Tc.Utils.Env,
    module GHC.Tc.Utils.Monad,

    module GHC.Types.Basic,
    module GHC.Types.Id,
    module GHC.Types.Name            ,
    module GHC.Types.Name.Set,

    module GHC.Types.Name.Cache,
    module GHC.Types.Name.Env,
    module GHC.Types.Name.Reader,
    module GHC.Utils.Error,
#if MIN_VERSION_ghc(9,2,0)
    module GHC.Types.Avail,
    module GHC.Types.SourceFile,
    module GHC.Types.SourceText,
    module GHC.Types.TyThing,
    module GHC.Types.TyThing.Ppr,
#endif
    module GHC.Types.Unique.Supply,
    module GHC.Types.Var,
    module GHC.Unit.Module,
#else
    module BasicTypes,
    module Class,
    module Coercion,
    module Predicate,
    module ConLike,
    module CoreUtils,
    module DataCon,
    module DsExpr,
    module DsMonad,
    module ErrUtils,
    module FamInst,
    module FamInstEnv,
    module HeaderInfo,
    module Id,
    module InstEnv,
    module IfaceSyn,
    module Module,
    module Name,
    module NameCache,
    module NameEnv,
    module NameSet,
    module PatSyn,
    module PprTyThing,
    module PrelInfo,
    module PrelNames,
    module RdrName,
    module RnSplice,
    module RnNames,
    module TcEnv,
    module TcEvidence,
    module TcType,
    module TcRnTypes,
    module TcRnDriver,
    module TcRnMonad,
    module TyCon,
    module TysPrim,
    module TysWiredIn,
    module Type,
    module Unify,
    module UniqFM,
    module UniqSupply,
    module Var,
#endif
    -- * Syntax re-exports
#if MIN_VERSION_ghc(9,0,0)
    module GHC.Hs,
    module GHC.Parser,
    module GHC.Parser.Header,
    module GHC.Parser.Lexer,
#else
    module GHC.Hs,
    module ExtractDocs,
    module Parser,
    module Lexer,
#endif
#if MIN_VERSION_ghc(9,3,0)
    CompileReason(..),
    hsc_type_env_vars,
    hscUpdateHUG, hscUpdateHPT, hsc_HUG,
    GhcMessage(..),
    getKey,
    module GHC.Driver.Env.KnotVars,
    module GHC.Iface.Recomp,
    module GHC.Linker.Types,
    module GHC.Unit.Module.Graph,
    module GHC.Types.Unique.Map,
    module GHC.Utils.TmpFs,
    module GHC.Utils.Panic,
    module GHC.Unit.Finder.Types,
    module GHC.Unit.Env,
    module GHC.Driver.Phases,
#endif
# if !MIN_VERSION_ghc(9,4,0)
    pattern HsFieldBind,
    hfbAnn,
    hfbLHS,
    hfbRHS,
    hfbPun,
#endif
#if !MIN_VERSION_ghc_boot_th(9,4,1)
    Extension(.., NamedFieldPuns),
#else
    Extension(..)
#endif
    ) where

import qualified GHC

#if MIN_VERSION_ghc(9,3,0)
import GHC.Iface.Recomp (CompileReason(..))
import GHC.Driver.Env.Types (hsc_type_env_vars)
import GHC.Driver.Env (hscUpdateHUG, hscUpdateHPT, hsc_HUG)
import GHC.Driver.Env.KnotVars
import GHC.Iface.Recomp
import GHC.Linker.Types
import GHC.Unit.Module.Graph
import GHC.Driver.Errors.Types
import GHC.Types.Unique.Map
import GHC.Types.Unique
import GHC.Utils.TmpFs
import GHC.Utils.Panic
import GHC.Unit.Finder.Types
import GHC.Unit.Env
import GHC.Driver.Phases
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Builtin.Names            hiding (Unique, printName)
import           GHC.Builtin.Types
import           GHC.Builtin.Types.Prim
import           GHC.Builtin.Utils
import           GHC.Core.Class
import           GHC.Core.Coercion
import           GHC.Core.ConLike
import           GHC.Core.DataCon             hiding (dataConExTyCoVars)
import qualified GHC.Core.DataCon             as DataCon
import           GHC.Core.FamInstEnv          hiding (pprFamInst)
import           GHC.Core.InstEnv
import           GHC.Types.Unique.FM
#if MIN_VERSION_ghc(9,3,0)
import qualified GHC.Driver.Config.Tidy       as GHC
import qualified GHC.Data.Strict              as Strict
#endif
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Data.Bag
import           GHC.Core.Multiplicity        (scaledThing)
#else
import           GHC.Core.Ppr.TyThing         hiding (pprFamInst)
import           GHC.Core.TyCo.Rep            (scaledThing)
#endif
import           GHC.Core.PatSyn
import           GHC.Core.Predicate
import           GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Rep            as TyCoRep
import           GHC.Core.TyCon
import           GHC.Core.Type                hiding (mkInfForAllTys)
import           GHC.Core.Unify
import           GHC.Core.Utils


#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env
#else
import           GHC.Driver.Finder hiding     (mkHomeModLocation)
import           GHC.Driver.Types
import           GHC.Driver.Ways
#endif
import           GHC.Driver.CmdLine           (Warn (..))
import           GHC.Driver.Hooks
import           GHC.Driver.Main              as GHC
import           GHC.Driver.Monad
import           GHC.Driver.Phases
import           GHC.Driver.Pipeline
import           GHC.Driver.Plugins
import           GHC.Driver.Session           hiding (ExposePackage)
import qualified GHC.Driver.Session           as DynFlags
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Hs                       (HsModule (..), SrcSpanAnn')
import           GHC.Hs.Decls                 hiding (FunDep)
import           GHC.Hs.Doc
import           GHC.Hs.Expr
import           GHC.Hs.Extension
import           GHC.Hs.ImpExp
import           GHC.Hs.Pat
import           GHC.Hs.Type
import           GHC.Hs.Utils                 hiding (collectHsBindsBinders)
import qualified GHC.Hs.Utils                 as GHC
#endif
#if !MIN_VERSION_ghc(9,2,0)
import           GHC.Hs                       hiding (HsLet, LetStmt)
#endif
import           GHC.HsToCore.Docs
import           GHC.HsToCore.Expr
import           GHC.HsToCore.Monad
import           GHC.Iface.Load
import           GHC.Iface.Make               (mkFullIface, mkPartialIface)
import           GHC.Iface.Make               as GHC
import           GHC.Iface.Recomp
import           GHC.Iface.Syntax
import           GHC.Iface.Tidy               as GHC
import           GHC.IfaceToCore
import           GHC.Parser
import           GHC.Parser.Header            hiding (getImports)
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Linker.Loader            as Linker
import           GHC.Linker.Types
import           GHC.Parser.Lexer             hiding (initParserState)
import           GHC.Parser.Annotation        (EpAnn (..))
import           GHC.Platform.Ways
import           GHC.Runtime.Context          (InteractiveImport (..))
#else
import           GHC.Parser.Lexer
import qualified GHC.Runtime.Linker           as Linker
#endif
import           GHC.Rename.Fixity            (lookupFixityRn)
import           GHC.Rename.Names
import           GHC.Rename.Splice
import qualified GHC.Runtime.Interpreter      as GHCi
import           GHC.Tc.Instance.Family
import           GHC.Tc.Module
import           GHC.Tc.Types
import           GHC.Tc.Types.Evidence        hiding ((<.>))
import           GHC.Tc.Utils.Env
import           GHC.Tc.Utils.Monad           hiding (Applicative (..), IORef,
                                               MonadFix (..), MonadIO (..),
                                               allM, anyM, concatMapM,
                                               mapMaybeM, (<$>))
import           GHC.Tc.Utils.TcType          as TcType
import qualified GHC.Types.Avail              as Avail
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Types.Avail              (greNamePrintableName)
import           GHC.Types.Fixity             (LexicalFixity (..), Fixity (..), defaultFixity)
#endif
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Types.Meta
#endif
import           GHC.Types.Basic
import           GHC.Types.Id
import           GHC.Types.Name               hiding (varName)
import           GHC.Types.Name.Cache
import           GHC.Types.Name.Env
import           GHC.Types.Name.Reader        hiding (GRE, gre_name, gre_imp, gre_lcl, gre_par)
import qualified GHC.Types.Name.Reader        as RdrName
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Types.Name.Set
import           GHC.Types.SourceFile         (HscSource (..),
#if !MIN_VERSION_ghc(9,3,0)
                                               SourceModified(..)
#endif
                                               )
import           GHC.Types.SourceText
import           GHC.Types.Target             (Target (..), TargetId (..))
import           GHC.Types.TyThing
import           GHC.Types.TyThing.Ppr
#else
import           GHC.Types.Name.Set
#endif
import           GHC.Types.SrcLoc             (BufPos, BufSpan,
                                               SrcLoc (UnhelpfulLoc),
                                               SrcSpan (UnhelpfulSpan))
import qualified GHC.Types.SrcLoc             as SrcLoc
import           GHC.Types.Unique.Supply
import           GHC.Types.Var                (Var (varName), setTyVarUnique,
                                               setVarUnique)
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Unit.Finder              hiding (mkHomeModLocation)
import           GHC.Unit.Home.ModInfo
#endif
import           GHC.Unit.Info                (PackageName (..))
import           GHC.Unit.Module              hiding (ModLocation (..), UnitId,
                                               addBootSuffixLocnOut, moduleUnit,
                                               toUnitId)
import qualified GHC.Unit.Module              as Module
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Unit.Module.Graph        (mkModuleGraph)
import           GHC.Unit.Module.Imported
import           GHC.Unit.Module.ModDetails
import           GHC.Unit.Module.ModGuts
import           GHC.Unit.Module.ModIface     (IfaceExport, ModIface (..),
                                               ModIface_ (..), mi_fix)
import           GHC.Unit.Module.ModSummary   (ModSummary (..))
#endif
import           GHC.Unit.State               (ModuleOrigin (..))
import           GHC.Utils.Error              (Severity (..), emptyMessages)
import           GHC.Utils.Panic              hiding (try)
import qualified GHC.Utils.Panic.Plain        as Plain
#else
import qualified Avail
import           BasicTypes                   hiding (Version)
import           Class
import           CmdLineParser                (Warn (..))
import           ConLike
import           CoreUtils
import           DataCon                      hiding (dataConExTyCoVars)
import qualified DataCon
import           DriverPhases
import           DriverPipeline
import           DsExpr
import           DsMonad                      hiding (foldrM)
import           DynFlags                     hiding (ExposePackage)
import qualified DynFlags
import           ErrUtils                     hiding (logInfo, mkWarnMsg)
import           ExtractDocs
import           FamInst
import           FamInstEnv
import           Finder                       hiding (mkHomeModLocation)
import           GHC.Hs                       hiding (HsLet, LetStmt)
import qualified GHCi
import           GhcMonad
import           HeaderInfo                   hiding (getImports)
import           Hooks
import           HscMain                      as GHC
import           HscTypes
import           Id
import           IfaceSyn
import           InstEnv
import           Lexer                        hiding (getSrcLoc)
import qualified Linker
import           LoadIface
import           MkIface                      as GHC
import           Module                       hiding (ModLocation (..), UnitId,
                                               addBootSuffixLocnOut,
                                               moduleUnitId)
import qualified Module
import           Name                         hiding (varName)
import           NameCache
import           NameEnv
import           NameSet
import           Packages
import           Panic                        hiding (try)
import qualified PlainPanic                   as Plain
import           Parser
import           PatSyn
import           RnFixity
import           Plugins
import           PprTyThing                   hiding (pprFamInst)
import           PrelInfo
import           PrelNames                    hiding (Unique, printName)
import           RdrName                      hiding (GRE, gre_name, gre_imp, gre_lcl, gre_par)
import qualified RdrName
import           RnNames
import           RnSplice
import qualified SrcLoc
import           TcEnv
import           TcEvidence                   hiding ((<.>))
import           TcIface
import           TcRnDriver
import           TcRnMonad                    hiding (Applicative (..), IORef,
                                               MonadFix (..), MonadIO (..),
                                               allM, anyM, concatMapM, foldrM,
                                               mapMaybeM, (<$>))
import           TcRnTypes
import           TcType
import qualified TcType
import           TidyPgm                     as GHC
import qualified TyCoRep
import           TyCon
import           Type
import           TysPrim
import           TysWiredIn
import           Unify
import           UniqFM
import           UniqSupply
import           Var                          (Var (varName), setTyVarUnique,
                                               setVarUnique, varType)

import           Coercion                     (coercionKind)
import           Predicate
import           SrcLoc                       (Located, SrcLoc (UnhelpfulLoc),
                                               SrcSpan (UnhelpfulSpan))
#endif


import           Data.List                    (isSuffixOf)
import           System.FilePath


#if MIN_VERSION_ghc(9,2,0)
import           Language.Haskell.Syntax hiding (FunDep)
#endif
#if MIN_VERSION_ghc(9,3,0)
import GHC.Driver.Env as GHCi
#endif

import Data.Foldable (toList)

#if MIN_VERSION_ghc(9,3,0)
import qualified GHC.Unit.Finder as GHC
import qualified GHC.Driver.Config.Finder as GHC
#elif MIN_VERSION_ghc(9,2,0)
import qualified GHC.Unit.Finder as GHC
#elif MIN_VERSION_ghc(9,0,0)
import qualified GHC.Driver.Finder as GHC
#else
import qualified Finder as GHC
#endif

-- NOTE(ozkutuk): Cpp clashes Phase.Cpp, so we hide it.
-- Not the greatest solution, but gets the job done
-- (until the CPP extension is actually needed).
import GHC.LanguageExtensions.Type hiding (Cpp)


mkHomeModLocation :: DynFlags -> ModuleName -> FilePath -> IO Module.ModLocation
#if MIN_VERSION_ghc(9,3,0)
mkHomeModLocation df mn f = pure $ GHC.mkHomeModLocation (GHC.initFinderOpts df) mn f
#else
mkHomeModLocation = GHC.mkHomeModLocation
#endif


#if !MIN_VERSION_ghc(9,0,0)
type BufSpan = ()
type BufPos = ()
#endif

#if MIN_VERSION_ghc(9,3,0)
pattern RealSrcSpan :: SrcLoc.RealSrcSpan -> Maybe BufSpan -> SrcLoc.SrcSpan
#else
pattern RealSrcSpan :: SrcLoc.RealSrcSpan -> Maybe BufSpan -> SrcLoc.SrcSpan
#endif

#if MIN_VERSION_ghc(9,3,0)
pattern RealSrcSpan x y <- SrcLoc.RealSrcSpan x ((\case Strict.Nothing -> Nothing; Strict.Just a -> Just a) -> y) where
  RealSrcSpan x y = SrcLoc.RealSrcSpan x (case y of Nothing -> Strict.Nothing; Just a -> Strict.Just a)

#elif MIN_VERSION_ghc(9,0,0)
pattern RealSrcSpan x y = SrcLoc.RealSrcSpan x y
#else
pattern RealSrcSpan x y <- ((,Nothing) -> (SrcLoc.RealSrcSpan x, y)) where
    RealSrcSpan x _ = SrcLoc.RealSrcSpan x
#endif
{-# COMPLETE RealSrcSpan, UnhelpfulSpan #-}

#if MIN_VERSION_ghc(9,3,0)
pattern RealSrcLoc :: SrcLoc.RealSrcLoc -> Strict.Maybe BufPos-> SrcLoc.SrcLoc
#else
pattern RealSrcLoc :: SrcLoc.RealSrcLoc -> Maybe BufPos-> SrcLoc.SrcLoc
#endif
#if MIN_VERSION_ghc(9,0,0)
pattern RealSrcLoc x y = SrcLoc.RealSrcLoc x y
#else
pattern RealSrcLoc x y <- ((,Nothing) -> (SrcLoc.RealSrcLoc x, y)) where
    RealSrcLoc x _ = SrcLoc.RealSrcLoc x
#endif
{-# COMPLETE RealSrcLoc, UnhelpfulLoc #-}


pattern AvailTC :: Name -> [Name] -> [FieldLabel] -> Avail.AvailInfo
#if __GLASGOW_HASKELL__ >= 902
pattern AvailTC n names pieces <- Avail.AvailTC n ((\gres -> foldr (\gre (names, pieces) -> case gre of
      Avail.NormalGreName name -> (name: names, pieces)
      Avail.FieldGreName label -> (names, label:pieces)) ([], []) gres) -> (names, pieces))
#else
pattern AvailTC n names pieces <- Avail.AvailTC n names pieces
#endif

pattern AvailName :: Name -> Avail.AvailInfo
#if __GLASGOW_HASKELL__ >= 902
pattern AvailName n <- Avail.Avail (Avail.NormalGreName n)
#else
pattern AvailName n <- Avail.Avail n
#endif

pattern AvailFL :: FieldLabel -> Avail.AvailInfo
#if __GLASGOW_HASKELL__ >= 902
pattern AvailFL fl <- Avail.Avail (Avail.FieldGreName fl)
#else
-- pattern synonym that is never populated
pattern AvailFL x <- Avail.Avail (const (True, undefined) -> (False, x))
#endif

{-# COMPLETE AvailTC, AvailName, AvailFL #-}

setImportPaths :: [FilePath] -> DynFlags -> DynFlags
setImportPaths importPaths flags = flags { importPaths = importPaths }

pattern ExposePackage :: String -> PackageArg -> ModRenaming -> PackageFlag
-- https://github.com/facebook/fbghc
#ifdef __FACEBOOK_HASKELL__
pattern ExposePackage s a mr <- DynFlags.ExposePackage s a _ mr
#else
pattern ExposePackage s a mr = DynFlags.ExposePackage s a mr
#endif

pattern FunTy :: Type -> Type -> Type
pattern FunTy arg res <- TyCoRep.FunTy {ft_arg = arg, ft_res = res}

#if MIN_VERSION_ghc(9,0,0)
-- type HasSrcSpan x a = (GenLocated SrcSpan a ~ x)
-- type HasSrcSpan x = () :: Constraint

class HasSrcSpan a where
  getLoc :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  getLoc = id

instance HasSrcSpan (SrcLoc.GenLocated SrcSpan a) where
  getLoc = GHC.getLoc

#if MIN_VERSION_ghc(9,2,0)
instance HasSrcSpan (SrcSpanAnn' ann) where
  getLoc = locA
instance HasSrcSpan (SrcLoc.GenLocated (SrcSpanAnn' ann) a) where
  getLoc (L l _) = l

pattern L :: HasSrcSpan a => SrcSpan -> e -> SrcLoc.GenLocated a e
pattern L l a <- GHC.L (getLoc -> l) a
{-# COMPLETE L #-}
#endif

#else
type HasSrcSpan = SrcLoc.HasSrcSpan
getLoc :: SrcLoc.HasSrcSpan a => a -> SrcLoc.SrcSpan
getLoc = SrcLoc.getLoc
#endif

-- | Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut :: GHC.ModLocation -> GHC.ModLocation
addBootSuffixLocnOut = Module.addBootSuffixLocnOut

#if !MIN_VERSION_ghc(9,0,0)
-- Linear Haskell
type Scaled a = a
scaledThing :: Scaled a -> a
scaledThing = id

unrestricted :: a -> Scaled a
unrestricted = id
#endif

mkInfForAllTys :: [TyVar] -> Type -> Type
mkInfForAllTys =
#if MIN_VERSION_ghc(9,0,0)
  TcType.mkInfForAllTys
#else
  mkInvForAllTys
#endif

#if !MIN_VERSION_ghc(9,2,0)
splitForAllTyCoVars :: Type -> ([TyCoVar], Type)
splitForAllTyCoVars =
  splitForAllTys
#endif

tcSplitForAllTyVars :: Type -> ([TyVar], Type)
tcSplitForAllTyVars =
#if MIN_VERSION_ghc(9,2,0)
  TcType.tcSplitForAllTyVars
#else
  tcSplitForAllTys
#endif


tcSplitForAllTyVarBinder_maybe :: Type -> Maybe (TyVarBinder, Type)
tcSplitForAllTyVarBinder_maybe =
#if MIN_VERSION_ghc(9,2,0)
  TcType.tcSplitForAllTyVarBinder_maybe
#else
  tcSplitForAllTy_maybe
#endif


#if !MIN_VERSION_ghc(9,0,0)
pattern NotBoot, IsBoot :: IsBootInterface
pattern NotBoot = False
pattern IsBoot = True
#endif

#if MIN_VERSION_ghc(9,0,0)
-- This is from the old api, but it still simplifies
pattern ConPatIn :: SrcLoc.Located (ConLikeP GhcPs) -> HsConPatDetails GhcPs -> Pat GhcPs
#if MIN_VERSION_ghc(9,2,0)
pattern ConPatIn con args <- ConPat EpAnnNotUsed (L _ (SrcLoc.noLoc -> con)) args
  where
    ConPatIn con args = ConPat EpAnnNotUsed (GHC.noLocA $ SrcLoc.unLoc con) args
#else
pattern ConPatIn con args = ConPat NoExtField con args
#endif
#endif

conPatDetails :: Pat p -> Maybe (HsConPatDetails p)
#if MIN_VERSION_ghc(9,0,0)
conPatDetails (ConPat _ _ args) = Just args
conPatDetails _ = Nothing
#else
conPatDetails (ConPatIn _ args) = Just args
conPatDetails _ = Nothing
#endif

mapConPatDetail :: (HsConPatDetails p -> Maybe (HsConPatDetails p)) -> Pat p -> Maybe (Pat p)
#if MIN_VERSION_ghc(9,0,0)
mapConPatDetail f pat@(ConPat _ _ args) = (\args' -> pat { pat_args = args'}) <$> f args
mapConPatDetail _ _ = Nothing
#else
mapConPatDetail f (ConPatIn ss args) = ConPatIn ss <$> f args
mapConPatDetail _ _ = Nothing
#endif


initDynLinker, initObjLinker :: HscEnv -> IO ()
initDynLinker =
#if !MIN_VERSION_ghc(9,0,0)
    Linker.initDynLinker
#else
    -- It errors out in GHC 9.0 and doesn't exist in 9.2
    const $ return ()
#endif

initObjLinker env =
#if !MIN_VERSION_ghc(9,2,0)
    GHCi.initObjLinker env
#else
    GHCi.initObjLinker (GHCi.hscInterp env)
#endif

loadDLL :: HscEnv -> String -> IO (Maybe String)
loadDLL env =
#if !MIN_VERSION_ghc(9,2,0)
    GHCi.loadDLL env
#else
    GHCi.loadDLL (GHCi.hscInterp env)
#endif

unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env linkables =
  Linker.unload
#if MIN_VERSION_ghc(9,2,0)
    (GHCi.hscInterp hsc_env)
#endif
    hsc_env linkables

#if !MIN_VERSION_ghc(9,3,0)
setOutputFile :: FilePath -> DynFlags -> DynFlags
setOutputFile f d = d {
#if MIN_VERSION_ghc(9,2,0)
  outputFile_    = Just f
#else
  outputFile     = Just f
#endif
  }
#endif

isSubspanOfA :: LocatedAn la a -> LocatedAn lb b -> Bool
#if MIN_VERSION_ghc(9,2,0)
isSubspanOfA a b = SrcLoc.isSubspanOf (GHC.getLocA a) (GHC.getLocA b)
#else
isSubspanOfA a b = SrcLoc.isSubspanOf (GHC.getLoc a) (GHC.getLoc b)
#endif

#if MIN_VERSION_ghc(9,2,0)
type LocatedAn a = GHC.LocatedAn a
#else
type LocatedAn a = GHC.Located
#endif

#if MIN_VERSION_ghc(9,2,0)
locA :: SrcSpanAnn' a -> SrcSpan
locA = GHC.locA
#else
locA = id
#endif

#if MIN_VERSION_ghc(9,2,0)
unLocA :: forall pass a. XRec (GhcPass pass) a -> a
unLocA = unXRec @(GhcPass pass)
#else
unLocA = id
#endif

#if MIN_VERSION_ghc(9,2,0)
getLocA :: SrcLoc.GenLocated (SrcSpanAnn' a) e -> SrcSpan
getLocA = GHC.getLocA
#else
-- getLocA :: HasSrcSpan a => a -> SrcSpan
getLocA x = GHC.getLoc x
#endif

noLocA :: a -> LocatedAn an a
#if MIN_VERSION_ghc(9,2,0)
noLocA = GHC.noLocA
#else
noLocA = GHC.noLoc
#endif

#if !MIN_VERSION_ghc(9,2,0)
type AnnListItem = SrcLoc.SrcSpan
#endif

#if !MIN_VERSION_ghc(9,2,0)
type NameAnn = SrcLoc.SrcSpan
#endif

pattern GRE :: Name -> Parent -> Bool -> [ImportSpec] -> RdrName.GlobalRdrElt
{-# COMPLETE GRE #-}
#if MIN_VERSION_ghc(9,2,0)
pattern GRE{gre_name, gre_par, gre_lcl, gre_imp} <- RdrName.GRE
    {gre_name = (greNamePrintableName -> gre_name)
    ,gre_par, gre_lcl, gre_imp = (toList -> gre_imp)}
#else
pattern GRE{gre_name, gre_par, gre_lcl, gre_imp} = RdrName.GRE{..}
#endif

#if MIN_VERSION_ghc(9,2,0)
collectHsBindsBinders :: CollectPass p => Bag (XRec p (HsBindLR p idR)) -> [IdP p]
collectHsBindsBinders x = GHC.collectHsBindsBinders CollNoDictBinders x
#endif

#if !MIN_VERSION_ghc(9,2,0)
pattern HsLet xlet localBinds expr <- GHC.HsLet xlet (SrcLoc.unLoc -> localBinds) expr
pattern LetStmt xlet localBinds <- GHC.LetStmt xlet (SrcLoc.unLoc -> localBinds)
#endif

#if !MIN_VERSION_ghc(9,2,0)
rationalFromFractionalLit :: FractionalLit -> Rational
rationalFromFractionalLit = fl_value
#endif

makeSimpleDetails :: HscEnv -> TcGblEnv -> IO ModDetails
makeSimpleDetails hsc_env =
  GHC.makeSimpleDetails
#if MIN_VERSION_ghc(9,3,0)
              (hsc_logger hsc_env)
#else
              hsc_env
#endif

mkIfaceTc hsc_env sf details ms tcGblEnv =
  GHC.mkIfaceTc hsc_env sf details
#if MIN_VERSION_ghc(9,3,0)
              ms
#endif
              tcGblEnv

mkBootModDetailsTc :: HscEnv -> TcGblEnv -> IO ModDetails
mkBootModDetailsTc session = GHC.mkBootModDetailsTc
#if MIN_VERSION_ghc(9,3,0)
          (hsc_logger session)
#else
          session
#endif

#if !MIN_VERSION_ghc(9,3,0)
type TidyOpts = HscEnv
#endif

initTidyOpts :: HscEnv -> IO TidyOpts
initTidyOpts =
#if MIN_VERSION_ghc(9,3,0)
  GHC.initTidyOpts
#else
  pure
#endif

driverNoStop =
#if MIN_VERSION_ghc(9,3,0)
                                         NoStop
#else
                                         StopLn
#endif

#if !MIN_VERSION_ghc(9,3,0)
hscUpdateHPT :: (HomePackageTable -> HomePackageTable) -> HscEnv -> HscEnv
hscUpdateHPT k session = session { hsc_HPT = k (hsc_HPT session) }
#endif

#if !MIN_VERSION_ghc(9,2,0)
match :: HsRecField' id arg -> ((), id, arg, Bool)
match (HsRecField lhs rhs pun) = ((), SrcLoc.unLoc lhs, rhs, pun)

pattern HsFieldBind :: () -> id -> arg -> Bool -> HsRecField' id arg
pattern HsFieldBind {hfbAnn, hfbLHS, hfbRHS, hfbPun} <- (match -> (hfbAnn, hfbLHS, hfbRHS, hfbPun)) where
  HsFieldBind _ lhs rhs pun = HsRecField (SrcLoc.noLoc lhs) rhs pun
#elif !MIN_VERSION_ghc(9,4,0)
pattern HsFieldBind :: XHsRecField id -> id -> arg -> Bool -> HsRecField' id arg
pattern HsFieldBind {hfbAnn, hfbLHS, hfbRHS, hfbPun} <- HsRecField hfbAnn (SrcLoc.unLoc -> hfbLHS) hfbRHS hfbPun where
  HsFieldBind ann lhs rhs pun = HsRecField ann (SrcLoc.noLoc lhs) rhs pun
#endif

#if !MIN_VERSION_ghc_boot_th(9,4,1)
pattern NamedFieldPuns :: Extension
pattern NamedFieldPuns = RecordPuns
#endif
