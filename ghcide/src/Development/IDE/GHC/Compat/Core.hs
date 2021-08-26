{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
-- {-# OPTIONS -Wno-dodgy-imports #-}

-- | Compat Core module that handles the GHC module hierarchy re-organisation
-- by re-exporting everything we care about.
--
-- This module provides no other compat mechanisms, except for simple
-- backward-compatible pattern synonyms.
module Development.IDE.GHC.Compat.Core (
    -- * Exception handling
    GhcException,
    handleGhcException,
    gcatch,
    -- * Bags
    Bag,
    bagToList,
    listToBag,
    unionBags,
    isEmptyBag,
    -- * UniqueSupply
    mkSplitUniqSupply,
    -- * Maybes
    MaybeErr(..),
    orElse,
#if MIN_VERSION_ghc(8,10,0)
    -- * Pair
    Pair(..),
#endif
    -- * Session
    DynFlags,
    packageFlags,
    hiDir,
    tmpDir,
    importPaths,
    useUnicode,
    objectDir,
    flagsForCompletion,
    setImportPaths,
    outputFile,
    gopt,
    gopt_set,
    gopt_unset,
    wopt,
    wopt_set,
    xopt,
    xopt_set,
    WarningFlag(..),
    GeneralFlag(..),
    PackageFlag,
    pattern ExposePackage,
    parseDynamicFilePragma,
    WarnReason(..),
    wWarningFlags,
    flagSpecName,
    flagSpecFlag,
    updOptLevel,
    -- slightly unsafe
    setUnsafeGlobalDynFlags,
    -- * ConLike
    ConLike(..),
    conLikeName,
    conLikeFieldLabels,
    dataConWrapId,
    -- * Fingerprint
    Fingerprint(..),
    getFileHash,
    fingerprintData,
    fingerprintFingerprints,
    -- * Interface Files
    IfaceExport,
    IfaceTyCon(..),
#if MIN_VERSION_ghc(8,10,0)
    ModIface,
    ModIface_(..),
#else
    ModIface(..),
#endif
    HscSource(..),
    WhereFrom(..),
    loadInterface,
    SourceModified(..),
    loadModuleInterface,
    initIfaceLoad,
    RecompileRequired(..),
#if MIN_VERSION_ghc(8,10,0)
    mkPartialIface,
    mkFullIface,
#else
    mkIface,
#endif
    checkOldIface,
    -- * ModSummary
    ModSummary(..),
    -- * HomeModInfo
    HomeModInfo(..),
    -- * ModGuts
    ModGuts(..),
    CgGuts(..),
    -- * ModDetails
    ModDetails(..),
    -- * NameCache
    NameCache,
    initNameCache,
    -- * NameEnv
    NameEnv,
    nameEnvElts,
    mkNameEnv,
    unitNameEnv,
    extendNameEnv,
    lookupNameEnv,
    -- * NameSpace
    isTcClsNameSpace,
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
    isPredTy,
    isDictTy,
    isForAllTy,
    isFunTy,
    isPiTy,
#if MIN_VERSION_ghc(8,10,0)
    coercionKind,
    isCoercionTy_maybe,
#else
    isCoercionTy,
    splitCoercionType_maybe,
#endif
    splitFunTys,
    splitPiTys,
    splitForAllTys,
    TyThing(..),
    binderVar,
    Var,
    varType,
    varName,
    mkVarOcc,
    -- * TyCon
    TyCon,
    tyConName,
    -- * Id
    idName,
    idType,
    -- * GlobalRdrEnv
    GlobalRdrEnv,
    GlobalRdrElt(..),
    lookupGlobalRdrEnv,
    globalRdrEnvElts,
    lookupGRE_Name,
    -- * Specs
    ImpDeclSpec(..),
    ImportSpec(..),
    -- * Name
    Name,
    isValName,
    isSystemName,
    isInternalName,
    nameSrcSpan,
    nameSrcLoc,
    nameRdrName,
    nameModule_maybe,
    getSrcSpan,
    RdrName(..),
    mkRdrUnqual,
    rdrNameFieldOcc,
    OccName(..),
    occName,
    nameOccName,
    rdrNameOcc,
    parenSymOcc,
    isValOcc,
    isVarOcc,
    isDataOcc,
    isSymOcc,
    isTcOcc,
    occNameString,
    isDataConName,
    mkVarOccFS,
    pprNameDefnLoc,
    Parent(..),
    -- * AvailInfo
    Avail.AvailInfo,
    pattern AvailName,
    pattern AvailFL,
    pattern AvailTC,
    -- * NameSet
    elemNameSet,
    Avail.availsToNameSet,
    -- * TcGblEnv
    TcGblEnv(..),
    -- * FieldLabel
    FieldLabel,
    flSelector,
    flLabel,
    -- * FastString exports
    FastString,
#if MIN_VERSION_ghc(9,2,0)
    -- Export here, so we can coerce safely on consumer sites
    LexicalFastString(..),
#endif
    uniq,
    unpackFS,
    fingerprintString,
    mkFastString,
    fsLit,
    -- * Header Parser
    getOptions,
    -- * ErrUtils
    Severity(..),
    -- * String Buffer
    StringBuffer(..),
    hGetStringBuffer,
    stringToStringBuffer,
    -- * Parsing and Expr types
    P(..),
    PState(..),
    ParseResult(..),
    getMessages,
#if MIN_VERSION_ghc(8,10,0)
    getErrorMessages,
#endif
    HsParsedModule(..),
    ParsedModule(..),
    ParsedSource,
    RenamedSource,
    HsModule(..),
    LHsContext,
    HsContext,
    LHsExpr,
    HsExpr(..),
    LIE,
    IE(..),
    ieNames,
    IEWrappedName(..),
    IEWildcard(..),
    Pat(..),
    LPat,
    LHsDecl,
    HsDecl(..),
    TyClDecl(..),
    HsDataDefn(..),
    ConDecl(..),
    InstDecl(..),
#if MIN_VERSION_ghc(9,0,0)
    ClsInst,
#endif
    ClsInstDecl(..),
    DataFamInstDecl(..),
    TyFamInstDecl(..),
    FamEqn(..),
    DerivDecl(..),
    LSig,
    Sig(..),
    DefaultDecl(..),
    ForeignDecl(..),
    WarnDecls(..),
    AnnDecl(..),
    RuleDecls(..),
    SpliceDecl(..),
    DocDecl(..),
    RoleAnnotDecl(..),
    FamilyDecl(..),
    HsConDetails(..),
    LHsBind,
    HsBind,
    HsBindLR(..),
    PatSynBind(..),
    MatchGroup(..),
    LHsType,
    HsType(..),
    LImportDecl,
    ImportDecl(..),
#if MIN_VERSION_ghc(8,10,0)
    ImportDeclQualifiedStyle(..),
#endif
    HsWildCardBndrs(..),
    HsImplicitBndrs(..),
    LConDeclField,
    ConDeclField(..),
    HsValBindsLR(..),
    LMatch,
    Match(..),
    StmtLR(..),
    GRHS(..),
    GRHSs(..),
    HsLocalBinds,
    HsLocalBindsLR(..),
    parseHeader,
    parseIdentifier,
    parseModule,
    parenthesizeHsExpr,
    parenthesizeHsType,
    parenthesizePat,
    hsTypeNeedsParens,
    sigPrec,
    appPrec,
    StringLiteral(..),
    -- * Pat Syn
    PatSyn,
    mkPatSyn,
    patSynBuilder,
    patSynFieldLabels,
    patSynIsInfix,
    patSynMatcher,
    patSynName,
    patSynSig,
    pprPatSynType,
    -- * API Annotations
    AnnKeywordId(..),
    AnnotationComment(..),
    -- * Compilation Main
    HscEnv,
    runGhc,
    unGhc,
    Session(..),
    modifySession,
    getSession,
    setSessionDynFlags,
    GhcMonad,
    runHsc,
    compileFile,
    Phase(..),
    hscDesugar,
    hscGenHardCode,
    hscInteractive,
    hscSimplify,
    hscTypecheckRename,
    makeSimpleDetails,
    -- * Typecheck utils
    TcM,
    initTc,
    initTcWithGbl,
    tcLookup,
    TcTyThing(..),
    tcRnImportDecls,
    typecheckIface,
    mkIfaceTc,
    finalSafeMode,
    ImportAvails(..),
    ImportedModsVal(..),
    importedByUser,
    collectHsBindsBinders,
    -- * Source Locations
    HasSrcSpan,
    Located,
    unLoc,
    getLoc,
    SrcLoc.RealLocated,
    GenLocated(..),
    SrcLoc.SrcSpan(SrcLoc.UnhelpfulSpan),
    SrcLoc.RealSrcSpan,
    pattern RealSrcSpan,
    SrcLoc.SrcLoc(..),
    BufSpan,
    SrcLoc.mkGeneralSrcSpan,
    SrcLoc.mkRealSrcSpan,
    SrcLoc.mkRealSrcLoc,
    getRealSrcSpan,
    SrcLoc.realSrcLocSpan,
    SrcLoc.realSrcSpanStart,
    SrcLoc.realSrcSpanEnd,
    SrcLoc.isSubspanOf,
    SrcLoc.wiredInSrcSpan,
    SrcLoc.mkSrcSpan,
    SrcLoc.srcSpanStart,
    SrcLoc.srcSpanEnd,
    SrcLoc.srcSpanFile,
    SrcLoc.srcLocCol,
    SrcLoc.srcLocLine,
#if !MIN_VERSION_ghc(8,10,0) && MIN_VERSION_ghc(8,8,0)
    SrcLoc.dL,
#endif
    -- * Finder
    FindResult(..),
    mkHomeModLocation,
    findObjectLinkableMaybe,
    InstalledFindResult(..),
    -- * Module and Package
    Module,
    ModuleOrigin(..),
    PackageName(..),
    ModuleName,
    moduleName,
    mkModuleName,
    mkModule,
    moduleNameFS,
    moduleNameSlashes,
    moduleNameString,
    ModuleEnv,
    moduleEnvElts,
    emptyModuleEnv,
    extendModuleEnv,
    moduleEnvToList,
    extendInstalledModuleEnv,
    -- * Linker
    Unlinked(..),
    Linkable(..),
    unload,
    initDynLinker,
    -- * Doc Strings
    extractDocs,
    HsDocString,
    DeclDocMap(..),
    ArgDocMap(..),
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
    -- * Tidy
    tcInitTidyEnv,
    tidyOpenType,
    mkBootModDetailsTc,
    tidyProgram,
    -- * PrelInfo
    mkPrelImports,
    knownKeyNames,
    -- * Wired-in
    unitDataConId,
    -- * Utils with no home, neither here nor in GHC
    mkVarBind,
    addBootSuffixLocnOut,
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
    -- * Panic
    panic,
    ) where

import           GHC                    hiding (HasSrcSpan, ModLocation, getLoc,
                                         lookupName, RealSrcSpan, moduleUnitId, parseModule,
                                         Phase)

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC
import GHC.Builtin.Utils
import GHC.Builtin.Types
import GHC.Core.DataCon (dataConWrapId)
import GHC.Core.Coercion
import GHC.Core.ConLike (ConLike(..), conLikeName, conLikeFieldLabels)
#if !MIN_VERSION_ghc(9,2,0)
import GHC.Core.PatSyn
#endif
import qualified GHC.Core.TyCo.Rep as TyCoRep
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Predicate
import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.StringBuffer
import GHC.Data.Pair
#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env
import GHC.Driver.Env.Types
#else
import GHC.Driver.Types
import GHC.Driver.Finder
#endif
import GHC.Driver.Main
import GHC.Driver.Hooks
import GHC.Driver.Monad
import GHC.Driver.Pipeline
import GHC.Driver.Phases
import GHC.Driver.Session hiding (ExposePackage)
import qualified GHC.Driver.Session as DynFlags
#if !MIN_VERSION_ghc(9,2,0)
import GHC.HsToCore.Docs
#endif
import GHC.Iface.Load
import GHC.Iface.Make (mkIfaceExports, mkPartialIface, mkIfaceTc, mkFullIface)
import GHC.Iface.Tidy
import GHC.Iface.Type
import GHC.Iface.Recomp
import GHC.IfaceToCore
import GHC.Parser
import GHC.Parser.Header
#if MIN_VERSION_ghc(9,2,0)
import GHC.Linker.Loader
import GHC.Linker.Types
#else
import GHC.Parser.Lexer
import GHC.Runtime.Interpreter
import GHC.Runtime.Linker
#endif
import GHC.Tc.Module
import GHC.Tc.Types
import GHC.Tc.Utils.Env
import GHC.Tc.Utils.Monad
import qualified GHC.Types.Avail as Avail
import GHC.Types.FieldLabel
#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.Meta
#else
import GHC.Types.Basic
import GHC.Types.Id
#endif
import GHC.Types.Name hiding (varName)
import GHC.Types.Name.Occurrence hiding (varName)
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceFile (HscSource(..), SourceModified(..))
#else
import GHC.Types.Name.Set
#endif
import GHC.Types.SrcLoc (BufSpan, getRealSrcSpan)
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Var
#if MIN_VERSION_ghc(9,2,0)
import GHC.Unit.Env
import GHC.Unit.Finder
import GHC.Unit.Home.ModInfo
#endif
import GHC.Unit.Info (PackageName(..))
import GHC.Unit.Module.Env
#if MIN_VERSION_ghc(9,2,0)
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.ModDetails
import GHC.Unit.Module.ModIface (IfaceExport, mi_mod_hash)
#endif
import GHC.Unit.Module.Location
import GHC.Unit.Module.Name
import GHC.Unit.State (ModuleOrigin(..))
import GHC.Unit.Types (UnitId, unitString, IsBootInterface(..))
import GHC.Utils.Fingerprint
import GHC.Utils.Panic
import           Control.Exception.Safe as Safe (Exception, MonadCatch, catch)
#else
import           Bag
import           ConLike                (ConLike(..), conLikeName, conLikeFieldLabels)
import           DataCon                (dataConWrapId)
import           DynFlags               hiding (ExposePackage)
import qualified DynFlags
import           FastString
import           FieldLabel             (FieldLabel, flSelector, flLabel)
import           Finder
import           Fingerprint
import           Maybes
import           Module
#if MIN_VERSION_ghc(9,0,1)
import           Control.Exception.Safe as Safe (Exception, MonadCatch, catch)
import           GHC.Core.TyCo.Ppr      (pprSigmaType)
import           GHC.Core.TyCo.Rep      (Scaled, scaledThing)
import           GHC.Iface.Load
import           GHC.Types.Unique.Set   (emptyUniqSet)
import           Module                 (unitString)
#endif

import qualified Avail
import           HscTypes
import           HeaderInfo
import           NameEnv
import           HscMain
import           Hooks
import           IfaceType
import           Linker
import           LoadIface
import           MkIface
import           Name hiding (varName)
import           NameCache
import           Packages
import           Panic
import           Parser
import           PrelInfo
import           RdrName
import qualified SrcLoc
import           StringBuffer
import           TcEnv
import           TcIface
import           TcRnDriver
import           TcRnMonad
import qualified TyCoRep

import           TyCon
import           UniqSupply
import           Var
import TidyPgm
import DriverPipeline
import DriverPhases
import TysWiredIn
import BasicTypes
import Lexer
import GHCi
import ExtractDocs (extractDocs)
import PatSyn
import NameSet
import Id
import GhcMonad

#if MIN_VERSION_ghc(8,10,0)
import           TyCoTidy
import Predicate
import Type
import Pair
import Coercion (coercionKind)
#else
import           SrcLoc                 (RealLocated)
import Type
#endif
#endif

#if MIN_VERSION_ghc(9,2,0)
-- We are using Safe here, which is not equivalent, but probably what we want.
gcatch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
gcatch = Safe.catch

#elif MIN_VERSION_ghc(9,0,0)
-- We are using Safe here, which is not equivalent, but probably what we want.
gcatch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
gcatch = Safe.catch
#endif

#if !MIN_VERSION_ghc(9,0,0)
type BufSpan = ()
#endif

pattern RealSrcSpan :: SrcLoc.RealSrcSpan -> Maybe BufSpan -> SrcLoc.SrcSpan
#if MIN_VERSION_ghc(9,0,0)
pattern RealSrcSpan x y = SrcLoc.RealSrcSpan x y
#else
pattern RealSrcSpan x y <- ((,Nothing) -> (SrcLoc.RealSrcSpan x, y)) where
    RealSrcSpan x _ = SrcLoc.RealSrcSpan x
#endif
{-# COMPLETE RealSrcSpan, UnhelpfulSpan #-}


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
pattern AvailFL x <- Avail.Avail ((\_ -> (True, undefined)) -> (False, x))
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
#if MIN_VERSION_ghc(8,10,0)
pattern FunTy arg res <- TyCoRep.FunTy {ft_arg = arg, ft_res = res}
#else
pattern FunTy arg res <- TyCoRep.FunTy arg res
#endif

#if MIN_VERSION_ghc(9,0,0)
-- type HasSrcSpan x a = (GenLocated SrcSpan a ~ x)
-- type HasSrcSpan x = () :: Constraint

class HasSrcSpan a where
  getLoc :: a -> SrcSpan

instance HasSrcSpan (GenLocated SrcSpan a) where
  getLoc = GHC.getLoc

-- getLoc :: GenLocated l a -> l
-- getLoc = GHC.getLoc

#elif MIN_VERSION_ghc(8,8,0)
type HasSrcSpan = SrcLoc.HasSrcSpan
getLoc :: SrcLoc.HasSrcSpan a => a -> SrcSpan
getLoc = SrcLoc.getLoc

#else

class HasSrcSpan a where
    getLoc :: a -> SrcSpan
instance HasSrcSpan Name where
    getLoc = nameSrcSpan
instance HasSrcSpan (GenLocated SrcSpan a) where
    getLoc = SrcLoc.getLoc

#endif

#if !MIN_VERSION_ghc(8,8,0)

-- | Add the @-boot@ suffix to all output file paths associated with the
-- module, not including the input file itself
addBootSuffixLocnOut :: ModLocation -> ModLocation
addBootSuffixLocnOut locn
  = locn { ml_hi_file  = Module.addBootSuffix (ml_hi_file locn)
         , ml_obj_file = Module.addBootSuffix (ml_obj_file locn)
         }

getRealSrcSpan :: RealLocated a -> SrcLoc.RealSrcSpan
getRealSrcSpan = SrcLoc.getLoc
#endif
