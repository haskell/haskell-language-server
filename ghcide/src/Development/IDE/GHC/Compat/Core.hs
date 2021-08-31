{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
-- TODO: remove
{-# OPTIONS -Wno-dodgy-imports #-}

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
#if MIN_VERSION_ghc(8,8,0)
    CommandLineOption,
#if !MIN_VERSION_ghc(9,2,0)
    staticPlugins,
#endif
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
    WarnReason(..),
    wWarningFlags,
    updOptLevel,
    -- slightly unsafe
    setUnsafeGlobalDynFlags,
    -- * Linear Haskell
    Scaled,
    scaledThing,
    -- * UniqueSupply
    UniqSupply,
    takeUniqFromSupply,
    mkSplitUniqSupply,
    -- * ConLike
    ConLike(..),
    conLikeName,
    conLikeFieldLabels,
    conLikeFieldType,
    conLikeIsInfix,
    conLikeInstOrigArgTys,
    conLikeResTy,
    DataCon,
    dataConFieldLabels,
    dataConName,
    dataConWrapId,
    nilDataCon,
    dataConCannotMatch,
    isDataConName,
    isTupleDataCon,
    tupleDataCon,
    consDataCon,
    dataConIsInfix,
    dataConInstSig,
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
    -- * Fixity
    LexicalFixity(..),
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
    -- * InstEnvs
    InstEnvs(..),
    lookupInstEnv,
    -- * FamInstEnvs
    FamInstEnvs,
    normaliseType,
    -- * Var
    Id,
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
    isBoolTy,
    isFloatingTy,
    isIntTy,
    isIntegerTy,
    isStringTy,
#if MIN_VERSION_ghc(8,10,0)
    coercionKind,
    isCoercionTy_maybe,
#else
    isCoercionTy,
    splitCoercionType_maybe,
#endif
    substTyAddInScope,
    piResultTys,
    splitAppTys,
    splitFunTys,
    splitFunTy_maybe,
    splitPiTys,
    Development.IDE.GHC.Compat.Core.splitForAllTyCoVars,
    splitTyConApp_maybe,
    TCvSubst,
    extendTCvSubst,
    emptyTCvSubst,
    nonDetCmpType,
    substTy,
    unionTCvSubst,
    zipTvSubst,
    TyThing(..),
    binderVar,
    pprTyThingInContext,
    pprTypeForUser,
    TyVar,
    setTyVarUnique,
    getTyVar_maybe,
    Var,
    varType,
    varName,
    mkVarOcc,
    setVarUnique,
    Development.IDE.GHC.Compat.Core.mkVisFunTys,
    mkAppTys,
    mkTyVarTy,
    mkTyConTy,
    Development.IDE.GHC.Compat.Core.mkInfForAllTys,
    charTy,
    eqType,
    tcView,
    exprType,
    isAlgType,
    -- * Wired in types
    unitDataConId,
    charTyCon,
    doubleTyCon,
    floatTyCon,
    intTyCon,
    funTyCon,
    alphaTy,
    alphaTys,
    alphaTyVar,
    betaTy,
    betaTyVar,
    listTyCon,
    maybeTyCon,
    unitTyCon,
    -- * TyCoVar
    TyCoVar,
    tyCoVarsOfTypeList,
    tyCoVarsOfTypeWellScoped,
    Development.IDE.GHC.Compat.Core.dataConExTyCoVars,
    dataConOrigTyCon,
    dataConInstArgTys,
    -- * TyCon
    TyCon,
    tyConName,
    tyConDataCons,
    tyConClass_maybe,
    eqPrimTyCon,
#if MIN_VERSION_ghc(8,8,0)
    eqTyCon,
#endif
    eqTyCon_RDR,
    isTupleTyCon,
    -- * Class
    Class(..),
    classMinimalDef,
    classMethods,
    classSCTheta,
    -- * Id
    idName,
    idType,
    -- * GlobalRdrEnv
    GlobalRdrEnv,
    GlobalRdrElt(..),
    lookupGlobalRdrEnv,
    globalRdrEnvElts,
    lookupGRE_Name,
    -- * OccEnv
    OccEnv,
    emptyOccEnv,
    lookupOccEnv,
    -- * Specs
    ImpDeclSpec(..),
    ImportSpec(..),
    -- * SourceText
    SourceText(..),
    -- * Name
#if !MIN_VERSION_ghc(9,0,0)
    NameOrRdrName,
#endif
    Name,
    isValName,
    isSystemName,
    isInternalName,
    nameSrcSpan,
    nameSrcLoc,
    nameRdrName,
    nameModule,
    nameModule_maybe,
    isQual,
    isQual_maybe,
    getSrcSpan,
    RdrName(..),
    mkRdrUnqual,
    rdrNameFieldOcc,
    HasOccName,
    getOccName,
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
    mkClsOcc,
    mkVarOccFS,
    pprNameDefnLoc,
    Parent(..),
    tyThingParent_maybe,
    -- * Field Occs
    FieldOcc,
    mkFieldOcc,
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
    -- * NameSet
    NameSet,
    FreeVars,
    elemNameSet,
    mkNameSet,
    Avail.availsToNameSet,
    -- * TcGblEnv
    TcGblEnv(..),
    -- * Renamer stage
    RnM,
    rnTopSpliceDecls,
    rnSplicePat,
    rnSpliceType,
    rnSpliceExpr,
    -- * Rename Stage Names
    findImportUsage,
    getMinimalImports,
    -- * FieldLabel
    FieldLabel,
    flSelector,
    flLabel,
    -- * Header Parser
    getOptions,
    -- * ErrUtils
    Severity(..),
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
    HsMatchContext(..),
    LHsExpr,
    HsExpr(..),
    isAtomicHsExpr,
    LIE,
    IE(..),
    ieName,
    ieNames,
    IEWrappedName(..),
    IEWildcard(..),
    LPat,
    Pat(..),
    ListPatTc(..),
    LHsDecl,
    HsDecl(..),
    TyClDecl(..),
    TyClGroup(..),
    HsDataDefn(..),
    ConDecl(..),
    InstDecl(..),
    ClsInst,
    is_dfun,
    ClsInstDecl(..),
    DataFamInstDecl(..),
    TyFamInstDecl(..),
    FamEqn(..),
    DerivDecl(..),
    LRuleDecls,
    RuleDecl(..),
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
    HsConDeclDetails,
    LHsBinds,
    LHsBind,
    HsBind,
    ABExport(..),
    LHsBindLR,
    HsBindLR(..),
    PatSynBind(..),
    HsGroup(..),
    MatchGroup(..),
    HsSplice(..),
    LHsSigType,
    LHsType,
    HsType(..),
    HsRecField,
    HsRecField'(..),
    HsRecFields(..),
    LImportDecl,
    ImportDecl(..),
#if MIN_VERSION_ghc(8,10,0)
    ImportDeclQualifiedStyle(..),
#endif
    HsWrapper(..),
    HsWildCardBndrs(..),
    HsImplicitBndrs(..),
    LConDeclField,
    ConDeclField(..),
    HsValBindsLR(..),
    NHsValBindsLR(..),
    LMatch,
    Match(..),
    StmtLR(..),
    GRHS(..),
    GRHSs(..),
    HsLocalBinds,
    HsLocalBindsLR(..),
#if !MIN_VERSION_ghc(9,0,0)
    UnboundVar(..),
#endif
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
    patSynExTyVars,
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
    makeSimpleDetails,
    -- * Typecheck utils
    TcM,
    initTc,
    initTcWithGbl,
    tcLookup,
    tcLookupDataFamInst_maybe,
    tcSplitAppTys,
    tcSplitPhiTy,
    tcSplitTyConApp,
    tcSplitTyConApp_maybe,
    tcSplitFunTys,
    tcSplitNestedSigmaTys,
    Development.IDE.GHC.Compat.Core.tcSplitForAllTyVars,
    Development.IDE.GHC.Compat.Core.tcSplitForAllTyVarBinder_maybe,
    tcSplitSigmaTy,
    TcTyThing(..),
    tcTyConAppTyCon_maybe,
    tcVisibleOrphanMods,
    tcRnImportDecls,
    typecheckIface,
    mkIfaceTc,
    finalSafeMode,
    ImportAvails(..),
    ImportedModsVal(..),
    importedByUser,
    collectHsBindsBinders,
    TypecheckedSource,
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
    SrcLoc.RealSrcLoc,
    SrcLoc.SrcLoc(..),
    BufSpan,
    SrcLoc.containsSpan,
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
#if !MIN_VERSION_ghc(8,10,0) && MIN_VERSION_ghc(8,8,0)
    SrcLoc.dL,
#endif
    -- * Finder
    FindResult(..),
    mkHomeModLocation,
    addBootSuffixLocnOut,
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
    pRELUDE,
    mkPrelImports,
    knownKeyNames,
    -- * Utils with no home, neither here nor in GHC
    mkVarBind,
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
    getContext,
    setContext,
    parseImportDecl,
    runDecls,
    Warn(..),
    -- * Desugared
    dsExpr,
    initDs,
    -- * PredType
    PredType,
    ThetaType,
    -- * Role
    Role(..),
    -- * Module extraction
    extractModule,
    -- * Ppr utils
    showToHeader,
    pprDefinedAt,
    pprInfixName,
    -- * Panic
    panic,
    ) where

import           GHC                        hiding (HasSrcSpan, ModLocation,
                                             Phase, RealSrcSpan, exprType,
                                             getLoc, lookupName, moduleUnitId,
                                             parseModule)

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC
import           GHC.Builtin.Names
import           GHC.Builtin.Types
import           GHC.Builtin.Types.Prim
import           GHC.Builtin.Utils
import           GHC.Core.Class
import           GHC.Core.Coercion
import           GHC.Core.ConLike
import           GHC.Core.DataCon           as DataCon
import           GHC.Core.FamInstEnv
import           GHC.Core.InstEnv
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Core.Multiplicity      (scaledThing)
#else
import           GHC.Core.Ppr.TyThing
import           GHC.Core.TyCo.Rep          (scaledThing)
#endif
import           GHC.Core.PatSyn
import           GHC.Core.Predicate
import qualified GHC.Core.TyCo.Rep          as TyCoRep
import           GHC.Core.TyCon
import           GHC.Core.Type              as TcType
import           GHC.Core.Utils

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env
#else
import           GHC.Driver.Finder
import           GHC.Driver.Types
import           GHC.Driver.Ways
#endif
import           GHC.Driver.CmdLine         (Warn (..))
import           GHC.Driver.Hooks
import           GHC.Driver.Main
import           GHC.Driver.Monad
import           GHC.Driver.Phases
import           GHC.Driver.Pipeline
import           GHC.Driver.Plugins
import           GHC.Driver.Session         hiding (ExposePackage)
import qualified GHC.Driver.Session         as DynFlags
import           GHC.HsToCore.Docs
import           GHC.HsToCore.Expr
import           GHC.HsToCore.Monad
import           GHC.Iface.Load
import           GHC.Iface.Make             (mkFullIface, mkIfaceTc,
                                             mkPartialIface)
import           GHC.Iface.Recomp
import           GHC.Iface.Syntax
import           GHC.Iface.Tidy
import           GHC.IfaceToCore
import           GHC.Parser
import           GHC.Parser.Header
import           GHC.Parser.Lexer
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Linker.Loader
import           GHC.Linker.Types
import           GHC.Platform.Ways
#else
import           GHC.Runtime.Linker
#endif
import           GHC.Rename.Names
import           GHC.Rename.Splice
import           GHC.Runtime.Interpreter
import           GHC.Tc.Instance.Family
import           GHC.Tc.Module
import           GHC.Tc.Types
import           GHC.Tc.Types.Evidence
import           GHC.Tc.Utils.Env
import           GHC.Tc.Utils.Monad
import           GHC.Tc.Utils.TcType        as TcType
import qualified GHC.Types.Avail            as Avail
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Types.Meta
#endif
import           GHC.Types.Basic
import           GHC.Types.Id
import           GHC.Types.Name             hiding (varName)
import           GHC.Types.Name.Cache
import           GHC.Types.Name.Env
import           GHC.Types.Name.Reader
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Types.Name.Set
import           GHC.Types.SourceFile       (HscSource (..),
                                             SourceModified (..))
import           GHC.Types.SourceText
import           GHC.Types.TyThing
import           GHC.Types.TyThing.Ppr
#else
import           GHC.Types.Name.Set
#endif
import           GHC.Types.SrcLoc           (BufSpan)
import qualified GHC.Types.SrcLoc           as SrcLoc
import           GHC.Types.Unique.Supply
import           GHC.Types.Var
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Unit.Finder
import           GHC.Unit.Home.ModInfo
#endif
import           GHC.Unit.Info              (PackageName (..))
import           GHC.Unit.Module
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Unit.Module.Imported
import           GHC.Unit.Module.ModDetails
import           GHC.Unit.Module.ModGuts
import           GHC.Unit.Module.ModIface   (IfaceExport)
#endif
import           GHC.Unit.State             (ModuleOrigin (..))
import           GHC.Utils.Panic            hiding (try)
#else
import           ConLike
import           DataCon
import           DynFlags                   hiding (ExposePackage)
import qualified DynFlags
import           Finder
import           Module
#if MIN_VERSION_ghc(9,0,1)
import           GHC.Core.TyCo.Ppr          (pprSigmaType)
import           GHC.Core.TyCo.Rep          (Scaled, scaledThing)
import           GHC.Iface.Load
import           GHC.Types.Unique.Set       (emptyUniqSet)
import           Module                     (unitString)
#endif
import qualified Avail
import           BasicTypes
import           Class
import           CmdLineParser              (Warn (..))
import           CoreUtils                  (exprType)
import           DriverPhases
import           DriverPipeline
import           DsExpr
import           DsMonad
import           ExtractDocs                (extractDocs)
import           FamInst
import           FamInstEnv
import           GHCi
import           GhcMonad
import           HeaderInfo
import           Hooks
import           HscMain
import           HscTypes
import           Id
import           IfaceSyn
import           InstEnv
import           Lexer
import           Linker
import           LoadIface
import           MkIface
import           Name                       hiding (varName)
import           NameCache
import           NameEnv
import           NameSet
import           Packages
import           Panic                      hiding (try)
import           Parser
import           PatSyn
#if MIN_VERSION_ghc(8,8,0)
import           Plugins
#endif
import           PprTyThing
import           PrelInfo
import           PrelNames
import           RdrName
import           RnNames
import           RnSplice
import qualified SrcLoc
import           TcEnv
import           TcEvidence
import           TcIface
import           TcRnDriver
import           TcRnMonad
import           TcType
import           TidyPgm
import qualified TyCoRep
import           TyCon
import           Type
import           TysPrim
import           TysWiredIn
import           UniqSupply
import           Var

#if MIN_VERSION_ghc(8,10,0)
import           Coercion                   (coercionKind)
import           Predicate
#else
import           SrcLoc                     (RealLocated)
#endif
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

dataConExTyCoVars :: DataCon -> [TyCoVar]
#if __GLASGOW_HASKELL__ >= 808
dataConExTyCoVars = DataCon.dataConExTyCoVars
#else
dataConExTyCoVars = DataCon.dataConExTyVars
#endif

#if !MIN_VERSION_ghc(9,0,0)
-- Linear Haskell
type Scaled a = a
scaledThing :: Scaled a -> a
scaledThing = id
#endif

mkVisFunTys :: [Scaled Type] -> Type -> Type
mkVisFunTys =
#if __GLASGOW_HASKELL__ <= 808
  mkFunTys
#else
  TcType.mkVisFunTys
#endif

mkInfForAllTys :: [TyVar] -> Type -> Type
mkInfForAllTys =
#if MIN_VERSION_ghc(9,0,0)
  TcType.mkInfForAllTys
#else
  mkInvForAllTys
#endif

splitForAllTyCoVars :: Type -> ([TyCoVar], Type)
splitForAllTyCoVars =
#if MIN_VERSION_ghc(9,2,0)
  TcType.splitForAllTyCoVars
#else
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

