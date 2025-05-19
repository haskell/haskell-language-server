{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compat Core module that handles the GHC module hierarchy re-organization
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
    lookupType,
    needWiredInHomeIface,
    loadWiredInHomeIface,
    readIface,
    loadSysInterface,
    importDecl,
    CommandLineOption,
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
    wWarningFlags,
    updOptLevel,
    -- slightly unsafe
    setUnsafeGlobalDynFlags,
    -- * Linear Haskell
    scaledThing,
    -- * Interface Files
    IfaceExport,
    IfaceTyCon(..),
    ModIface,
    ModIface_(..),
#if MIN_VERSION_ghc(9,11,0)
    pattern ModIface,
    set_mi_top_env,
    set_mi_usages,
#endif
    HscSource(..),
    WhereFrom(..),
    loadInterface,
    loadModuleInterface,
    RecompileRequired(..),
    mkPartialIface,
    mkFullIface,
    IsBootInterface(..),
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
    -- * Specs
    ImpDeclSpec(..),
    ImportSpec(..),
    -- * SourceText
    SourceText(..),
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
#if !MIN_VERSION_ghc(9,7,0)
    Avail.availNamesWithSelectors,
#endif
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
    hscUpdateHPT,
    Development.IDE.GHC.Compat.Core.makeSimpleDetails,
    -- * Typecheck utils
    tcSplitForAllTyVars,
    tcSplitForAllTyVarBinder_maybe,
    typecheckIface,
    Development.IDE.GHC.Compat.Core.mkIfaceTc,
    Development.IDE.GHC.Compat.Core.mkBootModDetailsTc,
    Development.IDE.GHC.Compat.Core.initTidyOpts,
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
    GHC.getLocA,
    GHC.locA,
    GHC.noLocA,
    unLocA,
    LocatedAn,
    GHC.LocatedA,
    GHC.AnnListItem(..),
    GHC.NameAnn(..),
    SrcLoc.RealLocated,
    SrcLoc.GenLocated(..),
    SrcLoc.SrcSpan(SrcLoc.UnhelpfulSpan),
    SrcLoc.RealSrcSpan,
    pattern RealSrcSpan,
    SrcLoc.RealSrcLoc,
    pattern RealSrcLoc,
    SrcLoc.SrcLoc(SrcLoc.UnhelpfulLoc),
    BufSpan,
#if !MIN_VERSION_ghc(9,9,0)
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
    SrcLoc.srcSpanToRealSrcSpan,
    mapLoc,
    -- * Finder
    FindResult(..),
    mkHomeModLocation,
    findObjectLinkableMaybe,
    InstalledFindResult(..),
    -- * Module and Package
    ModuleOrigin(..),
    PackageName(..),
    -- * Linker
#if MIN_VERSION_ghc(9,11,0)
    LinkablePart(..),
#else
    Unlinked(..),
#endif
    Linkable(..),
    unload,
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
    mkSimpleTarget,
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
    -- * Other
    GHC.CoreModule(..),
    GHC.SafeHaskellMode(..),
    pattern GRE,
    gre_name,
    gre_imp,
    gre_lcl,
    gre_par,
    collectHsBindsBinders,
    -- * Util Module re-exports
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
    module GHC.Iface.Recomp,

    module GHC.Hs.Decls,
    module GHC.Hs.Expr,
    module GHC.Hs.Doc,
    module GHC.Hs.Extension,
    module GHC.Hs.ImpExp,
    module GHC.Hs.Pat,
    module GHC.Hs.Type,
    module GHC.Hs.Utils,
    module Language.Haskell.Syntax,

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
    module GHC.Types.Name,
    module GHC.Types.Name.Set,
    module GHC.Types.Name.Cache,
    module GHC.Types.Name.Env,
    module GHC.Types.Name.Reader,
    module GHC.Utils.Error,
#if !MIN_VERSION_ghc(9,7,0)
    module GHC.Types.Avail,
#endif
    module GHC.Types.SourceFile,
    module GHC.Types.SourceText,
    module GHC.Types.TyThing,
    module GHC.Types.TyThing.Ppr,
    module GHC.Types.Unique.Supply,
    module GHC.Types.Var,
    module GHC.Unit.Module,
    module GHC.Unit.Module.Graph,
    -- * Syntax re-exports
    module GHC.Hs,
    module GHC.Hs.Binds,
    module GHC.Parser,
    module GHC.Parser.Header,
    module GHC.Parser.Lexer,
    module GHC.Utils.Panic,
    CompileReason(..),
    hsc_type_env_vars,
    hscUpdateHUG, hsc_HUG,
    GhcMessage(..),
    getKey,
    module GHC.Driver.Env.KnotVars,
    module GHC.Linker.Types,
    module GHC.Types.Unique.Map,
    module GHC.Utils.TmpFs,
    module GHC.Unit.Finder.Types,
    module GHC.Unit.Env,
    module GHC.Driver.Phases,
#if !MIN_VERSION_ghc(9,4,0)
    pattern HsFieldBind,
    hfbAnn,
    hfbLHS,
    hfbRHS,
    hfbPun,
#endif
#if !MIN_VERSION_ghc_boot_th(9,4,1)
    Extension(.., NamedFieldPuns),
#else
    Extension(..),
#endif
    mkCgInteractiveGuts,
    justBytecode,
    justObjects,
    emptyHomeModInfoLinkable,
    homeModInfoByteCode,
    homeModInfoObject,
#if !MIN_VERSION_ghc(9,5,0)
    field_label,
#endif
    groupOrigin,
    isVisibleFunArg,
#if MIN_VERSION_ghc(9,8,0)
    lookupGlobalRdrEnv
#endif
    ) where

import qualified GHC

-- NOTE(ozkutuk): Cpp clashes Phase.Cpp, so we hide it.
-- Not the greatest solution, but gets the job done
-- (until the CPP extension is actually needed).
import           GHC.LanguageExtensions.Type hiding (Cpp)

import           GHC.Builtin.Names           hiding (Unique, printName)
import           GHC.Builtin.Types
import           GHC.Builtin.Types.Prim
import           GHC.Builtin.Utils
import           GHC.Core                    (CoreProgram)
import           GHC.Core.Class
import           GHC.Core.Coercion
import           GHC.Core.ConLike
import           GHC.Core.DataCon            hiding (dataConExTyCoVars)
import qualified GHC.Core.DataCon            as DataCon
import           GHC.Core.FamInstEnv         hiding (pprFamInst)
import           GHC.Core.InstEnv
import           GHC.Core.PatSyn
import           GHC.Core.Predicate
import           GHC.Core.TyCo.Ppr
import qualified GHC.Core.TyCo.Rep           as TyCoRep
import           GHC.Core.TyCon
import           GHC.Core.Type
import           GHC.Core.Unify
import           GHC.Core.Utils
import           GHC.Driver.CmdLine          (Warn (..))
import           GHC.Driver.Hooks
import           GHC.Driver.Main             as GHC
import           GHC.Driver.Monad
import           GHC.Driver.Phases
import           GHC.Driver.Pipeline
import           GHC.Driver.Plugins
import           GHC.Driver.Session          hiding (ExposePackage)
import qualified GHC.Driver.Session          as DynFlags
import           GHC.Hs.Binds
import           GHC.HsToCore.Docs
import           GHC.HsToCore.Expr
import           GHC.HsToCore.Monad
import           GHC.Iface.Load
import           GHC.Iface.Make              as GHC
import           GHC.Iface.Recomp
import           GHC.Iface.Syntax
import           GHC.Iface.Tidy              as GHC
import           GHC.IfaceToCore
import           GHC.Parser
import           GHC.Parser.Header           hiding (getImports)
import           GHC.Rename.Fixity           (lookupFixityRn)
import           GHC.Rename.Names
import           GHC.Rename.Splice
import qualified GHC.Runtime.Interpreter     as GHCi
import           GHC.Tc.Instance.Family
import           GHC.Tc.Module
import           GHC.Tc.Types
import           GHC.Tc.Types.Evidence       hiding ((<.>))
import           GHC.Tc.Utils.Env
import           GHC.Tc.Utils.Monad          hiding (Applicative (..), IORef,
                                              MonadFix (..), MonadIO (..), allM,
                                              anyM, concatMapM, mapMaybeM, foldMapM,
                                              (<$>))
import           GHC.Tc.Utils.TcType         as TcType
import qualified GHC.Types.Avail             as Avail
import           GHC.Types.Basic
import           GHC.Types.Id
import           GHC.Types.Name              hiding (varName)
import           GHC.Types.Name.Cache
import           GHC.Types.Name.Env
import           GHC.Types.Name.Reader       hiding (GRE, gre_imp, gre_lcl,
                                              gre_name, gre_par)
import qualified GHC.Types.Name.Reader       as RdrName
import           GHC.Types.SrcLoc            (BufPos, BufSpan,
                                              SrcLoc (UnhelpfulLoc),
                                              SrcSpan (UnhelpfulSpan))
import qualified GHC.Types.SrcLoc            as SrcLoc
import           GHC.Types.Unique.FM
import           GHC.Types.Unique.Supply
import           GHC.Types.Var               (Var (varName), setTyVarUnique,
                                              setVarUnique)

import qualified GHC.Types.Var               as TypesVar
import           GHC.Unit.Info               (PackageName (..))
import           GHC.Unit.Module             hiding (ModLocation (..), UnitId,
                                              moduleUnit, toUnitId)
import qualified GHC.Unit.Module             as Module
import           GHC.Unit.State              (ModuleOrigin (..))
import           GHC.Utils.Error             (Severity (..), emptyMessages)
import           GHC.Utils.Panic             hiding (try)
import qualified GHC.Utils.Panic.Plain       as Plain


import           Data.Foldable               (toList)
import           GHC.Core.Multiplicity       (scaledThing)
import           GHC.Data.Bag
import qualified GHC.Data.Strict             as Strict
import qualified GHC.Driver.Config.Finder    as GHC
import qualified GHC.Driver.Config.Tidy      as GHC
import           GHC.Driver.Env
import           GHC.Driver.Env              as GHCi
import           GHC.Driver.Env.KnotVars
import           GHC.Driver.Errors.Types
import           GHC.Hs                      (HsModule (..))
import           GHC.Hs.Decls                hiding (FunDep)
import           GHC.Hs.Doc
import           GHC.Hs.Expr
import           GHC.Hs.Extension
import           GHC.Hs.ImpExp
import           GHC.Hs.Pat
import           GHC.Hs.Type
import           GHC.Hs.Utils                hiding (collectHsBindsBinders)
import qualified GHC.Linker.Loader           as Linker
import           GHC.Linker.Types
import           GHC.Parser.Annotation       (EpAnn (..))
import           GHC.Parser.Lexer            hiding (getPsMessages,
                                              initParserState)
import           GHC.Platform.Ways
import           GHC.Runtime.Context         (InteractiveImport (..))
import           GHC.Types.Fixity            (Fixity (..), LexicalFixity (..),
                                              defaultFixity)
import           GHC.Types.Meta
import           GHC.Types.Name.Set
import           GHC.Types.SourceFile        (HscSource (..))
import           GHC.Types.SourceText
import           GHC.Types.Target            (Target (..), TargetId (..))
import           GHC.Types.TyThing
import           GHC.Types.TyThing.Ppr
import           GHC.Types.Unique
import           GHC.Types.Unique.Map
import           GHC.Unit.Env
import           GHC.Unit.Finder             hiding (mkHomeModLocation)
import qualified GHC.Unit.Finder             as GHC
import           GHC.Unit.Finder.Types
import           GHC.Unit.Home.ModInfo
import           GHC.Unit.Module.Graph
import           GHC.Unit.Module.Imported
import           GHC.Unit.Module.ModDetails
import           GHC.Unit.Module.ModGuts
#if !MIN_VERSION_ghc(9,9,0)
import           GHC.Hs                      (SrcSpanAnn')
#endif
import           GHC.Unit.Module.ModIface    (IfaceExport, ModIface,
                                              ModIface_ (..), mi_fix
#if MIN_VERSION_ghc(9,11,0)
                                             , pattern ModIface
                                             , set_mi_top_env
                                             , set_mi_usages
#endif
                                             )
import           GHC.Unit.Module.ModSummary  (ModSummary (..))
import           GHC.Utils.Error             (mkPlainErrorMsgEnvelope)
import           GHC.Utils.Panic
import           GHC.Utils.TmpFs
import           Language.Haskell.Syntax     hiding (FunDep)


-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if MIN_VERSION_ghc(9,11,0)
import System.OsPath
#endif

#if !MIN_VERSION_ghc(9,7,0)
import           GHC.Types.Avail             (greNamePrintableName)
#endif

#if !MIN_VERSION_ghc(9,9,0)
import           GHC.Hs                      (SrcSpanAnn')
#endif

mkHomeModLocation :: DynFlags -> ModuleName -> FilePath -> IO Module.ModLocation
#if MIN_VERSION_ghc(9,11,0)
mkHomeModLocation df mn f =
  let osf = unsafeEncodeUtf f
  in pure $ GHC.mkHomeModLocation (GHC.initFinderOpts df) mn osf
#else
mkHomeModLocation df mn f = pure $ GHC.mkHomeModLocation (GHC.initFinderOpts df) mn f
#endif

pattern RealSrcSpan :: SrcLoc.RealSrcSpan -> Maybe BufSpan -> SrcLoc.SrcSpan

pattern RealSrcSpan x y <- SrcLoc.RealSrcSpan x ((\case Strict.Nothing -> Nothing; Strict.Just a -> Just a) -> y) where
  RealSrcSpan x y = SrcLoc.RealSrcSpan x (case y of Nothing -> Strict.Nothing; Just a -> Strict.Just a)

{-# COMPLETE RealSrcSpan, UnhelpfulSpan #-}

pattern RealSrcLoc :: SrcLoc.RealSrcLoc -> Strict.Maybe BufPos-> SrcLoc.SrcLoc
pattern RealSrcLoc x y = SrcLoc.RealSrcLoc x y
{-# COMPLETE RealSrcLoc, UnhelpfulLoc #-}


pattern AvailTC :: Name -> [Name] -> [FieldLabel] -> Avail.AvailInfo
#if __GLASGOW_HASKELL__ >= 907
pattern AvailTC n names pieces <- Avail.AvailTC n ((,[]) -> (names,pieces))
#else
pattern AvailTC n names pieces <- Avail.AvailTC n ((\gres -> foldr (\gre (names, pieces) -> case gre of
      Avail.NormalGreName name -> (name: names, pieces)
      Avail.FieldGreName label -> (names, label:pieces)) ([], []) gres) -> (names, pieces))
#endif

pattern AvailName :: Name -> Avail.AvailInfo
#if __GLASGOW_HASKELL__ >= 907
pattern AvailName n <- Avail.Avail n
#else
pattern AvailName n <- Avail.Avail (Avail.NormalGreName n)
#endif

pattern AvailFL :: FieldLabel -> Avail.AvailInfo
#if __GLASGOW_HASKELL__ >= 907
pattern AvailFL fl <- (const Nothing -> Just fl) -- this pattern always fails as this field was removed in 9.7
#else
pattern AvailFL fl <- Avail.Avail (Avail.FieldGreName fl)
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

isVisibleFunArg :: Development.IDE.GHC.Compat.Core.FunTyFlag -> Bool
#if __GLASGOW_HASKELL__ >= 906
isVisibleFunArg = TypesVar.isVisibleFunArg
type FunTyFlag = TypesVar.FunTyFlag
#else
isVisibleFunArg VisArg = True
isVisibleFunArg _ = False
type FunTyFlag = TypesVar.AnonArgFlag
#endif
pattern FunTy :: Development.IDE.GHC.Compat.Core.FunTyFlag -> Type -> Type -> Type
pattern FunTy af arg res <- TyCoRep.FunTy {ft_af = af, ft_arg = arg, ft_res = res}


-- type HasSrcSpan x a = (GenLocated SrcSpan a ~ x)
-- type HasSrcSpan x = () :: Constraint

class HasSrcSpan a where
  getLoc :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  getLoc = id

instance HasSrcSpan (SrcLoc.GenLocated SrcSpan a) where
  getLoc = GHC.getLoc

#if MIN_VERSION_ghc(9,9,0)
instance HasSrcSpan (EpAnn a) where
  getLoc = GHC.getHasLoc
#endif

#if MIN_VERSION_ghc(9,9,0)
instance HasSrcSpan (SrcLoc.GenLocated (EpAnn ann) a) where
  getLoc (L l _) = getLoc l
instance HasSrcSpan (SrcLoc.GenLocated (GHC.EpaLocation) a) where
  getLoc = GHC.getHasLoc
#else
instance HasSrcSpan (SrcSpanAnn' ann) where
  getLoc = GHC.locA
instance HasSrcSpan (SrcLoc.GenLocated (SrcSpanAnn' ann) a) where
  getLoc (L l _) = l
#endif

pattern L :: HasSrcSpan a => SrcSpan -> e -> SrcLoc.GenLocated a e
pattern L l a <- GHC.L (getLoc -> l) a
{-# COMPLETE L #-}

-- This is from the old api, but it still simplifies
pattern ConPatIn :: SrcLoc.Located (ConLikeP GhcPs) -> HsConPatDetails GhcPs -> Pat GhcPs
#if MIN_VERSION_ghc(9,9,0)
pattern ConPatIn con args <- ConPat _ (L _ (SrcLoc.noLoc -> con)) args
  where
    ConPatIn con args = ConPat GHC.noAnn (GHC.noLocA $ SrcLoc.unLoc con) args
#else
pattern ConPatIn con args <- ConPat EpAnnNotUsed (L _ (SrcLoc.noLoc -> con)) args
  where
    ConPatIn con args = ConPat EpAnnNotUsed (GHC.noLocA $ SrcLoc.unLoc con) args
#endif

conPatDetails :: Pat p -> Maybe (HsConPatDetails p)
conPatDetails (ConPat _ _ args) = Just args
conPatDetails _ = Nothing

mapConPatDetail :: (HsConPatDetails p -> Maybe (HsConPatDetails p)) -> Pat p -> Maybe (Pat p)
mapConPatDetail f pat@(ConPat _ _ args) = (\args' -> pat { pat_args = args'}) <$> f args
mapConPatDetail _ _ = Nothing


initObjLinker :: HscEnv -> IO ()
initObjLinker env =
    GHCi.initObjLinker (GHCi.hscInterp env)

loadDLL :: HscEnv -> String -> IO (Maybe String)
loadDLL env str = do
    res <- GHCi.loadDLL (GHCi.hscInterp env) str
#if MIN_VERSION_ghc(9,11,0) || (MIN_VERSION_ghc(9, 8, 3) && !MIN_VERSION_ghc(9, 9, 0)) || (MIN_VERSION_ghc(9, 10, 2) && !MIN_VERSION_ghc(9, 11, 0))
    pure $
      case res of
        Left err_msg -> Just err_msg
        Right _      -> Nothing
#else
    pure res
#endif

unload :: HscEnv -> [Linkable] -> IO ()
unload hsc_env linkables =
  Linker.unload
    (GHCi.hscInterp hsc_env)
    hsc_env linkables


isSubspanOfA :: LocatedAn la a -> LocatedAn lb b -> Bool
isSubspanOfA a b = SrcLoc.isSubspanOf (GHC.getLocA a) (GHC.getLocA b)

type LocatedAn a = GHC.LocatedAn a

unLocA :: forall pass a. XRec (GhcPass pass) a -> a
unLocA = unXRec @(GhcPass pass)


pattern GRE :: Name -> Parent -> Bool -> [ImportSpec] -> RdrName.GlobalRdrElt
{-# COMPLETE GRE #-}
pattern GRE{gre_name, gre_par, gre_lcl, gre_imp} <- RdrName.GRE
#if MIN_VERSION_ghc(9,7,0)
    {gre_name = gre_name
#else
    {gre_name = (greNamePrintableName -> gre_name)
#endif
    ,gre_par, gre_lcl, gre_imp = (toList -> gre_imp)}

collectHsBindsBinders :: CollectPass p => LHsBindsLR p idR -> [IdP p]
collectHsBindsBinders x = GHC.collectHsBindsBinders CollNoDictBinders x



makeSimpleDetails :: HscEnv -> TcGblEnv -> IO ModDetails
makeSimpleDetails hsc_env =
  GHC.makeSimpleDetails
              (hsc_logger hsc_env)

mkIfaceTc :: HscEnv -> GHC.SafeHaskellMode -> ModDetails -> ModSummary -> Maybe CoreProgram -> TcGblEnv -> IO ModIface
mkIfaceTc hscEnv shm md _ms _mcp =
#if MIN_VERSION_ghc(9,5,0)
  GHC.mkIfaceTc hscEnv shm md _ms _mcp -- mcp::Maybe CoreProgram is only used in GHC >= 9.6
#else
  GHC.mkIfaceTc hscEnv shm md _ms -- ms::ModSummary is only used in GHC >= 9.4
#endif

mkBootModDetailsTc :: HscEnv -> TcGblEnv -> IO ModDetails
mkBootModDetailsTc session = GHC.mkBootModDetailsTc
          (hsc_logger session)


initTidyOpts :: HscEnv -> IO TidyOpts
initTidyOpts =
  GHC.initTidyOpts

driverNoStop :: StopPhase
driverNoStop = NoStop


#if !MIN_VERSION_ghc(9,4,0)
pattern HsFieldBind :: XHsRecField id -> id -> arg -> Bool -> HsRecField' id arg
pattern HsFieldBind {hfbAnn, hfbLHS, hfbRHS, hfbPun} <- HsRecField hfbAnn (SrcLoc.unLoc -> hfbLHS) hfbRHS hfbPun where
  HsFieldBind ann lhs rhs pun = HsRecField ann (SrcLoc.noLoc lhs) rhs pun
#endif

#if !MIN_VERSION_ghc_boot_th(9,4,1)
pattern NamedFieldPuns :: Extension
pattern NamedFieldPuns = RecordPuns
#endif

groupOrigin :: MatchGroup GhcRn body -> Origin
#if MIN_VERSION_ghc(9,5,0)
mapLoc :: (a -> b) -> SrcLoc.GenLocated l a -> SrcLoc.GenLocated l b
mapLoc = fmap
groupOrigin = mg_ext
#else
mapLoc :: (a -> b) -> SrcLoc.GenLocated l a -> SrcLoc.GenLocated l b
mapLoc = SrcLoc.mapLoc
groupOrigin = mg_origin
#endif


#if !MIN_VERSION_ghc(9,5,0)
mkCgInteractiveGuts :: CgGuts -> CgGuts
mkCgInteractiveGuts = id

emptyHomeModInfoLinkable :: Maybe Linkable
emptyHomeModInfoLinkable = Nothing

justBytecode :: Linkable -> Maybe Linkable
justBytecode = Just

justObjects :: Linkable -> Maybe Linkable
justObjects = Just

homeModInfoByteCode, homeModInfoObject :: HomeModInfo -> Maybe Linkable
homeModInfoByteCode = hm_linkable
homeModInfoObject = hm_linkable

field_label :: a -> a
field_label = id
#endif

mkSimpleTarget :: DynFlags -> FilePath -> Target
mkSimpleTarget df fp = Target (TargetFile fp Nothing) True (homeUnitId_ df) Nothing

#if MIN_VERSION_ghc(9,7,0)
lookupGlobalRdrEnv gre_env occ = lookupGRE gre_env (LookupOccName occ AllRelevantGREs)
#endif
