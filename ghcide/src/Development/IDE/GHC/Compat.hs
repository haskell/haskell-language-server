-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE PatternSynonyms   #-}

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    hPutStringBuffer,
    addIncludePathsQuote,
    getModuleHash,
    setUpTypedHoles,
    NameCacheUpdater(..),
#if MIN_VERSION_ghc(9,3,0)
    nameEnvElts,
#else
    upNameCache,
#endif
    lookupNameCache,
    disableWarningsAsErrors,
    reLoc,
    reLocA,
    renderMessages,
    pattern PFailedWithErrorMessages,

#if !MIN_VERSION_ghc(9,3,0)
    extendModSummaryNoDeps,
    emsModSummary,
#endif
    myCoreToStgExpr,

    Usage(..),

    liftZonkM,

    FastStringCompat,
    bytesFS,
    mkFastStringByteString,
    nodeInfo',
    getNodeIds,
    getSourceNodeIds,
    sourceNodeInfo,
    generatedNodeInfo,
    simpleNodeInfoCompat,
    isAnnotationInNodeInfo,
    nodeAnnotations,
    mkAstNode,
    combineRealSrcSpans,
#if !MIN_VERSION_ghc(9,3,0)
    nonDetOccEnvElts,
#endif
    nonDetFoldOccEnv,

    isQualifiedImport,
    GhcVersion(..),
    ghcVersion,
    ghcVersionStr,
    -- * HIE Compat
    HieFileResult(..),
    HieFile(..),
    hieExportNames,
    mkHieFile',
    enrichHie,
    writeHieFile,
    readHieFile,
    setHieDir,
    dontWriteHieFiles,
    module Compat.HieTypes,
    module Compat.HieUtils,
    -- * Compat modules
    module Development.IDE.GHC.Compat.Core,
    module Development.IDE.GHC.Compat.Env,
    module Development.IDE.GHC.Compat.Iface,
    module Development.IDE.GHC.Compat.Logger,
    module Development.IDE.GHC.Compat.Outputable,
    module Development.IDE.GHC.Compat.Parser,
    module Development.IDE.GHC.Compat.Plugins,
    module Development.IDE.GHC.Compat.Units,
    -- * Extras that rely on compat modules
    -- * SysTools
    Option (..),
    runUnlit,
    runPp,

    -- * Recompilation avoidance
    hscCompileCoreExprHook,
    CoreExpr,
    simplifyExpr,
    tidyExpr,
    emptyTidyEnv,
#if MIN_VERSION_ghc(9,7,0)
    tcInitTidyEnv,
#endif
    corePrepExpr,
    corePrepPgm,
    lintInteractiveExpr,
    icInteractiveModule,
    HomePackageTable,
    lookupHpt,
    loadModulesHome,
#if MIN_VERSION_ghc(9,3,0)
    Dependencies(dep_direct_mods),
#else
    Dependencies(dep_mods),
#endif
    bcoFreeNames,
    ModIfaceAnnotation,
    pattern Annotation,
    AnnTarget(ModuleTarget),
    extendAnnEnvList,
    module UniqDSet,
    module UniqSet,
    module UniqDFM,
    getDependentMods,
    flattenBinds,
    mkRnEnv2,
    emptyInScopeSet,
    Unfolding(..),
    noUnfolding,
    loadExpr,
    byteCodeGen,
    bc_bcos,
    loadDecls,
    hscInterp,
    expectJust,
    extract_cons,
    recDotDot,
#if MIN_VERSION_ghc(9,5,0)
    XModulePs(..),
#endif
    ) where

import           Prelude                               hiding (mod)
import           Development.IDE.GHC.Compat.Core hiding (moduleUnitId)
import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat.Iface
import           Development.IDE.GHC.Compat.Logger
import           Development.IDE.GHC.Compat.Outputable
import           Development.IDE.GHC.Compat.Parser
import           Development.IDE.GHC.Compat.Plugins
import           Development.IDE.GHC.Compat.Units
import           Development.IDE.GHC.Compat.Util
import           GHC                                   hiding (HasSrcSpan,
                                                        ModLocation,
                                                        RealSrcSpan, exprType,
                                                        getLoc, lookupName)
import           Data.Coerce                           (coerce)
import           Data.String                           (IsString (fromString))
import           Compat.HieAst                         (enrichHie)
import           Compat.HieBin
import           Compat.HieTypes                       hiding (nodeAnnotations)
import qualified Compat.HieTypes                       as GHC (nodeAnnotations)
import           Compat.HieUtils
import qualified Data.ByteString                       as BS
import           Data.List                             (foldl')
import qualified Data.Map                              as Map
import qualified Data.Set                              as S

import qualified GHC.Core.Opt.Pipeline                 as GHC
import           GHC.Core.Tidy                         (tidyExpr)
import           GHC.CoreToStg.Prep                    (corePrepPgm)
import qualified GHC.CoreToStg.Prep                    as GHC
import           GHC.Driver.Hooks                      (hscCompileCoreExprHook)

import           GHC.ByteCode.Asm                      (bcoFreeNames)
import           GHC.Types.Annotations                 (AnnTarget (ModuleTarget),
                                                        Annotation (..),
                                                        extendAnnEnvList)
import           GHC.Types.Unique.DFM                  as UniqDFM
import           GHC.Types.Unique.DSet                 as UniqDSet
import           GHC.Types.Unique.Set                  as UniqSet
import           GHC.Data.FastString
import           GHC.Core
import           GHC.Data.StringBuffer
import           GHC.Driver.Session                    hiding (ExposePackage)
import           GHC.Types.Var.Env
import           GHC.Iface.Make                        (mkIfaceExports)
import           GHC.SysTools.Tasks                    (runUnlit, runPp)
import qualified GHC.Types.Avail                       as Avail

import           GHC.Iface.Env
import           GHC.Types.SrcLoc                      (combineRealSrcSpans)
import           GHC.Runtime.Context                   (icInteractiveModule)
import           GHC.Unit.Home.ModInfo                 (HomePackageTable,
                                                        lookupHpt)
import           GHC.Driver.Env                        as Env
import           GHC.Unit.Module.ModIface
import           GHC.Builtin.Uniques
import           GHC.ByteCode.Types
import           GHC.CoreToStg
import           GHC.Data.Maybe
import           GHC.Linker.Loader                     (loadDecls, loadExpr)
import           GHC.Stg.Pipeline
import           GHC.Stg.Syntax
import           GHC.StgToByteCode
import           GHC.Types.CostCentre
import           GHC.Types.IPE

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,3,0)
import           GHC.Unit.Module.Deps (Dependencies(dep_mods), Usage(..))
import           GHC.Unit.Module.ModSummary
import           GHC.Runtime.Interpreter
import           Data.IORef
#endif

#if MIN_VERSION_ghc(9,3,0)
import GHC.Unit.Module.Deps (Dependencies(dep_direct_mods), Usage(..))
import GHC.Driver.Config.Stg.Pipeline
#endif

#if !MIN_VERSION_ghc(9,5,0)
import           GHC.Core.Lint                         (lintInteractiveExpr)
#endif

#if MIN_VERSION_ghc(9,5,0)
import           GHC.Core.Lint.Interactive                           (interactiveInScope)
import           GHC.Driver.Config.Core.Lint.Interactive             (lintInteractiveExpr)
import           GHC.Driver.Config.Core.Opt.Simplify                 (initSimplifyExprOpts)
import           GHC.Driver.Config.CoreToStg                         (initCoreToStgOpts)
import           GHC.Driver.Config.CoreToStg.Prep                    (initCorePrepConfig)
#endif

#if MIN_VERSION_ghc(9,7,0)
import           GHC.Tc.Zonk.TcType                    (tcInitTidyEnv)
#endif

#if !MIN_VERSION_ghc(9,7,0)
liftZonkM :: a -> a
liftZonkM = id

nonDetFoldOccEnv :: (a -> b -> b) -> b -> OccEnv a -> b
nonDetFoldOccEnv = foldOccEnv
#endif

#if !MIN_VERSION_ghc(9,3,0)
nonDetOccEnvElts :: OccEnv a -> [a]
nonDetOccEnvElts = occEnvElts
#endif

type ModIfaceAnnotation = Annotation

#if MIN_VERSION_ghc(9,3,0)
nameEnvElts :: NameEnv a -> [a]
nameEnvElts = nonDetNameEnvElts
#endif

myCoreToStgExpr :: Logger -> DynFlags -> InteractiveContext
#if MIN_VERSION_ghc(9,3,0)
            -> Bool
#endif
                -> Module -> ModLocation -> CoreExpr
                -> IO ( Id
#if MIN_VERSION_ghc(9,3,0)
                      ,[CgStgTopBinding] -- output program
#else
                      ,[StgTopBinding] -- output program
#endif
                      , InfoTableProvMap
                      , CollectedCCs )
myCoreToStgExpr logger dflags ictxt
#if MIN_VERSION_ghc(9,3,0)
                for_bytecode
#endif
                this_mod ml prepd_expr = do
    {- Create a temporary binding (just because myCoreToStg needs a
       binding for the stg2stg step) -}
    let bco_tmp_id = mkSysLocal (fsLit "BCO_toplevel")
                                (mkPseudoUniqueE 0)
#if MIN_VERSION_ghc(9,5,0)
                                ManyTy
#else
                                Many
#endif
                                (exprType prepd_expr)
    (stg_binds, prov_map, collected_ccs) <-
       myCoreToStg logger
                   dflags
                   ictxt
#if MIN_VERSION_ghc(9,3,0)
                   for_bytecode
#endif
                   this_mod
                   ml
                   [NonRec bco_tmp_id prepd_expr]
    return (bco_tmp_id, stg_binds, prov_map, collected_ccs)

myCoreToStg :: Logger -> DynFlags -> InteractiveContext
#if MIN_VERSION_ghc(9,3,0)
            -> Bool
#endif
            -> Module -> ModLocation -> CoreProgram
#if MIN_VERSION_ghc(9,3,0)
            -> IO ( [CgStgTopBinding] -- output program
#else
            -> IO ( [StgTopBinding] -- output program
#endif
                  , InfoTableProvMap
                  , CollectedCCs )  -- CAF cost centre info (declared and used)
myCoreToStg logger dflags ictxt
#if MIN_VERSION_ghc(9,3,0)
            for_bytecode
#endif
            this_mod ml prepd_binds = do
    let (stg_binds, denv, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg
#if MIN_VERSION_ghc(9,5,0)
             (initCoreToStgOpts dflags)
#else
             dflags
#endif
             this_mod ml prepd_binds

#if MIN_VERSION_ghc(9,8,0)
    (unzip -> (stg_binds2,_),_)
#elif MIN_VERSION_ghc(9,4,2)
    (stg_binds2,_)
#else
    stg_binds2
#endif
        <- {-# SCC "Stg2Stg" #-}
#if MIN_VERSION_ghc(9,3,0)
           stg2stg logger
#if MIN_VERSION_ghc(9,5,0)
                   (interactiveInScope ictxt)
#else
                   ictxt
#endif
                   (initStgPipelineOpts dflags for_bytecode) this_mod stg_binds
#else
           stg2stg logger dflags ictxt this_mod stg_binds
#endif

    return (stg_binds2, denv, cost_centre_info)



getDependentMods :: ModIface -> [ModuleName]
#if MIN_VERSION_ghc(9,3,0)
getDependentMods = map (gwib_mod . snd) . S.toList . dep_direct_mods . mi_deps
#else
getDependentMods = map gwib_mod . dep_mods . mi_deps
#endif

simplifyExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
#if MIN_VERSION_ghc(9,5,0)
simplifyExpr _ env = GHC.simplifyExpr (Development.IDE.GHC.Compat.Env.hsc_logger env) (ue_eps (Development.IDE.GHC.Compat.Env.hsc_unit_env env)) (initSimplifyExprOpts (hsc_dflags env) (hsc_IC env))
#else
simplifyExpr _ = GHC.simplifyExpr
#endif

corePrepExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
#if MIN_VERSION_ghc(9,5,0)
corePrepExpr _ env expr = do
  cfg <- initCorePrepConfig env
  GHC.corePrepExpr (Development.IDE.GHC.Compat.Env.hsc_logger env) cfg expr
#else
corePrepExpr _ = GHC.corePrepExpr
#endif

renderMessages :: PsMessages -> (Bag WarnMsg, Bag ErrMsg)
renderMessages msgs =
#if MIN_VERSION_ghc(9,3,0)
  let renderMsgs extractor = (fmap . fmap) renderDiagnosticMessageWithHints . getMessages $ extractor msgs
  in (renderMsgs psWarnings, renderMsgs psErrors)
#else
  msgs
#endif

pattern PFailedWithErrorMessages :: forall a b. (b -> Bag (MsgEnvelope DecoratedSDoc)) -> ParseResult a
pattern PFailedWithErrorMessages msgs
#if MIN_VERSION_ghc(9,3,0)
     <- PFailed (const . fmap (fmap renderDiagnosticMessageWithHints) . getMessages . getPsErrorMessages -> msgs)
#else
     <- PFailed (const . fmap pprError . getErrorMessages -> msgs)
#endif
{-# COMPLETE POk, PFailedWithErrorMessages #-}

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

#if MIN_VERSION_ghc(9,3,0)
type NameCacheUpdater = NameCache
#else

lookupNameCache :: Module -> OccName -> NameCache -> (NameCache, Name)
-- Lookup up the (Module,OccName) in the NameCache
-- If you find it, return it; if not, allocate a fresh original name and extend
-- the NameCache.
-- Reason: this may the first occurrence of (say) Foo.bar we have encountered.
-- If we need to explore its value we will load Foo.hi; but meanwhile all we
-- need is a Name for it.
lookupNameCache mod occ name_cache =
  case lookupOrigNameCache (nsNames name_cache) mod occ of {
    Just name -> (name_cache, name);
    Nothing   ->
        case takeUniqFromSupply (nsUniqs name_cache) of {
          (uniq, us) ->
              let
                name      = mkExternalName uniq mod occ noSrcSpan
                new_cache = extendNameCache (nsNames name_cache) mod occ name
              in (name_cache{ nsUniqs = us, nsNames = new_cache }, name) }}

upNameCache :: IORef NameCache -> (NameCache -> (NameCache, c)) -> IO c
upNameCache = updNameCache
#endif

mkHieFile' :: ModSummary
           -> [Avail.AvailInfo]
           -> HieASTs Type
           -> BS.ByteString
           -> Hsc HieFile
mkHieFile' ms exports asts src = do
  let Just src_file = ml_hs_file $ ms_location ms
      (asts',arr) = compressTypes asts
  return $ HieFile
      { hie_hs_file = src_file
      , hie_module = ms_mod ms
      , hie_types = arr
      , hie_asts = asts'
      -- mkIfaceExports sorts the AvailInfos for stability
      , hie_exports = mkIfaceExports exports
      , hie_hs_src = src
      }

addIncludePathsQuote :: FilePath -> DynFlags -> DynFlags
addIncludePathsQuote path x = x{includePaths = f $ includePaths x}
    where f i = i{includePathsQuote = path : includePathsQuote i}

setHieDir :: FilePath -> DynFlags -> DynFlags
setHieDir _f d = d { hieDir = Just _f}

dontWriteHieFiles :: DynFlags -> DynFlags
dontWriteHieFiles d = gopt_unset d Opt_WriteHie

setUpTypedHoles ::DynFlags -> DynFlags
setUpTypedHoles df
  = flip gopt_unset Opt_AbstractRefHoleFits    -- too spammy
  $ flip gopt_unset Opt_ShowDocsOfHoleFits     -- not used
  $ flip gopt_unset Opt_ShowMatchesOfHoleFits  -- nice but broken (forgets module qualifiers)
  $ flip gopt_unset Opt_ShowProvOfHoleFits     -- not used
  $ flip gopt_unset Opt_ShowTypeAppOfHoleFits  -- not used
  $ flip gopt_unset Opt_ShowTypeAppVarsOfHoleFits -- not used
  $ flip gopt_unset Opt_ShowTypeOfHoleFits     -- massively simplifies parsing
  $ flip gopt_set   Opt_SortBySubsumHoleFits   -- very nice and fast enough in most cases
  $ flip gopt_unset Opt_SortValidHoleFits
  $ flip gopt_unset Opt_UnclutterValidHoleFits
  $ df
  { refLevelHoleFits = Just 1   -- becomes slow at higher levels
  , maxRefHoleFits   = Just 10  -- quantity does not impact speed
  , maxValidHoleFits = Nothing  -- quantity does not impact speed
  }


nameListFromAvails :: [Avail.AvailInfo] -> [(SrcSpan, Name)]
nameListFromAvails as =
  map (\n -> (nameSrcSpan n, n)) (concatMap Avail.availNames as)


getModuleHash :: ModIface -> Fingerprint
getModuleHash = mi_mod_hash . mi_final_exts


disableWarningsAsErrors :: DynFlags -> DynFlags
disableWarningsAsErrors df =
    flip gopt_unset Opt_WarnIsError $! foldl' wopt_unset_fatal df [toEnum 0 ..]

isQualifiedImport :: ImportDecl a -> Bool
isQualifiedImport ImportDecl{ideclQualified = NotQualified} = False
isQualifiedImport ImportDecl{}                              = True
isQualifiedImport _                                         = False

-- | Like getNodeIds but with generated node removed
getSourceNodeIds :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
getSourceNodeIds = Map.foldl' combineNodeIds Map.empty . Map.filterWithKey (\k _ -> k == SourceInfo) . getSourcedNodeInfo . sourcedNodeInfo

getNodeIds :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
getNodeIds = Map.foldl' combineNodeIds Map.empty . getSourcedNodeInfo . sourcedNodeInfo

combineNodeIds :: Map.Map Identifier (IdentifierDetails a)
                        -> NodeInfo a -> Map.Map Identifier (IdentifierDetails a)
ad `combineNodeIds` (NodeInfo _ _ bd) = Map.unionWith (<>) ad bd

--  Copied from GHC and adjusted to accept TypeIndex instead of Type
-- nodeInfo' :: Ord a => HieAST a -> NodeInfo a
nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' = Map.foldl' combineNodeInfo' emptyNodeInfo . getSourcedNodeInfo . sourcedNodeInfo

combineNodeInfo' :: Ord a => NodeInfo a -> NodeInfo a -> NodeInfo a
(NodeInfo as ai ad) `combineNodeInfo'` (NodeInfo bs bi bd) =
  NodeInfo (S.union as bs) (mergeSorted ai bi) (Map.unionWith (<>) ad bd)
  where
    mergeSorted :: Ord a => [a] -> [a] -> [a]
    mergeSorted la@(a:axs) lb@(b:bxs) = case compare a b of
                                        LT -> a : mergeSorted axs lb
                                        EQ -> a : mergeSorted axs bxs
                                        GT -> b : mergeSorted la bxs
    mergeSorted axs [] = axs
    mergeSorted [] bxs = bxs

sourceNodeInfo :: HieAST a -> Maybe (NodeInfo a)
sourceNodeInfo = Map.lookup SourceInfo . getSourcedNodeInfo . sourcedNodeInfo

generatedNodeInfo :: HieAST a -> Maybe (NodeInfo a)
generatedNodeInfo = Map.lookup GeneratedInfo . getSourcedNodeInfo . sourcedNodeInfo

data GhcVersion
  = GHC92
  | GHC94
  | GHC96
  | GHC98
  deriving (Eq, Ord, Show)

ghcVersionStr :: String
ghcVersionStr = VERSION_ghc

ghcVersion :: GhcVersion
#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
ghcVersion = GHC98
#elif MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
ghcVersion = GHC96
#elif MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
ghcVersion = GHC94
#elif MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
ghcVersion = GHC92
#endif

simpleNodeInfoCompat :: FastStringCompat -> FastStringCompat -> NodeInfo a
simpleNodeInfoCompat ctor typ = simpleNodeInfo (coerce ctor) (coerce typ)

isAnnotationInNodeInfo :: (FastStringCompat, FastStringCompat) -> NodeInfo a -> Bool
isAnnotationInNodeInfo p = S.member p . nodeAnnotations

nodeAnnotations :: NodeInfo a -> S.Set (FastStringCompat, FastStringCompat)
nodeAnnotations = S.map (\(NodeAnnotation ctor typ) -> (coerce ctor, coerce typ)) . GHC.nodeAnnotations

newtype FastStringCompat = FastStringCompat LexicalFastString
    deriving (Show, Eq, Ord)

instance IsString FastStringCompat where
    fromString = FastStringCompat . LexicalFastString . fromString

mkAstNode :: NodeInfo a -> Span -> [HieAST a] -> HieAST a
mkAstNode n = Node (SourcedNodeInfo $ Map.singleton GeneratedInfo n)

-- | Load modules, quickly. Input doesn't need to be desugared.
-- A module must be loaded before dependent modules can be typechecked.
-- This variant of loadModuleHome will *never* cause recompilation, it just
-- modifies the session.
-- The order modules are loaded is important when there are hs-boot files.
-- In particular you should make sure to load the .hs version of a file after the
-- .hs-boot version.
loadModulesHome
    :: [HomeModInfo]
    -> HscEnv
    -> HscEnv
loadModulesHome mod_infos e =
#if MIN_VERSION_ghc(9,3,0)
  hscUpdateHUG (\hug -> foldl' (flip addHomeModInfoToHug) hug mod_infos) (e { hsc_type_env_vars = emptyKnotVars })
#else
  let !new_modules = addListToHpt (hsc_HPT e) [(mod_name x, x) | x <- mod_infos]
  in e { hsc_HPT = new_modules
       , hsc_type_env_var = Nothing
       }
    where
      mod_name = moduleName . mi_module . hm_iface
#endif

recDotDot :: HsRecFields (GhcPass p) arg -> Maybe Int
recDotDot x =
#if MIN_VERSION_ghc(9,5,0)
            unRecFieldsDotDot <$>
#endif
            unLoc <$> rec_dotdot x

#if MIN_VERSION_ghc(9,5,0)
extract_cons (NewTypeCon x) = [x]
extract_cons (DataTypeCons _ xs) = xs
#else
extract_cons = id
#endif
