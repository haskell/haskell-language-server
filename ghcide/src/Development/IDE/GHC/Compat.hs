-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    hPutStringBuffer,
    addIncludePathsQuote,
    getModuleHash,
    setUpTypedHoles,
    lookupNameCache,
    disableWarningsAsErrors,
    reLoc,
    reLocA,
    renderMessages,
    pattern PFailedWithErrorMessages,
    myCoreToStgExpr,
    Usage(..),
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
    corePrepExpr,
    corePrepPgm,
    lintInteractiveExpr,
    icInteractiveModule,
    HomePackageTable,
    lookupHpt,
    loadModulesHome,
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


    Dependencies(dep_direct_mods),
    NameCacheUpdater,

    XModulePs(..),

#if !MIN_VERSION_ghc(9,7,0)
    liftZonkM,
    nonDetFoldOccEnv,
#endif

#if MIN_VERSION_ghc(9,7,0)
    tcInitTidyEnv,
#endif
    ) where

import           Compat.HieAst                           (enrichHie)
import           Compat.HieBin
import           Compat.HieTypes                         hiding
                                                         (nodeAnnotations)
import qualified Compat.HieTypes                         as GHC (nodeAnnotations)
import           Compat.HieUtils
import           Control.Applicative                     ((<|>))
import qualified Data.ByteString                         as BS
import           Data.Coerce                             (coerce)
import           Data.List                               (foldl')
import qualified Data.Map                                as Map
import qualified Data.Set                                as S
import           Data.String                             (IsString (fromString))
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat.Iface
import           Development.IDE.GHC.Compat.Logger
import           Development.IDE.GHC.Compat.Outputable
import           Development.IDE.GHC.Compat.Parser
import           Development.IDE.GHC.Compat.Plugins
import           Development.IDE.GHC.Compat.Units
import           Development.IDE.GHC.Compat.Util
import           GHC                                     hiding (ModLocation,
                                                          RealSrcSpan, exprType,
                                                          getLoc, lookupName)
import           Prelude                                 hiding (mod)

import qualified GHC.Core.Opt.Pipeline                   as GHC
import           GHC.Core.Tidy                           (tidyExpr)
import           GHC.CoreToStg.Prep                      (corePrepPgm)
import qualified GHC.CoreToStg.Prep                      as GHC
import           GHC.Driver.Hooks                        (hscCompileCoreExprHook)

import           GHC.ByteCode.Asm                        (bcoFreeNames)
import           GHC.Core
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Driver.Session                      hiding (ExposePackage)
import           GHC.Iface.Make                          (mkIfaceExports)
import           GHC.SysTools.Tasks                      (runPp, runUnlit)
import           GHC.Types.Annotations                   (AnnTarget (ModuleTarget),
                                                          Annotation (..),
                                                          extendAnnEnvList)
import qualified GHC.Types.Avail                         as Avail
import           GHC.Types.Unique.DFM                    as UniqDFM
import           GHC.Types.Unique.DSet                   as UniqDSet
import           GHC.Types.Unique.Set                    as UniqSet
import           GHC.Types.Var.Env

import           GHC.Builtin.Uniques
import           GHC.ByteCode.Types
import           GHC.Core.Lint.Interactive               (interactiveInScope)
import           GHC.CoreToStg
import           GHC.Data.Maybe
import           GHC.Driver.Config.Core.Lint.Interactive (lintInteractiveExpr)
import           GHC.Driver.Config.Core.Opt.Simplify     (initSimplifyExprOpts)
import           GHC.Driver.Config.CoreToStg             (initCoreToStgOpts)
import           GHC.Driver.Config.CoreToStg.Prep        (initCorePrepConfig)
import           GHC.Driver.Config.Stg.Pipeline
import           GHC.Driver.Env                          as Env
import           GHC.Iface.Env
import           GHC.Linker.Loader                       (loadDecls, loadExpr)
import           GHC.Runtime.Context                     (icInteractiveModule)
import           GHC.Stg.Pipeline
import           GHC.Stg.Syntax
import           GHC.StgToByteCode
import           GHC.Types.CostCentre
import           GHC.Types.IPE
import           GHC.Types.SrcLoc                        (combineRealSrcSpans)
import           GHC.Unit.Home.ModInfo                   (HomePackageTable,
                                                          lookupHpt)
import           GHC.Unit.Module.Deps                    (Dependencies (dep_direct_mods),
                                                          Usage (..))
import           GHC.Unit.Module.ModIface

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if MIN_VERSION_ghc(9,7,0)
import           GHC.Tc.Zonk.TcType                      (tcInitTidyEnv)
#endif

#if !MIN_VERSION_ghc(9,7,0)
liftZonkM :: a -> a
liftZonkM = id

nonDetFoldOccEnv :: (a -> b -> b) -> b -> OccEnv a -> b
nonDetFoldOccEnv = foldOccEnv
#endif


type ModIfaceAnnotation = Annotation


myCoreToStgExpr :: Logger -> DynFlags -> InteractiveContext
            -> Bool
                -> Module -> ModLocation -> CoreExpr
                -> IO ( Id
                      ,[CgStgTopBinding] -- output program
                      , InfoTableProvMap
                      , CollectedCCs )
myCoreToStgExpr logger dflags ictxt
                for_bytecode
                this_mod ml prepd_expr = do
    {- Create a temporary binding (just because myCoreToStg needs a
       binding for the stg2stg step) -}
    let bco_tmp_id = mkSysLocal (fsLit "BCO_toplevel")
                                (mkPseudoUniqueE 0)
                                ManyTy
                                (exprType prepd_expr)
    (stg_binds, prov_map, collected_ccs) <-
       myCoreToStg logger
                   dflags
                   ictxt
                   for_bytecode
                   this_mod
                   ml
                   [NonRec bco_tmp_id prepd_expr]
    return (bco_tmp_id, stg_binds, prov_map, collected_ccs)

myCoreToStg :: Logger -> DynFlags -> InteractiveContext
            -> Bool
            -> Module -> ModLocation -> CoreProgram
            -> IO ( [CgStgTopBinding] -- output program
                  , InfoTableProvMap
                  , CollectedCCs )  -- CAF cost centre info (declared and used)
myCoreToStg logger dflags ictxt
            for_bytecode
            this_mod ml prepd_binds = do
    let (stg_binds, denv, cost_centre_info)
         = {-# SCC "Core2Stg" #-}
           coreToStg
             (initCoreToStgOpts dflags)
             this_mod ml prepd_binds

#if MIN_VERSION_ghc(9,8,0)
    (unzip -> (stg_binds2,_),_)
#else
    (stg_binds2,_)
#endif
        <- {-# SCC "Stg2Stg" #-}
           stg2stg logger
                   (interactiveInScope ictxt)
                   (initStgPipelineOpts dflags for_bytecode) this_mod stg_binds

    return (stg_binds2, denv, cost_centre_info)

#if MIN_VERSION_ghc(9,9,0)
reLocA :: (HasLoc (GenLocated a e), HasAnnotation b)
      => GenLocated a e -> GenLocated b e
reLocA = reLoc
#endif

getDependentMods :: ModIface -> [ModuleName]
getDependentMods = map (gwib_mod . snd) . S.toList . dep_direct_mods . mi_deps

simplifyExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
simplifyExpr _ env = GHC.simplifyExpr (Development.IDE.GHC.Compat.Env.hsc_logger env) (ue_eps (Development.IDE.GHC.Compat.Env.hsc_unit_env env)) (initSimplifyExprOpts (hsc_dflags env) (hsc_IC env))

corePrepExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
corePrepExpr _ env expr = do
  cfg <- initCorePrepConfig env
  GHC.corePrepExpr (Development.IDE.GHC.Compat.Env.hsc_logger env) cfg expr

renderMessages :: PsMessages -> (Bag WarnMsg, Bag ErrMsg)
renderMessages msgs =
  let renderMsgs extractor = (fmap . fmap) GhcPsMessage . getMessages $ extractor msgs
  in (renderMsgs psWarnings, renderMsgs psErrors)

pattern PFailedWithErrorMessages :: forall a b. (b -> Bag (MsgEnvelope GhcMessage)) -> ParseResult a
pattern PFailedWithErrorMessages msgs
     <- PFailed (const . fmap (fmap GhcPsMessage) . getMessages . getPsErrorMessages -> msgs)
{-# COMPLETE POk, PFailedWithErrorMessages #-}

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

type NameCacheUpdater = NameCache

mkHieFile' :: ModSummary
           -> [Avail.AvailInfo]
#if MIN_VERSION_ghc(9,11,0)
           -> (HieASTs Type, NameEntityInfo)
#else
           -> HieASTs Type
#endif
           -> BS.ByteString
           -> Hsc HieFile
mkHieFile' ms exports
#if MIN_VERSION_ghc(9,11,0)
            (asts, entityInfo)
#else
            asts
#endif
            src = do
  let Just src_file = ml_hs_file $ ms_location ms
      (asts',arr) = compressTypes asts
  return $ HieFile
      { hie_hs_file = src_file
      , hie_module = ms_mod ms
      , hie_types = arr
      , hie_asts = asts'
#if MIN_VERSION_ghc(9,11,0)
      , hie_entity_infos = entityInfo
#endif
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

setUpTypedHoles :: DynFlags -> DynFlags
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
  { refLevelHoleFits = refLevelHoleFits df <|> Just 1   -- becomes slow at higher levels

   -- Sometimes GHC can emit a lot of hole fits, this causes editors to be slow
   -- or just crash, we limit the hole fits to 10. The number was chosen
   -- arbirtarily by the author.
  , maxRefHoleFits   = maxRefHoleFits df <|> Just 10
  , maxValidHoleFits = maxValidHoleFits df <|> Just 10
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
  = GHC96
  | GHC98
  | GHC910
  | GHC912
  deriving (Eq, Ord, Show, Enum)

ghcVersionStr :: String
ghcVersionStr = VERSION_ghc

ghcVersion :: GhcVersion
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
ghcVersion = GHC912
#elif MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)
ghcVersion = GHC910
#elif MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
ghcVersion = GHC98
#else
ghcVersion = GHC96
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
  hscUpdateHUG (\hug -> foldl' (flip addHomeModInfoToHug) hug mod_infos) (e { hsc_type_env_vars = emptyKnotVars })

recDotDot :: HsRecFields (GhcPass p) arg -> Maybe Int
recDotDot x =
            unRecFieldsDotDot <$>
            unLoc <$> rec_dotdot x

extract_cons (NewTypeCon x)      = [x]
extract_cons (DataTypeCons _ xs) = xs
