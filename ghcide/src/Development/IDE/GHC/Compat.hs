-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS -Wno-incomplete-uni-patterns -Wno-dodgy-imports #-}

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    mkHomeModLocation,
    hPutStringBuffer,
    addIncludePathsQuote,
    getModuleHash,
    setUpTypedHoles,
    NameCacheUpdater(..),
#if MIN_VERSION_ghc(9,3,0)
    getMessages,
    renderDiagnosticMessageWithHints,
    nameEnvElts,
#else
    upNameCache,
#endif
    lookupNameCache,
    disableWarningsAsErrors,
    reLoc,
    reLocA,
    getPsMessages,
    renderMessages,
    pattern PFailedWithErrorMessages,
    isObjectLinkable,

#if !MIN_VERSION_ghc(9,0,1)
    RefMap,
#endif

#if MIN_VERSION_ghc(9,2,0)
#if !MIN_VERSION_ghc(9,3,0)
    extendModSummaryNoDeps,
    emsModSummary,
#endif
    myCoreToStgExpr,
#endif

    FastStringCompat,
    bytesFS,
    mkFastStringByteString,
    nodeInfo',
    getNodeIds,
    sourceNodeInfo,
    generatedNodeInfo,
    simpleNodeInfoCompat,
    isAnnotationInNodeInfo,
    nodeAnnotations,
    mkAstNode,
    combineRealSrcSpans,

    nonDetOccEnvElts,

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
    supportsHieFiles,
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
#if MIN_VERSION_ghc(9,2,0)
    loadExpr,
    byteCodeGen,
    bc_bcos,
    loadDecls,
    hscInterp,
    expectJust,
#else
    coreExprToBCOs,
    linkExpr,
#endif
    ) where

import           Data.Bifunctor
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


#if MIN_VERSION_ghc(9,0,0)
import           GHC.Core.Lint                         (lintInteractiveExpr)
import qualified GHC.Core.Opt.Pipeline                 as GHC
import           GHC.Core.Tidy                         (tidyExpr)
import           GHC.CoreToStg.Prep                    (corePrepPgm)
import qualified GHC.CoreToStg.Prep                    as GHC
import           GHC.Driver.Hooks                      (hscCompileCoreExprHook)
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Linker.Loader                     (loadExpr)
import           GHC.Linker.Types                      (isObjectLinkable)
import           GHC.Runtime.Context                   (icInteractiveModule)
import           GHC.Unit.Home.ModInfo                 (HomePackageTable,
                                                        lookupHpt)
#if MIN_VERSION_ghc(9,3,0)
import GHC.Unit.Module.Deps (Dependencies(dep_direct_mods))
#else
import GHC.Unit.Module.Deps (Dependencies(dep_mods))
#endif
#else
import           GHC.CoreToByteCode                    (coreExprToBCOs)
import           GHC.Driver.Types                      (Dependencies (dep_mods),
                                                        HomePackageTable,
                                                        icInteractiveModule,
                                                        lookupHpt)
import           GHC.Runtime.Linker                    (linkExpr)
#endif
import           GHC.ByteCode.Asm                      (bcoFreeNames)
import           GHC.Types.Annotations                 (AnnTarget (ModuleTarget),
                                                        Annotation (..),
                                                        extendAnnEnvList)
import           GHC.Types.Unique.DFM                  as UniqDFM
import           GHC.Types.Unique.DSet                 as UniqDSet
import           GHC.Types.Unique.Set                  as UniqSet
#else
import           Annotations                           (AnnTarget (ModuleTarget),
                                                        Annotation (..),
                                                        extendAnnEnvList)
import           ByteCodeAsm                           (bcoFreeNames)
import           ByteCodeGen                           (coreExprToBCOs)
import           CoreLint                              (lintInteractiveExpr)
import           CorePrep                              (corePrepExpr,
                                                        corePrepPgm)
import           CoreSyn                               (CoreExpr,
                                                        Unfolding (..),
                                                        flattenBinds,
                                                        noUnfolding)
import           CoreTidy                              (tidyExpr)
import           Hooks                                 (hscCompileCoreExprHook)
import           Linker                                (linkExpr)
import qualified SimplCore                             as GHC
import           UniqDFM
import           UniqDSet
import           UniqSet
import           VarEnv                                (emptyInScopeSet,
                                                        emptyTidyEnv, mkRnEnv2)
#endif

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Data.FastString
import           GHC.Core
import           GHC.Data.StringBuffer
import           GHC.Driver.Session                    hiding (ExposePackage)
import qualified GHC.Types.SrcLoc                      as SrcLoc
import           GHC.Types.Var.Env
import           GHC.Utils.Error
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env                        as Env
import           GHC.Unit.Module.ModIface
import           GHC.Unit.Module.ModSummary
#else
import           GHC.Driver.Types
#endif
import           GHC.Iface.Env
import           GHC.Iface.Make                        (mkIfaceExports)
import qualified GHC.SysTools.Tasks                    as SysTools
import qualified GHC.Types.Avail                       as Avail
#else
import           FastString
import qualified Avail
import           DynFlags                              hiding (ExposePackage)
import           HscTypes
import           MkIface                               hiding (writeIfaceFile)

import           StringBuffer                          (hPutStringBuffer)
import qualified SysTools
#endif

import           Compat.HieAst                         (enrichHie)
import           Compat.HieBin
import           Compat.HieTypes                       hiding (nodeAnnotations)
import qualified Compat.HieTypes                       as GHC (nodeAnnotations)
import           Compat.HieUtils
import qualified Data.ByteString                       as BS
import           Data.IORef

import           Data.List                             (foldl')
import qualified Data.Map                              as Map
import qualified Data.Set                              as S

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Builtin.Uniques
import           GHC.ByteCode.Types
import           GHC.CoreToStg
import           GHC.Data.Maybe
import           GHC.Linker.Loader                     (loadDecls)
import           GHC.Runtime.Interpreter
import           GHC.Stg.Pipeline
import           GHC.Stg.Syntax
import           GHC.StgToByteCode
import           GHC.Types.CostCentre
import           GHC.Types.IPE
#endif

#if MIN_VERSION_ghc(9,3,0)
import GHC.Types.Error
import GHC.Driver.Config.Stg.Pipeline
import GHC.Driver.Plugins                              (PsMessages (..))
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

#if MIN_VERSION_ghc(9,2,0)
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
                                Many
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
           coreToStg dflags this_mod ml prepd_binds

#if MIN_VERSION_ghc(9,4,2)
    (stg_binds2,_)
#else
    stg_binds2
#endif
        <- {-# SCC "Stg2Stg" #-}
#if MIN_VERSION_ghc(9,3,0)
           stg2stg logger ictxt (initStgPipelineOpts dflags for_bytecode) this_mod stg_binds
#else
           stg2stg logger dflags ictxt this_mod stg_binds
#endif

    return (stg_binds2, denv, cost_centre_info)
#endif


#if !MIN_VERSION_ghc(9,2,0)
reLoc :: Located a -> Located a
reLoc = id

reLocA :: Located a -> Located a
reLocA = id
#endif

getDependentMods :: ModIface -> [ModuleName]
#if MIN_VERSION_ghc(9,3,0)
getDependentMods = map (gwib_mod . snd) . S.toList . dep_direct_mods . mi_deps
#elif MIN_VERSION_ghc(9,0,0)
getDependentMods = map gwib_mod . dep_mods . mi_deps
#else
getDependentMods = map fst . dep_mods . mi_deps
#endif

simplifyExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
#if MIN_VERSION_ghc(9,0,0)
simplifyExpr _ = GHC.simplifyExpr

corePrepExpr :: DynFlags -> HscEnv -> CoreExpr -> IO CoreExpr
corePrepExpr _ = GHC.corePrepExpr
#else
simplifyExpr df _ = GHC.simplifyExpr df
#endif

renderMessages :: PsMessages -> (Bag WarnMsg, Bag ErrMsg)
renderMessages msgs =
#if MIN_VERSION_ghc(9,3,0)
  let renderMsgs extractor = (fmap . fmap) renderDiagnosticMessageWithHints . getMessages $ extractor msgs
  in (renderMsgs psWarnings, renderMsgs psErrors)
#else
  msgs
#endif

#if MIN_VERSION_ghc(9,2,0)
pattern PFailedWithErrorMessages :: forall a b. (b -> Bag (MsgEnvelope DecoratedSDoc)) -> ParseResult a
pattern PFailedWithErrorMessages msgs
#if MIN_VERSION_ghc(9,3,0)
     <- PFailed (const . fmap (fmap renderDiagnosticMessageWithHints) . getMessages . getPsErrorMessages -> msgs)
#else
     <- PFailed (const . fmap pprError . getErrorMessages -> msgs)
#endif
#else
pattern PFailedWithErrorMessages :: (DynFlags -> ErrorMessages) -> ParseResult a
pattern PFailedWithErrorMessages msgs
     <- PFailed (getErrorMessages -> msgs)
#endif
{-# COMPLETE POk, PFailedWithErrorMessages #-}

supportsHieFiles :: Bool
supportsHieFiles = True

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

#if !MIN_VERSION_ghc(9,0,1)
type RefMap a = Map.Map Identifier [(Span, IdentifierDetails a)]
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



#if MIN_VERSION_ghc(9,0,0)
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
    mergeSorted la@(a:as) lb@(b:bs) = case compare a b of
                                        LT -> a : mergeSorted as lb
                                        EQ -> a : mergeSorted as bs
                                        GT -> b : mergeSorted la bs
    mergeSorted as [] = as
    mergeSorted [] bs = bs

#else

getNodeIds :: HieAST a -> NodeIdentifiers a
getNodeIds = nodeIdentifiers . nodeInfo
-- import qualified FastString as FS

-- nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' :: Ord a => HieAST a -> NodeInfo a
nodeInfo' = nodeInfo
-- type Unit = UnitId
-- moduleUnit :: Module -> Unit
-- moduleUnit = moduleUnitId
-- unhelpfulSpanFS :: FS.FastString -> FS.FastString
-- unhelpfulSpanFS = id
#endif

sourceNodeInfo :: HieAST a -> Maybe (NodeInfo a)
#if MIN_VERSION_ghc(9,0,0)
sourceNodeInfo = Map.lookup SourceInfo . getSourcedNodeInfo . sourcedNodeInfo
#else
sourceNodeInfo = Just . nodeInfo
#endif

generatedNodeInfo :: HieAST a -> Maybe (NodeInfo a)
#if MIN_VERSION_ghc(9,0,0)
generatedNodeInfo = Map.lookup GeneratedInfo . getSourcedNodeInfo . sourcedNodeInfo
#else
generatedNodeInfo = sourceNodeInfo -- before ghc 9.0, we don't distinguish the source
#endif

data GhcVersion
  = GHC810
  | GHC90
  | GHC92
  | GHC94
  deriving (Eq, Ord, Show)

ghcVersionStr :: String
ghcVersionStr = VERSION_ghc

ghcVersion :: GhcVersion
#if MIN_VERSION_GLASGOW_HASKELL(9,4,0,0)
ghcVersion = GHC94
#elif MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
ghcVersion = GHC92
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
ghcVersion = GHC90
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
ghcVersion = GHC810
#endif

runUnlit :: Logger -> DynFlags -> [Option] -> IO ()
runUnlit =
#if MIN_VERSION_ghc(9,2,0)
    SysTools.runUnlit
#else
    const SysTools.runUnlit
#endif

runPp :: Logger -> DynFlags -> [Option] -> IO ()
runPp =
#if MIN_VERSION_ghc(9,2,0)
    SysTools.runPp
#else
    const SysTools.runPp
#endif

simpleNodeInfoCompat :: FastStringCompat -> FastStringCompat -> NodeInfo a
simpleNodeInfoCompat ctor typ = simpleNodeInfo (coerce ctor) (coerce typ)

isAnnotationInNodeInfo :: (FastStringCompat, FastStringCompat) -> NodeInfo a -> Bool
isAnnotationInNodeInfo p = S.member p . nodeAnnotations

nodeAnnotations :: NodeInfo a -> S.Set (FastStringCompat, FastStringCompat)
#if MIN_VERSION_ghc(9,2,0)
nodeAnnotations = S.map (\(NodeAnnotation ctor typ) -> (coerce ctor, coerce typ)) . GHC.nodeAnnotations
#else
nodeAnnotations = S.map (bimap coerce coerce) . GHC.nodeAnnotations
#endif

#if MIN_VERSION_ghc(9,2,0)
newtype FastStringCompat = FastStringCompat LexicalFastString
#else
newtype FastStringCompat = FastStringCompat FastString
#endif
    deriving (Show, Eq, Ord)

instance IsString FastStringCompat where
#if MIN_VERSION_ghc(9,2,0)
    fromString = FastStringCompat . LexicalFastString . fromString
#else
    fromString = FastStringCompat . fromString
#endif

mkAstNode :: NodeInfo a -> Span -> [HieAST a] -> HieAST a
#if MIN_VERSION_ghc(9,0,0)
mkAstNode n = Node (SourcedNodeInfo $ Map.singleton GeneratedInfo n)
#else
mkAstNode = Node
#endif

combineRealSrcSpans :: RealSrcSpan -> RealSrcSpan -> RealSrcSpan
#if MIN_VERSION_ghc(9,2,0)
combineRealSrcSpans = SrcLoc.combineRealSrcSpans
#else
combineRealSrcSpans span1 span2
  = mkRealSrcSpan (mkRealSrcLoc file line_start col_start) (mkRealSrcLoc file line_end col_end)
  where
    (line_start, col_start) = min (srcSpanStartLine span1, srcSpanStartCol span1)
                                  (srcSpanStartLine span2, srcSpanStartCol span2)
    (line_end, col_end)     = max (srcSpanEndLine span1, srcSpanEndCol span1)
                                  (srcSpanEndLine span2, srcSpanEndCol span2)
    file = srcSpanFile span1
#endif
