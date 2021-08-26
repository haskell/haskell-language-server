-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS -Wno-incomplete-uni-patterns #-}

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    NameCacheUpdater(..),
    hPutStringBuffer,
    addIncludePathsQuote,
    getModuleHash,
    setUpTypedHoles,
    GHC.ModLocation,
    Module.addBootSuffix,
    pattern ModLocation,
    ml_hs_file,
    ml_obj_file,
    ml_hi_file,
    ml_hie_file,
    upNameCache,
    disableWarningsAsErrors,

#if MIN_VERSION_ghc(8,10,0)
    module GHC.Hs.Extension,
#else
    module HsExtension,
    noExtField,
#endif

#if !MIN_VERSION_ghc(9,0,1)
    RefMap,
#endif
    -- Linear
    Scaled,
    scaledThing,

#if MIN_VERSION_ghc(9,0,0)
    IsBootInterface(..),
#else
    pattern IsBoot,
    pattern NotBoot,
#endif

    nodeInfo',
    getNodeIds,
    stringToUnit,
    unitString,

    pprSigmaType,

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
    ) where

import           GHC                    hiding (HasSrcSpan, ModLocation, getLoc,
                                         lookupName, RealSrcSpan)
import qualified GHC

import Development.IDE.GHC.Compat.Core
import Development.IDE.GHC.Compat.Env
import Development.IDE.GHC.Compat.Iface
import Development.IDE.GHC.Compat.Logger
import Development.IDE.GHC.Compat.Outputable
import Development.IDE.GHC.Compat.Parser
import Development.IDE.GHC.Compat.Plugins
import Development.IDE.GHC.Compat.Units

#if MIN_VERSION_ghc(9,0,0)
import GHC.Core.DataCon (dataConWrapId)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.Multiplicity
import qualified GHC.Core.TyCo.Rep as TyCoRep
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Data.Bag
import GHC.Driver.Session hiding (ExposePackage)
import qualified GHC.Driver.Session as DynFlags
#if !MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Types
#endif
import GHC.Hs.Extension
import qualified GHC.Hs.Type as GHC
import GHC.Iface.Load
import GHC.Iface.Make (mkIfaceExports)
import GHC.Unit.Info (PackageName)
import qualified GHC.Unit.Info as Packages
import qualified GHC.Unit.Module.Location as Module
import GHC.Unit.Module.Name (moduleNameSlashes)
import GHC.Unit.State (ModuleOrigin(..))
import qualified GHC.Unit.State as Packages
import qualified GHC.Unit.Types as Module
import GHC.Unit.Types (unitString, IsBootInterface(..))
import GHC.Utils.Fingerprint
import GHC.Utils.Panic
import qualified GHC.SysTools.Tasks as SysTools
import GHC.Tc.Types (TcGblEnv(..))
import GHC.Tc.Utils.TcType (pprSigmaType)
import qualified GHC.Types.Avail as Avail
import GHC.Types.FieldLabel
import GHC.Types.Name
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Cache
import GHC.Types.Name.Env
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (BufSpan)
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Var
import           Control.Exception.Safe as Safe (Exception, MonadCatch, catch)
#else
import           DynFlags               hiding (ExposePackage)
import qualified Module
#if MIN_VERSION_ghc(9,0,0)
import           Control.Exception.Safe as Safe (Exception, MonadCatch, catch)
import           GHC.Core.TyCo.Ppr      (pprSigmaType)
import           GHC.Core.TyCo.Rep      (Scaled, scaledThing)
import           GHC.Iface.Load
import           GHC.Types.Unique.Set   (emptyUniqSet)
import           Module                 (unitString)
#else
import           TcType                 (pprSigmaType)
#endif

import           HscTypes
import           MkIface hiding (writeIfaceFile)
#if MIN_VERSION_ghc(8,10,0)
import           GHC.Hs.Extension
#else
import           HsExtension
#endif
import qualified Avail

#if MIN_VERSION_ghc(8,8,0)
import           StringBuffer           (hPutStringBuffer)
#endif
import qualified SysTools

#if !MIN_VERSION_ghc(8,8,0)
import           SrcLoc                 (RealLocated)
import           System.FilePath        ((-<.>))
import qualified EnumSet

import           Foreign.ForeignPtr
import           System.IO
#endif
#endif

import           Compat.HieAst          (enrichHie)
import           Compat.HieBin
import           Compat.HieTypes
import           Compat.HieUtils
import qualified Data.ByteString        as BS
import           Data.IORef

import qualified Data.Map               as Map

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import qualified Data.Set               as S
#endif
#if MIN_VERSION_ghc(8,8,0)
import           Data.List              (foldl')
#else
import           Data.List              (foldl', isSuffixOf)
#endif

#if !MIN_VERSION_ghc(8,8,0)
hPutStringBuffer :: Handle -> StringBuffer -> IO ()
hPutStringBuffer hdl (StringBuffer buf len cur)
    = withForeignPtr (plusForeignPtr buf cur) $ \ptr ->
             hPutBuf hdl ptr len
#endif

#if !MIN_VERSION_ghc(8,10,0)
noExtField :: NoExt
noExtField = noExt
#endif

supportsHieFiles :: Bool
supportsHieFiles = True

hieExportNames :: HieFile -> [(SrcSpan, Name)]
hieExportNames = nameListFromAvails . hie_exports

#if !MIN_VERSION_ghc(8,8,0)
ml_hie_file :: GHC.ModLocation -> FilePath
ml_hie_file ml
  | "boot" `isSuffixOf ` ml_hi_file ml = ml_hi_file ml -<.> ".hie-boot"
  | otherwise  = ml_hi_file ml -<.> ".hie"
#endif

upNameCache :: IORef NameCache -> (NameCache -> (NameCache, c)) -> IO c
#if MIN_VERSION_ghc(8,8,0)
upNameCache = updNameCache
#else
upNameCache ref upd_fn
  = atomicModifyIORef' ref upd_fn
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

pattern ModLocation :: Maybe FilePath -> FilePath -> FilePath -> GHC.ModLocation
#if MIN_VERSION_ghc(8,8,0)
pattern ModLocation a b c <-
    GHC.ModLocation a b c _ where ModLocation a b c = GHC.ModLocation a b c ""
#else
pattern ModLocation a b c <-
    GHC.ModLocation a b c where ModLocation a b c = GHC.ModLocation a b c
#endif

setHieDir :: FilePath -> DynFlags -> DynFlags
setHieDir _f d =
#if MIN_VERSION_ghc(8,8,0)
    d { hieDir     = Just _f}
#else
    d
#endif

dontWriteHieFiles :: DynFlags -> DynFlags
dontWriteHieFiles d =
#if MIN_VERSION_ghc(8,8,0)
    gopt_unset d Opt_WriteHie
#else
    d
#endif

setUpTypedHoles ::DynFlags -> DynFlags
setUpTypedHoles df
  = flip gopt_unset Opt_AbstractRefHoleFits    -- too spammy
#if MIN_VERSION_ghc(8,8,0)
  $ flip gopt_unset Opt_ShowDocsOfHoleFits     -- not used
#endif
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
#if MIN_VERSION_ghc(8,10,0)
getModuleHash = mi_mod_hash . mi_final_exts
#else
getModuleHash = mi_mod_hash
#endif

#if MIN_VERSION_ghc(9,2,0)

packageName            = Packages.unitPackageName
moduleUnitId           = Module.moduleUnit
thisInstalledUnitId    = GHC.homeUnitId_
thisPackage            = GHC.homeUnitId_

#elif MIN_VERSION_ghc(9,0,0)
packageName            = Packages.unitPackageName
getPackageIncludePath  = Packages.getUnitIncludePath
moduleUnitId           = Module.moduleUnit
-- initUnits              = Packages.initUnits
-- initPackages           = initPackagesx

thisInstalledUnitId    = GHC.homeUnitId
thisPackage            = DynFlags.homeUnit
#else


pattern NotBoot, IsBoot :: IsBootInterface
pattern NotBoot = False
pattern IsBoot = True


-- Linear Haskell
type Scaled a = a
scaledThing :: Scaled a -> a
scaledThing = id
#endif


disableWarningsAsErrors :: DynFlags -> DynFlags
disableWarningsAsErrors df =
    flip gopt_unset Opt_WarnIsError $ foldl' wopt_unset_fatal df [toEnum 0 ..]

#if !MIN_VERSION_ghc(8,8,0)
wopt_unset_fatal :: DynFlags -> WarningFlag -> DynFlags
wopt_unset_fatal dfs f
    = dfs { fatalWarningFlags = EnumSet.delete f (fatalWarningFlags dfs) }
#endif

isQualifiedImport :: ImportDecl a -> Bool
#if MIN_VERSION_ghc(8,10,0)
isQualifiedImport ImportDecl{ideclQualified = NotQualified} = False
isQualifiedImport ImportDecl{}                              = True
#else
isQualifiedImport ImportDecl{ideclQualified}                = ideclQualified
#endif
isQualifiedImport _                                         = False



#if MIN_VERSION_ghc(9,0,0)
getNodeIds :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
getNodeIds = Map.foldl' combineNodeIds Map.empty . getSourcedNodeInfo . sourcedNodeInfo

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

stringToUnit = Module.stringToUnit
#else

getNodeIds :: HieAST a -> NodeIdentifiers a
getNodeIds = nodeIdentifiers . nodeInfo
-- import qualified FastString as FS

-- nodeInfo' :: HieAST TypeIndex -> NodeInfo TypeIndex
nodeInfo' :: Ord a => HieAST a -> NodeInfo a
nodeInfo' = nodeInfo
-- type Unit = UnitId
unitString :: Unit -> String
unitString = Module.unitIdString
stringToUnit :: String -> Unit
stringToUnit = Module.stringToUnitId
-- moduleUnit :: Module -> Unit
-- moduleUnit = moduleUnitId
-- unhelpfulSpanFS :: FS.FastString -> FS.FastString
-- unhelpfulSpanFS = id
#endif

data GhcVersion
  = GHC86
  | GHC88
  | GHC810
  | GHC90
  | GHC92
  deriving (Eq, Ord, Show)

ghcVersionStr :: String
ghcVersionStr = VERSION_ghc

ghcVersion :: GhcVersion
#if MIN_VERSION_GLASGOW_HASKELL(9,2,0,0)
ghcVersion = GHC92
#elif MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)
ghcVersion = GHC90
#elif MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
ghcVersion = GHC810
#elif MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)
ghcVersion = GHC88
#elif MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)
ghcVersion = GHC86
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
