-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-fields #-} -- to enable prettyPrint
{-# OPTIONS_GHC -Wno-orphans #-}

-- | GHC utility functions. Importantly, code using our GHC should never:
--
-- * Call runGhc, use runGhcFast instead. It's faster and doesn't require config we don't have.
--
-- * Call setSessionDynFlags, use modifyDynFlags instead. It's faster and avoids loading packages.
module Development.IDE.UtilGHC(
    PackageDynFlags(..), setPackageDynFlags, getPackageDynFlags,
    modifyDynFlags,
    removeTypeableInfo,
    setPackageImports,
    setPackageDbs,
    fakeSettings,
    fakeLlvmConfig,
    prettyPrint,
    importGenerated,
    mkImport,
    runGhcFast,
    setImports,
    setThisInstalledUnitId,
    modIsInternal
    ) where

import           Config
import           Fingerprint
import           GHC                         hiding (convertLit)
import           GhcMonad
import           GhcPlugins                  as GHC hiding (fst3, (<>))
import           HscMain
import qualified Packages
import           Platform
import qualified EnumSet

import           Control.DeepSeq
import           Data.IORef
import           Data.List
import GHC.Generics (Generic)
import qualified StringBuffer as SB

----------------------------------------------------------------------
-- GHC setup

setPackageDbs :: [FilePath] -> DynFlags -> DynFlags
setPackageDbs paths dflags =
  dflags
    { packageDBFlags =
        [PackageDB $ PkgConfFile path | path <- paths] ++ [NoGlobalPackageDB, ClearPackageDBs]
    , pkgDatabase = if null paths then Just [] else Nothing
      -- if we don't load any packages set the package database to empty and loaded.
    , settings = (settings dflags)
        {sTopDir = case paths of p:_ -> p; _ -> error "No package db path available but used $topdir"
        , sSystemPackageConfig = case paths of p:_ -> p; _ -> error "No package db path available but used system package config"
        }
    }

setPackageImports :: Bool -> [(String, ModRenaming)] -> DynFlags -> DynFlags
setPackageImports hideAllPkgs pkgImports dflags = dflags {
    packageFlags = packageFlags dflags ++
        [ExposePackage pkgName (UnitIdArg $ stringToUnitId pkgName) renaming
        | (pkgName, renaming) <- pkgImports
        ]
    , generalFlags = if hideAllPkgs
                      then Opt_HideAllPackages `EnumSet.insert` generalFlags dflags
                      else generalFlags dflags
    }

modifyDynFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyDynFlags f = do
  newFlags <- f <$> getSessionDynFlags
  -- We do not use setSessionDynFlags here since we handle package
  -- initialization separately.
  modifySession $ \h ->
    h { hsc_dflags = newFlags, hsc_IC = (hsc_IC h) {ic_dflags = newFlags} }

-- | The subset of @DynFlags@ computed by package initialization.
data PackageDynFlags = PackageDynFlags
    { pdfPkgDatabase :: !(Maybe [(FilePath, [Packages.PackageConfig])])
    , pdfPkgState :: !Packages.PackageState
    , pdfThisUnitIdInsts :: !(Maybe [(ModuleName, Module)])
    } deriving (Generic, Show)

instance NFData PackageDynFlags where
  rnf (PackageDynFlags db state insts) = db `seq` state `seq` rnf insts

setPackageDynFlags :: PackageDynFlags -> DynFlags -> DynFlags
setPackageDynFlags PackageDynFlags{..} dflags = dflags
    { pkgDatabase = pdfPkgDatabase
    , pkgState = pdfPkgState
    , thisUnitIdInsts_ = pdfThisUnitIdInsts
    }

getPackageDynFlags :: DynFlags -> PackageDynFlags
getPackageDynFlags DynFlags{..} = PackageDynFlags
    { pdfPkgDatabase = pkgDatabase
    , pdfPkgState = pkgState
    , pdfThisUnitIdInsts = thisUnitIdInsts_
    }


-- | A version of `showSDoc` that uses default flags (to avoid uses of
-- `showSDocUnsafe`).
showSDocDefault :: SDoc -> String
showSDocDefault = showSDoc dynFlags
  where dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

prettyPrint :: Outputable a => a -> String
prettyPrint = showSDocDefault . ppr

-- FIXME(#1203): This must move out of `haskell-ide-core` and into `damlc`.
internalModules :: [String]
internalModules =
  [ "Data.String"
  , "GHC.CString"
  , "GHC.Integer.Type"
  , "GHC.Natural"
  , "GHC.Real"
  , "GHC.Types"
  ]

-- | Checks if a given module is internal, i.e. gets removed in the Core->LF
-- translation. TODO where should this live?
modIsInternal :: Module -> Bool
modIsInternal m = moduleNameString (moduleName m) `elem` internalModules
  -- TODO should we consider DA.Internal.* internal? Difference to GHC.*
  -- modules is that these do not disappear in the LF conversion.

-- | This import was generated, not user written, so should not produce unused import warnings
importGenerated :: Bool -> ImportDecl phase -> ImportDecl phase
importGenerated qual i = i{ideclImplicit=True, ideclQualified=qual}

mkImport :: Located ModuleName -> ImportDecl GhcPs
mkImport mname = GHC.ImportDecl GHC.NoExt GHC.NoSourceText mname Nothing False False False False Nothing Nothing

-- FIXME(#1203): This needs to move out of haskell-ide-core.
removeTypeableInfo :: ModGuts -> ModGuts
removeTypeableInfo guts =
  guts{mg_binds = filter (not . isTypeableInfo) (mg_binds guts)}
  where
    isTypeableInfo = \case
      NonRec name _ -> any (`isPrefixOf` getOccString name) ["$krep", "$tc", "$trModule"]
      Rec _ -> False

-- | Like 'runGhc' but much faster (400x), with less IO and no file dependency
runGhcFast :: Ghc a -> IO a
-- copied from GHC with the nasty bits dropped
runGhcFast act = do
  ref <- newIORef (error "empty session")
  let session = Session ref
  flip unGhc session $ do
    dflags <- liftIO $ initDynFlags $ defaultDynFlags fakeSettings fakeLlvmConfig
    liftIO $ setUnsafeGlobalDynFlags dflags
    env <- liftIO $ newHscEnv dflags
    setSession env
    withCleanupSession act

-- These settings are mostly undefined, but define just enough for what we want to do (which isn't code gen)
fakeSettings :: Settings
fakeSettings = Settings
  {sTargetPlatform=platform
  ,sPlatformConstants=platformConstants
  ,sProjectVersion=cProjectVersion
  ,sProgramName="ghc"
  ,sOpt_P_fingerprint=fingerprint0
  }
  where
    platform = Platform{platformWordSize=8, platformOS=OSUnknown, platformUnregisterised=True}
    platformConstants = PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])


setThisInstalledUnitId :: UnitId -> DynFlags -> DynFlags
setThisInstalledUnitId unitId dflags =
  dflags {thisInstalledUnitId = toInstalledUnitId unitId}

setImports :: [FilePath] -> DynFlags -> DynFlags
setImports paths dflags = dflags { importPaths = paths }



-- Orphan instances for types from the GHC API.
instance Show CoreModule where show = prettyPrint
instance NFData CoreModule where rnf !_ = ()

instance Show RdrName where show = prettyPrint
instance NFData RdrName where rnf !_ = ()

instance Show InstalledUnitId where
  show = installedUnitIdString

instance NFData InstalledUnitId where
  rnf = rwhnf

instance NFData SB.StringBuffer where
    rnf = rwhnf

instance Show Module where
  show = moduleNameString . moduleName

instance Show ComponentId where show = prettyPrint
instance Show SourcePackageId where show = prettyPrint
instance Show ModuleName where show = prettyPrint
instance Show (GenLocated SrcSpan ModuleName) where show = prettyPrint
instance Show PackageName where show = prettyPrint
instance Show Packages.PackageState where show _ = "PackageState"
instance Show Name where show = prettyPrint
