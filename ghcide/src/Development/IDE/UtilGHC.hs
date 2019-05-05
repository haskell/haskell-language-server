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
module Development.IDE.UtilGHC(module Development.IDE.UtilGHC) where

import           Config
import qualified CmdLineParser as Cmd (warnMsg)
import           DynFlags (parseDynamicFilePragma)
import           Fingerprint
import           GHC                         hiding (convertLit)
import           GHC.LanguageExtensions.Type
import           GhcMonad
import           GhcPlugins                  as GHC hiding (PackageState, fst3, (<>))
import           HscMain
import qualified Packages
import           Panic (throwGhcExceptionIO)
import           Platform
import qualified StringBuffer                as SB
import qualified EnumSet

import           Control.DeepSeq
import           Control.Monad
import           Data.IORef
import           Data.List
import qualified Data.Text as T
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- GHC setup

-- | Language options enabled in the DAML-1.2 compilation
xExtensionsSet :: [Extension]
xExtensionsSet =
  [ -- syntactic convenience
    RecordPuns, RecordWildCards, LambdaCase, TupleSections, BlockArguments, ViewPatterns,
    NumericUnderscores
    -- records
  , DuplicateRecordFields, DisambiguateRecordFields
    -- types and kinds
  , ScopedTypeVariables, ExplicitForAll
  , DataKinds, KindSignatures, RankNTypes, TypeApplications
  , ConstraintKinds
    -- type classes
  , MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, TypeSynonymInstances
  , DefaultSignatures, StandaloneDeriving, FunctionalDependencies, DeriveFunctor
    -- replacing primitives
  , RebindableSyntax, OverloadedStrings
    -- strictness
  , Strict, StrictData
    -- avoiding letrec in list comp (see DEL-3841)
  , MonadComprehensions
    -- package imports
  , PackageImports
    -- our changes
  , NewColonConvention
  , DamlVersionRequired
  , WithRecordSyntax
  , DamlTemplate
  ]


-- | Language settings _disabled_ ($-XNo...$) in the DAML-1.2 compilation
xExtensionsUnset :: [Extension]
xExtensionsUnset = [  ]

-- | Flags set for DAML-1.2 compilation
xFlagsSet :: [ GeneralFlag ]
xFlagsSet = [
   Opt_Haddock
 , Opt_Ticky
 ]

-- | Warning options set for DAML compilation. Note that these can be modified
--   (per file) by the user via file headers '{-# OPTIONS -fwarn-... #-} and
--   '{-# OPTIONS -no-warn-... #-}'.
wOptsSet :: [ WarningFlag ]
wOptsSet =
  [ Opt_WarnUnusedImports
  , Opt_WarnPrepositiveQualifiedModule
  , Opt_WarnOverlappingPatterns
  , Opt_WarnIncompletePatterns
  ]

-- | Warning options set for DAML compilation, which become errors.
wOptsSetFatal :: [ WarningFlag ]
wOptsSetFatal =
  [ Opt_WarnMissingFields
  ]

-- | Warning options unset for DAML compilation. Note that these can be modified
--   (per file) by the user via file headers '{-# OPTIONS -fwarn-... #-} and
--   '{-# OPTIONS -no-warn-... #-}'.
wOptsUnset :: [ WarningFlag ]
wOptsUnset =
  [ Opt_WarnMissingMonadFailInstances -- failable pattern plus RebindableSyntax raises this error
  , Opt_WarnOverflowedLiterals -- this does not play well with -ticky and the error message is misleading
  ]


adjustDynFlags :: [FilePath] -> PackageState -> Maybe String -> DynFlags -> DynFlags
adjustDynFlags paths packageState mbPackageName dflags
  = setImports paths
  $ setPackageState packageState
  $ setThisInstalledUnitId (maybe mainUnitId stringToUnitId mbPackageName)
  -- once we have package imports working, we want to import the base package and set this to
  -- the default instead of always compiling in the context of ghc-prim.
  $ apply wopt_set wOptsSet
  $ apply wopt_unset wOptsUnset
  $ apply wopt_set_fatal wOptsSetFatal
  $ apply xopt_set xExtensionsSet
  $ apply xopt_unset xExtensionsUnset
  $ apply gopt_set xFlagsSet
  dflags{
    mainModIs = mkModule primUnitId (mkModuleName "NotAnExistingName"), -- avoid DEL-6770
    debugLevel = 1,
    ghcLink = NoLink, hscTarget = HscNothing -- avoid generating .o or .hi files
    {-, dumpFlags = Opt_D_ppr_debug `EnumSet.insert` dumpFlags dflags -- turn on debug output from GHC-}
  }
  where apply f xs d = foldl' f d xs

setThisInstalledUnitId :: UnitId -> DynFlags -> DynFlags
setThisInstalledUnitId unitId dflags =
  dflags {thisInstalledUnitId = toInstalledUnitId unitId}

setImports :: [FilePath] -> DynFlags -> DynFlags
setImports paths dflags = dflags { importPaths = paths }

setPackageState :: PackageState -> DynFlags -> DynFlags
setPackageState state dflags =
  dflags
    { pkgDatabase = pkgStateDb state
    , pkgState = pkgStateState state
    , thisUnitIdInsts_ = pkgThisUnitIdInsts state
    }

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

setPackageImports :: Bool -> [(String, [(String, String)])] -> DynFlags -> DynFlags
setPackageImports hideAllPkgs pkgImports dflags = dflags {
    packageFlags = packageFlags dflags ++
        [ExposePackage pkgName (UnitIdArg $ stringToUnitId pkgName)
          (ModRenaming False [(mkModuleName mod, mkModuleName alias) | (mod, alias) <- aliases])
        | (pkgName, aliases) <- pkgImports
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

-- | This is the subset of `DynFlags` that is computed by package initialization.
data PackageState = PackageState
  { pkgStateDb :: !(Maybe [(FilePath, [Packages.PackageConfig])])
  , pkgStateState :: !Packages.PackageState
  , pkgThisUnitIdInsts :: !(Maybe [(ModuleName, Module)])
  } deriving (Generic, Show)

instance NFData PackageState where
  rnf (PackageState db state insts) = db `seq` state `seq` rnf insts

-- | Configures the @DynFlags@ for this session to DAML-1.2
--  compilation:
--     * Installs a custom log action;
--     * Sets up the package databases;
--     * Sets the import paths to the given list of 'FilePath'.
--     * if present, parses and applies custom options for GHC
--       (may fail if the custom options are inconsistent with std DAML ones)
setupDamlGHC :: GhcMonad m => [FilePath] -> Maybe String -> PackageState -> [String] -> m ()
setupDamlGHC importPaths mbPackageName packageState [] =
  modifyDynFlags $ adjustDynFlags importPaths packageState mbPackageName
-- if custom options are given, add them after the standard DAML flag setup
setupDamlGHC importPaths mbPackageName packageState customOpts = do
  setupDamlGHC importPaths mbPackageName packageState []
  damlDFlags <- getSessionDynFlags
  (dflags', leftover, warns) <- parseDynamicFilePragma damlDFlags $ map noLoc customOpts

  let leftoverError = CmdLineError $
        (unlines . ("Unable to parse custom flags:":) . map unLoc) leftover
  unless (null leftover) $ liftIO $ throwGhcExceptionIO leftoverError

  unless (null warns) $
    liftIO $ putStrLn $ unlines $ "Warnings:" : map (unLoc . Cmd.warnMsg) warns

  modifySession $ \h ->
    h { hsc_dflags = dflags', hsc_IC = (hsc_IC h) {ic_dflags = dflags' } }

-- | A version of `showSDoc` that uses default flags (to avoid uses of
-- `showSDocUnsafe`).
showSDocDefault :: SDoc -> String
showSDocDefault = showSDoc dynFlags
  where dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

prettyPrint :: Outputable a => a -> String
prettyPrint = showSDocDefault . ppr

textToStringBuffer :: T.Text -> SB.StringBuffer
-- would be nice to do this more efficiently...
textToStringBuffer = SB.stringToStringBuffer . T.unpack

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

type RealLocated = GenLocated RealSrcSpan
