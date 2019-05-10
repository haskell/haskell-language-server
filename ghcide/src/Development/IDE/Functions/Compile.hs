-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

-- | Based on https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API.
--   Given a list of paths to find libraries, and a file to compile, produce a list of 'CoreModule' values.
module Development.IDE.Functions.Compile
  ( GhcModule(..)
  , TcModuleResult(..)
  , LoadPackageResult(..)
  , getGhcDynFlags
  , compileModule
  , getSrcSpanInfos
  , parseModule
  , typecheckModule
  , loadPackage
  , computePackageDeps
  , generatePackageState
  ) where

import           Development.IDE.Functions.Warnings
import           Development.IDE.Types.Diagnostics
import qualified Development.IDE.Functions.FindImports as FindImports
import           Development.IDE.Functions.GHCError
import           Development.IDE.Functions.SpanInfo
import Development.IDE.UtilGHC
import Development.IDE.Types.Options

import HieBin
import HieAst

import           GHC hiding (parseModule, typecheckModule)
import qualified Parser
import           Lexer
import           Bag

import qualified GHC
import           Panic
import           GhcMonad
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified HeaderInfo                     as Hdr
import           MkIface
import           NameCache
import           StringBuffer                   as SB
import           TidyPgm
import           InstEnv
import           FamInstEnv

import Control.DeepSeq
import           Control.Exception                        as E
import           Control.Monad
import qualified Control.Monad.Trans.Except               as Ex
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import           Data.Tuple.Extra
import qualified Data.Map.Strict                          as Map
import           Data.Time
import           Development.IDE.Types.SpanInfo
import GHC.Generics (Generic)
import           System.FilePath
import           System.Directory

-- | 'CoreModule' together with some additional information required for the
-- conversion to DAML-LF.
data GhcModule = GhcModule
  { gmPath :: Maybe FilePath
  , gmCore :: CoreModule
  }
  deriving (Generic, Show)

instance NFData GhcModule

-- | Contains the typechecked module and the OrigNameCache entry for
-- that module.
data TcModuleResult = TcModuleResult
    { tmrModule     :: TypecheckedModule
    , tmrModInfo    :: HomeModInfo
    , tmrOccEnvName :: OccEnv Name
    }

-- | Contains the result of loading an interface. In particular the delta to the name cache.
data LoadPackageResult = LoadPackageResult
    { lprInstalledUnitId :: InstalledUnitId
    , lprModuleEnv :: ModuleEnv (OccEnv Name)
    , lprEps :: ExternalPackageState
    }

-- | Get source span info, used for e.g. AtPoint and Goto Definition.
getSrcSpanInfos
    :: IdeOptions
    -> ParsedModule
    -> PackageDynFlags
    -> [(Located ModuleName, Maybe FilePath)]
    -> TcModuleResult
    -> IO [SpanInfo]
getSrcSpanInfos opt mod packageState imports tc =
    runGhcSession opt (Just mod) packageState
        . getSpanInfo imports
        $ tmrModule tc


-- | Given a string buffer, return a pre-processed @ParsedModule@.
parseModule
    :: IdeOptions
    -> PackageDynFlags
    -> FilePath
    -> (UTCTime, SB.StringBuffer)
    -> IO ([FileDiagnostic], Maybe ParsedModule)
parseModule opt@IdeOptions{..} packageState file =
    fmap (either (, Nothing) (second Just)) . Ex.runExceptT .
    -- We need packages since imports fail to resolve otherwise.
    runGhcSessionExcept opt Nothing packageState . parseFileContents optPreprocessor file

computePackageDeps ::
     IdeOptions -> PackageDynFlags -> InstalledUnitId -> IO (Either [FileDiagnostic] [InstalledUnitId])
computePackageDeps opts packageState iuid =
  Ex.runExceptT $
  runGhcSessionExcept opts Nothing packageState $
  catchSrcErrors $ do
    dflags <- hsc_dflags <$> getSession
    liftIO $ depends <$> getPackage dflags iuid

getPackage :: DynFlags -> InstalledUnitId -> IO PackageConfig
getPackage dflags p =
  case lookupInstalledPackage dflags p of
    Nothing -> E.throwIO $ CmdLineError (missingPackageMsg p)
    Just pkg -> return pkg
  where
    missingPackageMsg p = showSDoc dflags $ text "unknown package:" <+> ppr p

-- | Typecheck a single module using the supplied dependencies and packages.
typecheckModule
    :: IdeOptions
    -> ParsedModule
    -> PackageDynFlags
    -> UniqSupply
    -> [TcModuleResult]
    -> [LoadPackageResult]
    -> ParsedModule
    -> IO ([FileDiagnostic], Maybe TcModuleResult)
typecheckModule opt mod packageState uniqSupply deps pkgs pm =
    fmap (either (, Nothing) (second Just)) $ Ex.runExceptT $
    runGhcSessionExcept opt (Just mod) packageState $
        catchSrcErrors $ do
            setupEnv uniqSupply deps pkgs
            (warnings, tcm) <- withWarnings "Typechecker" $ \tweak ->
                GHC.typecheckModule pm{pm_mod_summary = tweak $ pm_mod_summary pm}
            tcm2 <- mkTcModuleResult (WriteInterface $ optWriteIface opt) tcm
            return (warnings, tcm2)

-- | Load a pkg and populate the name cache and external package state.
loadPackage ::
     IdeOptions
  -> PackageDynFlags
  -> UniqSupply
  -> [LoadPackageResult]
  -> InstalledUnitId
  -> IO (Either [FileDiagnostic] LoadPackageResult)
loadPackage opt packageState us lps p =
  Ex.runExceptT $
  runGhcSessionExcept opt Nothing packageState $
  catchSrcErrors $ do
    setupEnv us [] lps
    dflags <- hsc_dflags <$> getSession
    exposedMods <- liftIO $ exposedModules <$> getPackage dflags p
    let mods =
          [ Module (DefiniteUnitId (DefUnitId p)) mod
          | (mod, _mbParent) <- exposedMods
          ]
    forM_ mods $ \mod -> GHC.getModuleInfo mod
    -- this populates the namecache and external package state
    session <- getSession
    modEnv <- nsNames <$> liftIO (readIORef $ hsc_NC session)
    eps <- liftIO (readIORef $ hsc_EPS session)
    pure $ LoadPackageResult p modEnv eps

-- | Compile a single type-checked module to a 'CoreModule' value, or
-- provide errors.
compileModule
    :: IdeOptions
    -> ParsedModule
    -> PackageDynFlags
    -> UniqSupply
    -> [TcModuleResult]
    -> [LoadPackageResult]
    -> TcModuleResult
    -> IO ([FileDiagnostic], Maybe GhcModule)
compileModule opt mod packageState uniqSupply deps pkgs tmr =
    fmap (either (, Nothing) (second Just)) $ Ex.runExceptT $
    runGhcSessionExcept opt (Just mod) packageState $
        catchSrcErrors $ do
            setupEnv uniqSupply (deps ++ [tmr]) pkgs

            let tm = tmrModule tmr
            session <- getSession
            (warnings,desugar) <- withWarnings "Desugarer" $ \tweak -> do
                let pm = tm_parsed_module tm
                let pm' = pm{pm_mod_summary = tweak $ pm_mod_summary pm}
                let tm' = tm{tm_parsed_module  = pm'}
                GHC.dm_core_module <$> GHC.desugarModule tm'

            -- give variables unique OccNames
            (tidy, details) <- liftIO $ tidyProgram session desugar

            let path  = ml_hs_file $ ms_location $ pm_mod_summary $ tm_parsed_module tm
            let core = CoreModule
                         (cg_module tidy)
                         (md_types details)
                         (cg_binds tidy)
                         (mg_safe_haskell desugar)

            return (warnings, GhcModule path core)

-- | Evaluate a GHC session using a new environment constructed with
-- the supplied options.
runGhcSessionExcept
    :: IdeOptions
    -> Maybe ParsedModule
    -> PackageDynFlags
    -> Ex.ExceptT e Ghc a
    -> Ex.ExceptT e IO a
runGhcSessionExcept opts mbMod pkg m =
    Ex.ExceptT $ runGhcSession opts mbMod pkg $ Ex.runExceptT m


getGhcDynFlags :: IdeOptions -> ParsedModule -> PackageDynFlags -> IO DynFlags
getGhcDynFlags opts mod pkg = runGhcSession opts (Just mod) pkg getSessionDynFlags

-- | Evaluate a GHC session using a new environment constructed with
-- the supplied options.
runGhcSession
    :: IdeOptions
    -> Maybe ParsedModule
    -> PackageDynFlags
    -> Ghc a
    -> IO a
runGhcSession IdeOptions{..} = optRunGhcSession

-- When we make a fresh GHC environment, the OrigNameCache comes already partially
-- populated. So to be safe, we simply extend this one.
mkNameCache :: GhcMonad m => UniqSupply -> [TcModuleResult] -> [LoadPackageResult] -> m NameCache
mkNameCache uniqSupply tms pkgs = do
    session    <- getSession
    onc        <- nsNames <$> liftIO (readIORef $ hsc_NC session)
    return NameCache
            { nsUniqs = uniqSupply
            , nsNames = extendOrigNameCache' onc tms pkgs
            }

-- | Extend the name cache with the names from the typechecked home modules and the loaded packages.
-- If we have two environments containing the same module we take the later one. We do this because
-- the name cache comes prepopulated with modules from daml-prim and we overwrite those with our own
-- daml-prim package.
extendOrigNameCache' :: OrigNameCache -> [TcModuleResult] -> [LoadPackageResult] -> OrigNameCache
extendOrigNameCache' onc tms pkgs = foldl (plusModuleEnv_C (\_x y -> y)) onc modEnvs
  where
    modEnvs =
      mkModuleEnv
         [(ms_mod $ tcModSummary $ tmrModule tm, tmrOccEnvName tm) | tm <- tms] :
      [lprModuleEnv lm | lm <- pkgs]

newtype WriteInterface = WriteInterface Bool

mkTcModuleResult
    :: GhcMonad m
    => WriteInterface
    -> TypecheckedModule
    -> m TcModuleResult
mkTcModuleResult (WriteInterface writeIface) tcm = do
    session   <- getSession
    nc        <- liftIO $ readIORef (hsc_NC session)
    (iface,_) <- liftIO $ mkIfaceTc session Nothing Sf_None details tcGblEnv
    liftIO $ when writeIface $ do
        let path = ".interfaces" </> file tcm
        createDirectoryIfMissing True (takeDirectory path)
        writeIfaceFile (hsc_dflags session) (replaceExtension path ".hi") iface
        -- For now, we write .hie files whenever we write .hi files which roughly corresponds to
        -- when we are building a package. It should be easily decoupable if that turns out to be
        -- useful.
        hieFile <- runHsc session $ mkHieFile (tcModSummary tcm) tcGblEnv (fromJust $ renamedSource tcm)
        writeHieFile (replaceExtension path ".hie") hieFile
    let mod_info = HomeModInfo iface details Nothing
        origNc = nsNames nc
    case lookupModuleEnv origNc (tcmModule tcm) of
        Nothing  -> panic err
        Just occ -> return $ TcModuleResult tcm mod_info occ
  where
    file = ms_hspp_file . tcModSummary
    tcmModule = ms_mod . tcModSummary
    (tcGblEnv, details) = tm_internals_ tcm
    err = "Internal error : module not found in NameCache :" <>
              moduleNameString (moduleName $ tcmModule tcm)

tcModSummary :: TypecheckedModule -> ModSummary
tcModSummary = pm_mod_summary . tm_parsed_module

-- | Setup the environment that GHC needs according to our
-- best understanding (!)
setupEnv :: GhcMonad m => UniqSupply -> [TcModuleResult] -> [LoadPackageResult] -> m ()
setupEnv uniqSupply tms lps = do
    session <- getSession

    let mss = map (pm_mod_summary . tm_parsed_module . tmrModule) tms

    -- set the target and module graph in the session
    let graph = mkModuleGraph mss
    setSession session { hsc_mod_graph = graph }

    -- Make modules available for others that import them,
    -- by putting them in the finder cache.
    let ims  = map (InstalledModule (thisInstalledUnitId $ hsc_dflags session) . moduleName . ms_mod) mss
        ifrs = zipWith (\ms -> InstalledFound (ms_location ms)) mss ims
    liftIO $ modifyIORef (hsc_FC session) $ \fc ->
        foldl' (\fc (im, ifr) -> GHC.extendInstalledModuleEnv fc im ifr) fc
            $ zip ims ifrs

    -- construct a new NameCache
    nc' <- mkNameCache uniqSupply tms lps
    -- update the name cache
    liftIO $ modifyIORef (hsc_NC session) $ const nc'
    -- update the external package state
    liftIO $ modifyIORef (hsc_EPS session) (updateEps lps)
    -- load dependent modules, which must be in topological order.
    mapM_ loadModuleHome tms

-- | Update the external package state given the loaded package results.
updateEps :: [LoadPackageResult] -> ExternalPackageState -> ExternalPackageState
updateEps lps eps =
  eps
    { eps_inst_env = newInstEnv
    , eps_PIT = newPIT
    , eps_PTE = newPTE
    , eps_rule_base = newRuleBase
    , eps_complete_matches = newCompleteMatches
    , eps_fam_inst_env = newFamInst
    , eps_ann_env = newAnnEnv
    , eps_mod_fam_inst_env = newModFamInstEnv
    }
  where
    (newInstEnv, (newPIT, (newPTE, (newRuleBase, (newCompleteMatches, (newFamInst, (newAnnEnv, newModFamInstEnv))))))) =
      foldl
        (\(instEnv, (pit, (pte, (ruleBase, (completeMatches, (famInst, (annEnv, modFamInstEnv))))))) ->
           (instEnv `extendInstEnvList0`) ***
           (pit `plusModuleEnv`) ***
           (pte `plusTypeEnv`) ***
           (ruleBase `unionRuleBase`) ***
           (completeMatches `extendCompleteMatchMap`) ***
           (famInst `extendFamInstEnvList`) ***
           (annEnv `plusAnnEnv`) *** (modFamInstEnv `plusModuleEnv`))
        ( emptyInstEnv
        , ( emptyPackageIfaceTable
          , ( emptyTypeEnv
            , ( emptyRuleBase
              , (emptyUFM, (emptyFamInstEnv, (emptyAnnEnv, emptyModuleEnv)))))))
        [ ( instEnvElts $ eps_inst_env e
          , ( eps_PIT e
            , ( eps_PTE e
              , ( eps_rule_base e
                , ( concat $ eltsUFM $ eps_complete_matches e
                  , ( famInstEnvElts $ eps_fam_inst_env e
                    , (eps_ann_env e, eps_mod_fam_inst_env e)))))))
        | p <- lps
        , let e = lprEps p
        ]

    -- TODO (drsk): This is necessary because the EPS that we store include the results of
    -- previously loaded packages and we end up adding instances several times to the environment.
    -- It would be better to have pure delta stored in the LoadPackageResult, such that it
    -- contains only identities/instances/names coming from that specific loaded package, but I
    -- failed so far in computing the correct delta.
    extendInstEnvList0 instEnv0 clsInsts =
      extendInstEnvList emptyInstEnv $
      nubOrdOn is_dfun_name $ instEnvElts instEnv0 ++ clsInsts

-- | Load a module, quickly. Input doesn't need to be desugared.
-- A module must be loaded before dependent modules can be typechecked.
-- This variant of loadModuleHome will *never* cause recompilation, it just
-- modifies the session.
loadModuleHome
    :: (GhcMonad m)
    => TcModuleResult
    -> m ()
loadModuleHome tmr = modifySession $ \e ->
    e { hsc_HPT = addToHpt (hsc_HPT e) mod mod_info }
  where
    ms       = pm_mod_summary . tm_parsed_module . tmrModule $ tmr
    mod_info = tmrModInfo tmr
    mod      = ms_mod_name ms

-- | Produce a module summary from a StringBuffer.
getModSummaryFromBuffer
    :: GhcMonad m
    => FilePath
    -> (SB.StringBuffer, UTCTime)
    -> DynFlags
    -> GHC.ParsedSource
    -> Ex.ExceptT [FileDiagnostic] m ModSummary
getModSummaryFromBuffer fp (contents, fileDate) dflags parsed = do
  (modName, imports) <- FindImports.getImportsParsed dflags parsed

  let modLoc = ModLocation
          { ml_hs_file  = Just fp
          , ml_hi_file  = replaceExtension fp "hi"
          , ml_obj_file = replaceExtension fp "o"
#ifndef USE_GHC
          , ml_hie_file = replaceExtension fp "hie"
#endif
          -- This does not consider the dflags configuration
          -- (-osuf and -hisuf, object and hi dir.s).
          -- However, we anyway don't want to generate them.
          }
      InstalledUnitId unitId = thisInstalledUnitId dflags
  return $ ModSummary
    { ms_mod          = mkModule (fsToUnitId unitId) modName
    , ms_location     = modLoc
    , ms_hs_date      = fileDate
    , ms_textual_imps = imports
    , ms_hspp_file    = fp
    , ms_hspp_opts    = dflags
    , ms_hspp_buf     = Just contents

    -- defaults:
    , ms_hsc_src      = HsSrcFile
    , ms_obj_date     = Nothing
    , ms_iface_date   = Nothing
#ifndef USE_GHC
    , ms_hie_date     = Nothing
#endif
    , ms_srcimps      = []        -- source imports are not allowed
    , ms_parsed_mod   = Nothing
    }

-- | Given a buffer, flags, file path and module summary, produce a
-- parsed module (or errors) and any parse warnings.
parseFileContents
       :: GhcMonad m
       => (GHC.ParsedSource -> ([(GHC.SrcSpan, String)], GHC.ParsedSource))
       -> FilePath  -- ^ the filename (for source locations)
       -> (UTCTime, SB.StringBuffer) -- ^ Haskell module source text (full Unicode is supported)
       -> Ex.ExceptT [FileDiagnostic] m ([FileDiagnostic], ParsedModule)
parseFileContents preprocessor filename (time, contents) = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   dflags  <- parsePragmasIntoDynFlags filename contents
   case unP Parser.parseModule (mkPState dflags contents loc) of
#ifdef USE_GHC
     PFailed _ logMsg msgErr ->
      Ex.throwE $ mkErrorDoc dflags locErr msgErr
#else
     PFailed s ->
       -- A fatal parse error was encountered.
       Ex.throwE $ toDiagnostics dflags $ snd $ getMessages s dflags
#endif
     POk pst rdr_module ->
         let hpm_annotations =
               (Map.fromListWith (++) $ annotations pst,
                 Map.fromList ((noSrcSpan,comment_q pst)
                                  :annotations_comments pst))
             (warns, errs) = getMessages pst dflags
         in
           do
               -- Just because we got a `POk`, it doesn't mean there
               -- weren't errors! To clarify, the GHC parser
               -- distinguishes between fatal and non-fatal
               -- errors. Non-fatal errors are the sort that don't
               -- prevent parsing from continuing (that is, a parse
               -- tree can still be produced despite the error so that
               -- further errors/warnings can be collected). Fatal
               -- errors are those from which a parse tree just can't
               -- be produced.
               unless (null errs) $
                 Ex.throwE $ toDiagnostics dflags $ snd $ getMessages pst dflags

               -- Ok, we got here. It's safe to continue.
               let (errs, parsed) = preprocessor rdr_module
               unless (null errs) $ Ex.throwE $ mkErrors dflags errs
               ms <- getModSummaryFromBuffer filename (contents, time) dflags parsed
               let pm =
                     ParsedModule {
                         pm_mod_summary = ms
                       , pm_parsed_source = parsed
                       , pm_extra_src_files=[] -- src imports not allowed
                       , pm_annotations = hpm_annotations
                      }
                   warnings = mapMaybe (mkDiag dflags "Parser") $ bagToList warns
               pure (warnings, pm)


-- | This reads the pragma information directly from the provided buffer.
parsePragmasIntoDynFlags
    :: GhcMonad m
    => FilePath
    -> SB.StringBuffer
    -> Ex.ExceptT [FileDiagnostic] m DynFlags
parsePragmasIntoDynFlags fp contents = catchSrcErrors $ do
    dflags0  <- getSessionDynFlags
    let opts = Hdr.getOptions dflags0 contents fp
    (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
    return dflags

generatePackageState :: [FilePath] -> Bool -> [(String, ModRenaming)] -> IO PackageDynFlags
generatePackageState paths hideAllPkgs pkgImports = do
  let dflags = setPackageImports hideAllPkgs pkgImports $ setPackageDbs paths fakeDynFlags
  (newDynFlags, _) <- initPackages dflags
  pure $ getPackageDynFlags newDynFlags

-- | Run something in a Ghc monad and catch the errors (SourceErrors and
-- compiler-internal exceptions like Panic or InstallationError).
catchSrcErrors :: GhcMonad m => m a -> Ex.ExceptT [FileDiagnostic] m a
catchSrcErrors ghcM = do
      dflags <- getDynFlags
      Ex.ExceptT $
        handleGhcException (ghcExceptionToDiagnostics dflags) $
        handleSourceError (sourceErrorToDiagnostics dflags) $
        Right <$> ghcM
    where
        ghcExceptionToDiagnostics dflags = return . Left . mkErrorsGhcException dflags
        sourceErrorToDiagnostics dflags = return . Left . toDiagnostics dflags . srcErrorMessages
