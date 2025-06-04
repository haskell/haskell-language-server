-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

-- | Based on https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API.
--   Given a list of paths to find libraries, and a file to compile, produce a list of 'CoreModule' values.
module Development.IDE.Core.Compile
  ( TcModuleResult(..)
  , RunSimplifier(..)
  , compileModule
  , parseModule
  , typecheckModule
  , computePackageDeps
  , addRelativeImport
  , mkHiFileResultCompile
  , mkHiFileResultNoCompile
  , generateObjectCode
  , generateByteCode
  , generateHieAsts
  , writeAndIndexHieFile
  , indexHieFile
  , writeHiFile
  , getModSummaryFromImports
  , loadHieFile
  , loadInterface
  , RecompilationInfo(..)
  , loadModulesHome
  , getDocsBatch
  , lookupName
  , mergeEnvs
  , ml_core_file
  , coreFileToLinkable
  , TypecheckHelpers(..)
  , sourceTypecheck
  , sourceParser
  , shareUsages
  , setNonHomeFCHook
  ) where

import           Control.Concurrent.STM.Stats                 hiding (orElse)
import           Control.DeepSeq                              (NFData (..),
                                                               force, rnf)
import           Control.Exception                            (evaluate)
import           Control.Exception.Safe
import           Control.Lens                                 hiding (List, pre,
                                                               (<.>))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State.Strict             as S
import           Data.Aeson                                   (toJSON)
import           Data.Bifunctor                               (first, second)
import           Data.Binary
import qualified Data.ByteString                              as BS
import           Data.Coerce
import qualified Data.DList                                   as DL
import           Data.Functor
import           Data.Generics.Aliases
import           Data.Generics.Schemes
import qualified Data.HashMap.Strict                          as HashMap
import           Data.IntMap                                  (IntMap)
import           Data.IORef
import           Data.List.Extra
import qualified Data.Map.Strict                              as Map
import           Data.Maybe
import           Data.Proxy                                   (Proxy (Proxy))
import qualified Data.Text                                    as T
import           Data.Time                                    (UTCTime (..))
import           Data.Tuple.Extra                             (dupe)
import           Debug.Trace
import           Development.IDE.Core.FileStore               (resetInterfaceStore)
import           Development.IDE.Core.Preprocessor
import           Development.IDE.Core.ProgressReporting       (progressUpdate)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing                 (withTrace)
import qualified Development.IDE.GHC.Compat                   as Compat
import qualified Development.IDE.GHC.Compat                   as GHC
import           Development.IDE.GHC.Compat.Driver            (hscTypecheckRenameWithDiagnostics)
import qualified Development.IDE.GHC.Compat.Util              as Util
import           Development.IDE.GHC.CoreFile
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Orphans                  ()
import           Development.IDE.GHC.Util
import           Development.IDE.GHC.Warnings
import           Development.IDE.Import.DependencyInformation
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           GHC                                          (ForeignHValue,
                                                               GetDocsFailure (..),
                                                               ModLocation (..),
                                                               parsedSource)
import qualified GHC.LanguageExtensions                       as LangExt
import           GHC.Serialized
import           HieDb                                        hiding (withHieDb)
import qualified Language.LSP.Protocol.Message                as LSP
import           Language.LSP.Protocol.Types                  (DiagnosticTag (..))
import qualified Language.LSP.Server                          as LSP
import           Prelude                                      hiding (mod)
import           System.Directory
import           System.FilePath
import           System.IO.Extra                              (fixIO,
                                                               newTempFileWithin)

import qualified Data.Set                                     as Set
import qualified GHC                                          as G
import           GHC.Core.Lint.Interactive
import           GHC.Driver.Config.CoreToStg.Prep
import qualified GHC.Runtime.Loader                           as Loader
import           GHC.Tc.Gen.Splice
import           GHC.Types.Error
import           GHC.Types.ForeignStubs
import           GHC.Types.HpcInfo
import           GHC.Types.TypeEnv

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if MIN_VERSION_ghc(9,7,0)
import           Data.Foldable                                (toList)
import           GHC.Unit.Module.Warnings
#else
import           Development.IDE.Core.FileStore               (shareFilePath)
#endif

#if MIN_VERSION_ghc(9,10,0)
import           Development.IDE.GHC.Compat                   hiding (assert,
                                                               loadInterface,
                                                               parseHeader,
                                                               parseModule,
                                                               tcRnModule,
                                                               writeHieFile)
#else
import           Development.IDE.GHC.Compat                   hiding
                                                              (loadInterface,
                                                               parseHeader,
                                                               parseModule,
                                                               tcRnModule,
                                                               writeHieFile)
#endif

#if MIN_VERSION_ghc(9,11,0)
import qualified Data.List.NonEmpty                           as NE
import           Data.Time                                    (getCurrentTime)
import           GHC.Driver.Env                               (hsc_all_home_unit_ids)
#endif

#if MIN_VERSION_ghc(9,12,0)
import           Development.IDE.Import.FindImports
#endif

--Simple constants to make sure the source is consistently named
sourceTypecheck :: T.Text
sourceTypecheck = "typecheck"
sourceParser :: T.Text
sourceParser = "parser"

-- | Given a string buffer, return the string (after preprocessing) and the 'ParsedModule'.
parseModule
    :: IdeOptions
    -> HscEnv
    -> FilePath
    -> ModSummary
    -> IO (IdeResult ParsedModule)
parseModule IdeOptions{..} env filename ms =
    fmap (either (, Nothing) id) $
    runExceptT $ do
        (diag, modu) <- parseFileContents env optPreprocessor filename ms
        return (diag, Just modu)


-- | Given a package identifier, what packages does it depend on
computePackageDeps
    :: HscEnv
    -> Unit
    -> IO (Either [FileDiagnostic] [UnitId])
computePackageDeps env pkg = do
    case lookupUnit env pkg of
        Nothing ->
          return $ Left
            [ ideErrorText
                (toNormalizedFilePath' noFilePath)
                (T.pack $ "unknown package: " ++ show pkg)
            ]
        Just pkgInfo -> return $ Right $ unitDepends pkgInfo

data TypecheckHelpers
  = TypecheckHelpers
  { getLinkables   :: [NormalizedFilePath] -> IO [LinkableResult] -- ^ hls-graph action to get linkables for files
  , getModuleGraph :: IO DependencyInformation
  }

typecheckModule :: IdeDefer
                -> HscEnv
                -> TypecheckHelpers
                -> ParsedModule
                -> IO (IdeResult TcModuleResult)
typecheckModule (IdeDefer defer) hsc tc_helpers pm = do
        let modSummary = pm_mod_summary pm
            dflags = ms_hspp_opts modSummary
        initialized <- catchSrcErrors (hsc_dflags hsc) "typecheck (initialize plugins)"
                                      (Loader.initializePlugins (hscSetFlags (ms_hspp_opts modSummary) hsc))
        case initialized of
          Left errs -> return (errs, Nothing)
          Right hscEnv -> do
            etcm <-
                let
                   -- TODO: maybe setting ms_hspp_opts is unnecessary?
                  mod_summary' = modSummary { ms_hspp_opts = hsc_dflags hscEnv}
                in
                  catchSrcErrors (hsc_dflags hscEnv) sourceTypecheck $ do
                    tcRnModule hscEnv tc_helpers $ demoteIfDefer pm{pm_mod_summary = mod_summary'}
            case etcm of
              Left errs -> return (errs, Nothing)
              Right tcm ->
                let addReason diag =
                      map (Just (diagnosticReason (errMsgDiagnostic diag)),) $
                        diagFromErrMsg sourceTypecheck (hsc_dflags hscEnv) diag
                    errorPipeline = map (unDefer . hideDiag dflags . tagDiag) . addReason
                    diags = concatMap errorPipeline $ Compat.getMessages $ tmrWarnings tcm
                    deferredError = any fst diags
                in
                return (map snd diags, Just $ tcm{tmrDeferredError = deferredError})
    where
        demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id

-- | Install hooks to capture the splices as well as the runtime module dependencies
captureSplicesAndDeps :: TypecheckHelpers -> HscEnv -> (HscEnv -> IO a) -> IO (a, Splices, ModuleEnv BS.ByteString)
captureSplicesAndDeps TypecheckHelpers{..} env k = do
  splice_ref <- newIORef mempty
  dep_ref <- newIORef emptyModuleEnv
  res <- k (hscSetHooks (addSpliceHook splice_ref . addLinkableDepHook dep_ref $ hsc_hooks env) env)
  splices <- readIORef splice_ref
  needed_mods <- readIORef dep_ref
  return (res, splices, needed_mods)
  where
    addLinkableDepHook :: IORef (ModuleEnv BS.ByteString) -> Hooks -> Hooks
    addLinkableDepHook var h = h { hscCompileCoreExprHook = Just (compile_bco_hook var) }

    -- We want to record exactly which linkables/modules the typechecker needed at runtime
    -- This is useful for recompilation checking.
    -- See Note [Recompilation avoidance in the presence of TH]
    --
    -- From hscCompileCoreExpr' in GHC
    -- To update, copy hscCompileCoreExpr' (the implementation of
    -- hscCompileCoreExprHook) verbatim, and add code to extract all the free
    -- names in the compiled bytecode, recording the modules that those names
    -- come from in the IORef,, as these are the modules on whose implementation
    -- we depend.
    compile_bco_hook :: IORef (ModuleEnv BS.ByteString) -> HscEnv -> SrcSpan -> CoreExpr
                     -> IO (ForeignHValue, [Linkable], PkgsLoaded)
    compile_bco_hook var hsc_env srcspan ds_expr
      = do { let dflags = hsc_dflags hsc_env

             {- Simplify it -}
           ; simpl_expr <- simplifyExpr dflags hsc_env ds_expr

             {- Tidy it (temporary, until coreSat does cloning) -}
           ; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr

             {- Prepare for codegen -}
           ; prepd_expr <- corePrepExpr dflags hsc_env tidy_expr

             {- Lint if necessary -}
           ; lintInteractiveExpr "hscCompileExpr" hsc_env prepd_expr


           ; let iNTERACTIVELoc = G.ModLocation{ ml_hs_file   = Nothing,
                                        ml_hi_file   = panic "hscCompileCoreExpr':ml_hi_file",
                                        ml_obj_file  = panic "hscCompileCoreExpr':ml_obj_file",
                                        ml_dyn_obj_file = panic "hscCompileCoreExpr':ml_dyn_obj_file",
                                        ml_dyn_hi_file  = panic "hscCompileCoreExpr':ml_dyn_hi_file",
                                        ml_hie_file  = panic "hscCompileCoreExpr':ml_hie_file"
                                        }
           ; let ictxt = hsc_IC hsc_env

           ; (binding_id, stg_expr, _, _) <-
               myCoreToStgExpr (hsc_logger hsc_env)
                               (hsc_dflags hsc_env)
                               ictxt
                               True -- for bytecode
                               (icInteractiveModule ictxt)
                               iNTERACTIVELoc
                               prepd_expr

             {- Convert to BCOs -}
           ; bcos <- byteCodeGen hsc_env
                       (icInteractiveModule ictxt)
                       stg_expr
                       [] Nothing
#if MIN_VERSION_ghc(9,11,0)
                       [] -- spt_entries
#endif

            -- Exclude wired-in names because we may not have read
            -- their interface files, so getLinkDeps will fail
            -- All wired-in names are in the base package, which we link
            -- by default, so we can safely ignore them here.

            -- Find the linkables for the modules we need
           ; let needed_mods = mkUniqSet [
                                           mod -- We need the whole module for 9.4 because of multiple home units modules may have different unit ids

                                         | n <- concatMap (uniqDSetToList . bcoFreeNames) $ bc_bcos bcos
                                         , not (isWiredInName n) -- Exclude wired-in names
                                         , Just mod <- [nameModule_maybe n] -- Names from other modules
                                         , moduleUnitId mod `elem` home_unit_ids -- Only care about stuff from the home package set
                                         ]
                 home_unit_ids =
                    map fst (hugElts $ hsc_HUG hsc_env)
                 mods_transitive = getTransitiveMods hsc_env needed_mods

                 -- If we don't support multiple home units, ModuleNames are sufficient because all the units will be the same
                 mods_transitive_list =
                                         mapMaybe nodeKeyToInstalledModule $ Set.toList mods_transitive

           ; moduleLocs <- getModuleGraph
           ; lbs <- getLinkables [file
                                 | installedMod <- mods_transitive_list
                                 , let file = fromJust $ lookupModuleFile (installedMod { moduleUnit = RealUnit (Definite $ moduleUnit installedMod) }) moduleLocs
                                 ]
           ; let hsc_env' = loadModulesHome (map linkableHomeMod lbs) hsc_env

             {- load it -}
#if MIN_VERSION_ghc(9,11,0)
           ; bco_time <- getCurrentTime
           ; (fv_hvs, lbss, pkgs) <- loadDecls (hscInterp hsc_env') hsc_env' srcspan $
                Linkable bco_time (icInteractiveModule ictxt) $ NE.singleton $ BCOs bcos
#else
           ; (fv_hvs, lbss, pkgs) <- loadDecls (hscInterp hsc_env') hsc_env' srcspan bcos
#endif
           ; let hval = (expectJust "hscCompileCoreExpr'" $ lookup (idName binding_id) fv_hvs, lbss, pkgs)

           ; modifyIORef' var (flip extendModuleEnvList [(mi_module $ hm_iface hm, linkableHash lb) | lb <- lbs, let hm = linkableHomeMod lb])
           ; return hval }

    -- TODO: support backpack
    nodeKeyToInstalledModule :: NodeKey -> Maybe InstalledModule
    -- We shouldn't get boot files here, but to be safe, never map them to an installed module
    -- because boot files don't have linkables we can load, and we will fail if we try to look
    -- for them
    nodeKeyToInstalledModule (NodeKey_Module (ModNodeKeyWithUid (GWIB _ IsBoot) _)) = Nothing
    nodeKeyToInstalledModule (NodeKey_Module (ModNodeKeyWithUid (GWIB moduleName _) uid)) = Just $ mkModule uid moduleName
    nodeKeyToInstalledModule _ = Nothing
    moduleToNodeKey :: Module -> NodeKey
    moduleToNodeKey mod = NodeKey_Module $ ModNodeKeyWithUid (GWIB (moduleName mod) NotBoot) (moduleUnitId mod)

    -- Compute the transitive set of linkables required
    getTransitiveMods hsc_env needed_mods
      = Set.unions (Set.fromList (map moduleToNodeKey mods) : [ dep | m <- mods
                                                              , Just dep <- [Map.lookup (moduleToNodeKey m) (mgTransDeps (hsc_mod_graph hsc_env))]
                                                              ])
      where mods = nonDetEltsUniqSet needed_mods -- OK because we put them into a set immediately after

    -- | Add a Hook to the DynFlags which captures and returns the
    -- typechecked splices before they are run. This information
    -- is used for hover.
    addSpliceHook :: IORef Splices -> Hooks -> Hooks
    addSpliceHook var h = h { runMetaHook = Just (splice_hook (runMetaHook h) var) }

    splice_hook :: Maybe (MetaHook TcM) -> IORef Splices -> MetaHook TcM
    splice_hook (fromMaybe defaultRunMeta -> hook) var metaReq e = case metaReq of
        (MetaE f) -> do
            expr' <- metaRequestE hook e
            liftIO $ modifyIORef' var $ exprSplicesL %~ ((e, expr') :)
            pure $ f expr'
        (MetaP f) -> do
            pat' <- metaRequestP hook e
            liftIO $ modifyIORef' var $ patSplicesL %~ ((e, pat') :)
            pure $ f pat'
        (MetaT f) -> do
            type' <- metaRequestT hook e
            liftIO $ modifyIORef' var $ typeSplicesL %~ ((e, type') :)
            pure $ f type'
        (MetaD f) -> do
            decl' <- metaRequestD hook e
            liftIO $ modifyIORef' var $ declSplicesL %~ ((e, decl') :)
            pure $ f decl'
        (MetaAW f) -> do
            aw' <- metaRequestAW hook e
            liftIO $ modifyIORef' var $ awSplicesL %~ ((e, aw') :)
            pure $ f aw'


tcRnModule
  :: HscEnv
  -> TypecheckHelpers -- ^ Program linkables not to unload
  -> ParsedModule
  -> IO TcModuleResult
tcRnModule hsc_env tc_helpers pmod = do
  let ms = pm_mod_summary pmod
      hsc_env_tmp = hscSetFlags (ms_hspp_opts ms) hsc_env

  (((tc_gbl_env', mrn_info), warning_messages), splices, mod_env)
      <- captureSplicesAndDeps tc_helpers hsc_env_tmp $ \hscEnvTmp ->
             do  hscTypecheckRenameWithDiagnostics hscEnvTmp ms $
                          HsParsedModule { hpm_module = parsedSource pmod
                                         , hpm_src_files = pm_extra_src_files pmod
                                         }
  let rn_info = case mrn_info of
        Just x  -> x
        Nothing -> error "no renamed info tcRnModule"

      -- Serialize mod_env so we can read it from the interface
      mod_env_anns = map (\(mod, hash) -> Annotation (ModuleTarget mod) $ toSerialized BS.unpack hash)
                         (moduleEnvToList mod_env)
      tc_gbl_env = tc_gbl_env' { tcg_ann_env = extendAnnEnvList (tcg_ann_env tc_gbl_env') mod_env_anns }
  pure (TcModuleResult pmod rn_info tc_gbl_env splices False mod_env warning_messages)


-- Note [Clearing mi_globals after generating an iface]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- GHC populates the mi_global field in interfaces for GHCi if we are using the bytecode
-- interpreter.
-- However, this field is expensive in terms of heap usage, and we don't use it in HLS
-- anywhere. So we zero it out.
-- The field is not serialized or deserialised from disk, so we don't need to remove it
-- while reading an iface from disk, only if we just generated an iface in memory
--



-- | See https://github.com/haskell/haskell-language-server/issues/3450
-- GHC's recompilation avoidance in the presense of TH is less precise than
-- HLS. To avoid GHC from pessimising HLS, we filter out certain dependency information
-- that we track ourselves. See also Note [Recompilation avoidance in the presence of TH]
filterUsages :: [Usage] -> [Usage]
filterUsages = filter $ \case UsageHomeModuleInterface{} -> False
                              _ -> True

-- | Mitigation for https://gitlab.haskell.org/ghc/ghc/-/issues/22744
-- Important to do this immediately after reading the unit before
-- anything else has a chance to read `mi_usages`
shareUsages :: ModIface -> ModIface
shareUsages iface
  = iface
-- Fixed upstream in GHC 9.8
#if !MIN_VERSION_ghc(9,7,0)
      {mi_usages = usages}
  where usages = map go (mi_usages iface)
        go usg@UsageFile{} = usg {usg_file_path = fp}
          where !fp = shareFilePath (usg_file_path usg)
        go usg = usg
#endif


mkHiFileResultNoCompile :: HscEnv -> TcModuleResult -> IO HiFileResult
mkHiFileResultNoCompile session tcm = do
  let hsc_env_tmp = hscSetFlags (ms_hspp_opts ms) session
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm
  details <- makeSimpleDetails hsc_env_tmp tcGblEnv
  sf <- finalSafeMode (ms_hspp_opts ms) tcGblEnv
  iface' <- mkIfaceTc hsc_env_tmp sf details ms Nothing tcGblEnv
  -- See Note [Clearing mi_globals after generating an iface]
  let iface = iface'
#if MIN_VERSION_ghc(9,11,0)
                & set_mi_top_env Nothing
                & set_mi_usages (filterUsages (mi_usages iface'))
#else
                { mi_globals = Nothing, mi_usages = filterUsages (mi_usages iface') }
#endif
  pure $! mkHiFileResult ms iface details (tmrRuntimeModules tcm) Nothing

mkHiFileResultCompile
    :: ShakeExtras
    -> HscEnv
    -> TcModuleResult
    -> ModGuts
    -> IO (IdeResult HiFileResult)
mkHiFileResultCompile se session' tcm simplified_guts = catchErrs $ do
  let session = hscSetFlags (ms_hspp_opts ms) session'
      ms = pm_mod_summary $ tmrParsed tcm

  (details, guts) <- do
        -- write core file
        -- give variables unique OccNames
        tidy_opts <- initTidyOpts session
        (guts, details) <- tidyProgram tidy_opts simplified_guts
        pure (details, guts)

  let !partial_iface = force $ mkPartialIface session
                                              (cg_binds guts)
                                              details
                                              ms
#if MIN_VERSION_ghc(9,11,0)
                                              (tcg_import_decls (tmrTypechecked tcm))
#endif
                                              simplified_guts

  final_iface' <- mkFullIface session partial_iface Nothing
                    Nothing
#if MIN_VERSION_ghc(9,11,0)
                    NoStubs []
#endif
  -- See Note [Clearing mi_globals after generating an iface]
  let final_iface = final_iface'
#if MIN_VERSION_ghc(9,11,0)
                      & set_mi_top_env Nothing
                      & set_mi_usages (filterUsages (mi_usages final_iface'))
#else
                      {mi_globals = Nothing, mi_usages = filterUsages (mi_usages final_iface')}
#endif

  -- Write the core file now
  core_file <- do
        let core_fp  = ml_core_file $ ms_location ms
            core_file = codeGutsToCoreFile iface_hash guts
            iface_hash = getModuleHash final_iface
        core_hash1 <- atomicFileWrite se core_fp $ \fp ->
          writeBinCoreFile (hsc_dflags session) fp core_file
        -- We want to drop references to guts and read in a serialized, compact version
        -- of the core file from disk (as it is deserialised lazily)
        -- This is because we don't want to keep the guts in memory for every file in
        -- the project as it becomes prohibitively expensive
        -- The serialized file however is much more compact and only requires a few
        -- hundred megabytes of memory total even in a large project with 1000s of
        -- modules
        (coreFile, !core_hash2) <- readBinCoreFile (mkUpdater $ hsc_NC session) core_fp
        pure $ assert (core_hash1 == core_hash2)
             $ Just (coreFile, fingerprintToBS core_hash2)

  -- Verify core file by roundtrip testing and comparison
  IdeOptions{optVerifyCoreFile} <- getIdeOptionsIO se
  case core_file of
    Just (core, _) | optVerifyCoreFile -> do
      let core_fp = ml_core_file $ ms_location ms
      traceIO $ "Verifying " ++ core_fp
      let CgGuts{cg_binds = unprep_binds, cg_tycons = tycons } = guts
          mod = ms_mod ms
          data_tycons = filter isDataTyCon tycons
      CgGuts{cg_binds = unprep_binds'} <- coreFileToCgGuts session final_iface details core
      cp_cfg <- initCorePrepConfig session
      let corePrep = corePrepPgm
                       (hsc_logger session) cp_cfg (initCorePrepPgmConfig (hsc_dflags session) (interactiveInScope $ hsc_IC session))
                       mod (ms_location ms)

      -- Run corePrep first as we want to test the final version of the program that will
      -- get translated to STG/Bytecode
      prepd_binds
        <- corePrep unprep_binds data_tycons
      prepd_binds'
        <- corePrep unprep_binds' data_tycons
      let binds  = noUnfoldings $ (map flattenBinds . (:[])) prepd_binds
          binds' = noUnfoldings $ (map flattenBinds . (:[])) prepd_binds'

          -- diffBinds is unreliable, sometimes it goes down the wrong track.
          -- This fixes the order of the bindings so that it is less likely to do so.
          diffs2 = concat $ flip S.evalState (mkRnEnv2 emptyInScopeSet) $ zipWithM go binds binds'
          -- diffs1 = concat $ flip S.evalState (mkRnEnv2 emptyInScopeSet) $ zipWithM go (map (:[]) $ concat binds) (map (:[]) $ concat binds')
          -- diffs3  = flip S.evalState (mkRnEnv2 emptyInScopeSet) $ go (concat binds) (concat binds')

          diffs = diffs2
          go x y = S.state $ \s -> diffBinds True s x y

          -- The roundtrip doesn't preserver OtherUnfolding or occInfo, but neither are of these
          -- are used for generate core or bytecode, so we can safely ignore them
          -- SYB is slow but fine given that this is only used for testing
          noUnfoldings = everywhere $ mkT $ \v -> if isId v
            then
              let v' = if isOtherUnfolding (realIdUnfolding v) then setIdUnfolding v noUnfolding else v
                in setIdOccInfo v' noOccInfo
            else v
          isOtherUnfolding (OtherCon _) = True
          isOtherUnfolding _            = False


      when (not $ null diffs) $
        panicDoc "verify core failed!" (vcat $ punctuate (text "\n\n") diffs) -- ++ [ppr binds , ppr binds']))
    _ -> pure ()

  pure ([], Just $! mkHiFileResult ms final_iface details (tmrRuntimeModules tcm) core_file)

  where
    dflags = hsc_dflags session'
    source = "compile"
    catchErrs x = x `catches`
      [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
      , Handler $ \diag ->
          return
            ( diagFromString
                source DiagnosticSeverity_Error (noSpan "<internal>")
                ("Error during " ++ T.unpack source ++ show @SomeException diag)
                Nothing
            , Nothing
            )
      ]

-- | Whether we should run the -O0 simplifier when generating core.
--
-- This is required for template Haskell to work but we disable this in DAML.
-- See #256
newtype RunSimplifier = RunSimplifier Bool

-- | Compile a single type-checked module to a 'CoreModule' value, or
-- provide errors.
compileModule
    :: RunSimplifier
    -> HscEnv
    -> ModSummary
    -> TcGblEnv
    -> IO (IdeResult ModGuts)
compileModule (RunSimplifier simplify) session ms tcg =
    fmap (either (, Nothing) (second Just)) $
        catchSrcErrors (hsc_dflags session) "compile" $ do
            (warnings,desugared_guts) <- withWarnings "compile" $ \tweak -> do
                 -- Breakpoints don't survive roundtripping from disk
                 -- and this trips up the verify-core-files check
                 -- They may also lead to other problems.
                 -- We have to setBackend ghciBackend in 9.8 as otherwise
                 -- non-exported definitions are stripped out.
                 -- However, setting this means breakpoints are generated.
                 -- Solution: prevent breakpoing generation by unsetting
                 -- Opt_InsertBreakpoints
               let session' = tweak $ flip hscSetFlags session
#if MIN_VERSION_ghc(9,7,0)
                                    $ flip gopt_unset Opt_InsertBreakpoints
                                    $ setBackend ghciBackend
#endif
                                    $ ms_hspp_opts ms
               -- TODO: maybe settings ms_hspp_opts is unnecessary?
               -- MP: the flags in ModSummary should be right, if they are wrong then
               -- the correct place to fix this is when the ModSummary is created.
               desugar <- hscDesugar session' (ms { ms_hspp_opts = hsc_dflags session' }) tcg
               if simplify
               then do
                 plugins <- readIORef (tcg_th_coreplugins tcg)
                 hscSimplify session' plugins desugar
               else pure desugar
            return (map snd warnings, desugared_guts)

generateObjectCode :: HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateObjectCode session summary guts = do
    fmap (either (, Nothing) (second Just)) $
          catchSrcErrors (hsc_dflags session) "object" $ do
              let dot_o =  ml_obj_file (ms_location summary)
                  mod = ms_mod summary
                  fp = replaceExtension dot_o "s"
              createDirectoryIfMissing True (takeDirectory fp)
              (warnings, dot_o_fp) <-
                withWarnings "object" $ \tweak -> do
                      let env' = tweak (hscSetFlags (ms_hspp_opts summary) session)
                          target = platformDefaultBackend (hsc_dflags env')
                          newFlags = setBackend target $ updOptLevel 0 $ setOutputFile
                              (Just dot_o)
                            $ hsc_dflags env'
                          session' = hscSetFlags newFlags session
                      (outputFilename, _mStub, _foreign_files, _cinfos, _stgcinfos) <- hscGenHardCode session' guts
                                (ms_location summary)
                                fp
                      obj <- compileFile session' driverNoStop (outputFilename, Just (As False))
                      case obj of
                        Nothing -> throwGhcExceptionIO $ Panic "compileFile didn't generate object code"
                        Just x -> pure x
              -- Need time to be the modification time for recompilation checking
              t <- liftIO $ getModificationTime dot_o_fp
#if MIN_VERSION_ghc(9,11,0)
              let linkable = Linkable t mod (pure $ DotO dot_o_fp ModuleObject)
#else
              let linkable = LM t mod [DotO dot_o_fp]
#endif
              pure (map snd warnings, linkable)

newtype CoreFileTime = CoreFileTime UTCTime

generateByteCode :: CoreFileTime -> HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateByteCode (CoreFileTime time) hscEnv summary guts = do
    fmap (either (, Nothing) (second Just)) $
          catchSrcErrors (hsc_dflags hscEnv) "bytecode" $ do

#if MIN_VERSION_ghc(9,11,0)
              (warnings, (_, bytecode)) <-
                withWarnings "bytecode" $ \_tweak -> do
                      let session = _tweak (hscSetFlags (ms_hspp_opts summary) hscEnv)
                          -- TODO: maybe settings ms_hspp_opts is unnecessary?
                          summary' = summary { ms_hspp_opts = hsc_dflags session }
                      hscInteractive session (mkCgInteractiveGuts guts)
                                (ms_location summary')
#else
              (warnings, (_, bytecode, sptEntries)) <-
                withWarnings "bytecode" $ \_tweak -> do
                      let session = _tweak (hscSetFlags (ms_hspp_opts summary) hscEnv)
                          -- TODO: maybe settings ms_hspp_opts is unnecessary?
                          summary' = summary { ms_hspp_opts = hsc_dflags session }
                      hscInteractive session (mkCgInteractiveGuts guts)
                                (ms_location summary')
#endif

#if MIN_VERSION_ghc(9,11,0)
              let linkable = Linkable time (ms_mod summary) (pure $ BCOs bytecode)
#else
              let linkable = LM time (ms_mod summary) [BCOs bytecode sptEntries]
#endif

              pure (map snd warnings, linkable)

demoteTypeErrorsToWarnings :: ParsedModule -> ParsedModule
demoteTypeErrorsToWarnings =
  (update_pm_mod_summary . update_hspp_opts) demoteTEsToWarns where

  demoteTEsToWarns :: DynFlags -> DynFlags
  -- convert the errors into warnings, and also check the warnings are enabled
  demoteTEsToWarns = (`wopt_set` Opt_WarnDeferredTypeErrors)
                   . (`wopt_set` Opt_WarnTypedHoles)
                   . (`wopt_set` Opt_WarnDeferredOutOfScopeVariables)
                   . (`gopt_set` Opt_DeferTypeErrors)
                   . (`gopt_set` Opt_DeferTypedHoles)
                   . (`gopt_set` Opt_DeferOutOfScopeVariables)

update_hspp_opts :: (DynFlags -> DynFlags) -> ModSummary -> ModSummary
update_hspp_opts up ms = ms{ms_hspp_opts = up $ ms_hspp_opts ms}

update_pm_mod_summary :: (ModSummary -> ModSummary) -> ParsedModule -> ParsedModule
update_pm_mod_summary up pm =
  pm{pm_mod_summary = up $ pm_mod_summary pm}

unDefer :: (Maybe DiagnosticReason, FileDiagnostic) -> (Bool, FileDiagnostic)
unDefer (Just (WarningWithFlag Opt_WarnDeferredTypeErrors)         , fd) = (True, upgradeWarningToError fd)
unDefer (Just (WarningWithFlag Opt_WarnTypedHoles)                 , fd) = (True, upgradeWarningToError fd)
unDefer (Just (WarningWithFlag Opt_WarnDeferredOutOfScopeVariables), fd) = (True, upgradeWarningToError fd)
unDefer ( _                                        , fd) = (False, fd)

upgradeWarningToError :: FileDiagnostic -> FileDiagnostic
upgradeWarningToError =
  fdLspDiagnosticL %~ \diag -> diag {_severity = Just DiagnosticSeverity_Error, _message = warn2err $ _message diag}
  where
  warn2err :: T.Text -> T.Text
  warn2err = T.intercalate ": error:" . T.splitOn ": warning:"

hideDiag :: DynFlags -> (Maybe DiagnosticReason, FileDiagnostic) -> (Maybe DiagnosticReason, FileDiagnostic)
hideDiag originalFlags (w@(Just (WarningWithFlag warning)), fd)
  | not (wopt warning originalFlags)
  = (w, fd { fdShouldShowDiagnostic = HideDiag })
hideDiag _originalFlags t = t

-- | Warnings which lead to a diagnostic tag
unnecessaryDeprecationWarningFlags :: [WarningFlag]
unnecessaryDeprecationWarningFlags
  = [ Opt_WarnUnusedTopBinds
    , Opt_WarnUnusedLocalBinds
    , Opt_WarnUnusedPatternBinds
    , Opt_WarnUnusedImports
    , Opt_WarnUnusedMatches
    , Opt_WarnUnusedTypePatterns
    , Opt_WarnUnusedForalls
    , Opt_WarnUnusedRecordWildcards
    , Opt_WarnInaccessibleCode
#if !MIN_VERSION_ghc(9,7,0)
    , Opt_WarnWarningsDeprecations
#endif
    ]

-- | Add a unnecessary/deprecated tag to the required diagnostics.
tagDiag :: (Maybe DiagnosticReason, FileDiagnostic) -> (Maybe DiagnosticReason, FileDiagnostic)

#if MIN_VERSION_ghc(9,7,0)
tagDiag (w@(Just (WarningWithCategory cat)), fd)
  | cat == defaultWarningCategory -- default warning category is for deprecations
  = (w, fd & fdLspDiagnosticL %~ \diag -> diag { _tags = Just $ DiagnosticTag_Deprecated : concat (_tags diag) })
tagDiag (w@(Just (WarningWithFlags warnings)), fd)
  | tags <- mapMaybe requiresTag (toList warnings)
  = (w, fd & fdLspDiagnosticL %~ \diag -> diag { _tags = Just $ tags ++ concat (_tags diag) })
#else
tagDiag (w@(Just (WarningWithFlag warning)), fd)
  | Just tag <- requiresTag warning
  = (w, fd & fdLspDiagnosticL %~ \diag -> diag { _tags = Just $ tag : concat (_tags diag) })
#endif
    where
    requiresTag :: WarningFlag -> Maybe DiagnosticTag
#if !MIN_VERSION_ghc(9,7,0)
    -- doesn't exist on 9.8, we use WarningWithCategory instead
    requiresTag Opt_WarnWarningsDeprecations
      = Just DiagnosticTag_Deprecated
#endif
    requiresTag wflag  -- deprecation was already considered above
      | wflag `elem` unnecessaryDeprecationWarningFlags
      = Just DiagnosticTag_Unnecessary
    requiresTag _ = Nothing
-- other diagnostics are left unaffected
tagDiag t = t

addRelativeImport :: NormalizedFilePath -> ModuleName -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

-- | Also resets the interface store
atomicFileWrite :: ShakeExtras -> FilePath -> (FilePath -> IO a) -> IO a
atomicFileWrite se targetPath write = do
  let dir = takeDirectory targetPath
  createDirectoryIfMissing True dir
  (tempFilePath, cleanUp) <- newTempFileWithin dir
  (write tempFilePath >>= \x -> renameFile tempFilePath targetPath >> atomically (resetInterfaceStore se (toNormalizedFilePath' targetPath)) >> pure x)
    `onException` cleanUp

generateHieAsts :: HscEnv -> TcModuleResult
#if MIN_VERSION_ghc(9,11,0)
                -> IO ([FileDiagnostic], Maybe (HieASTs Type, NameEntityInfo))
#else
                -> IO ([FileDiagnostic], Maybe (HieASTs Type))
#endif
generateHieAsts hscEnv tcm =
  handleGenerationErrors' dflags "extended interface generation" $ runHsc hscEnv $ do
    -- These varBinds use unitDataConId but it could be anything as the id name is not used
    -- during the hie file generation process. It's a workaround for the fact that the hie modules
    -- don't export an interface which allows for additional information to be added to hie files.
    let fake_splice_binds =
#if !MIN_VERSION_ghc(9,11,0)
                            Util.listToBag $
#endif
                            map (mkVarBind unitDataConId) (spliceExpressions $ tmrTopLevelSplices tcm)
        real_binds = tcg_binds $ tmrTypechecked tcm
        all_binds =
#if MIN_VERSION_ghc(9,11,0)
          fake_splice_binds ++ real_binds
#else
          fake_splice_binds `Util.unionBags` real_binds
#endif
        ts = tmrTypechecked tcm :: TcGblEnv
        top_ev_binds = tcg_ev_binds ts :: Util.Bag EvBind
        insts = tcg_insts ts :: [ClsInst]
        tcs = tcg_tcs ts :: [TyCon]
        hie_asts = GHC.enrichHie all_binds (tmrRenamed tcm) top_ev_binds insts tcs

    pure $ Just $
#if MIN_VERSION_ghc(9,11,0)
      hie_asts (tcg_type_env ts)
#else
      hie_asts
#endif
  where
    dflags = hsc_dflags hscEnv

spliceExpressions :: Splices -> [LHsExpr GhcTc]
spliceExpressions Splices{..} =
    DL.toList $ mconcat
        [ DL.fromList $ map fst exprSplices
        , DL.fromList $ map fst patSplices
        , DL.fromList $ map fst typeSplices
        , DL.fromList $ map fst declSplices
        , DL.fromList $ map fst awSplices
        ]

-- | In addition to indexing the `.hie` file, this function is responsible for
-- maintaining the 'IndexQueue' state and notifying the user about indexing
-- progress.
--
-- We maintain a record of all pending index operations in the 'indexPending'
-- TVar.
-- When 'indexHieFile' is called, it must check to ensure that the file hasn't
-- already be queued up for indexing. If it has, then we can just skip it
--
-- Otherwise, we record the current file as pending and write an indexing
-- operation to the queue
--
-- When the indexing operation is picked up and executed by the worker thread,
-- the first thing it does is ensure that a newer index for the same file hasn't
-- been scheduled by looking at 'indexPending'. If a newer index has been
-- scheduled, we can safely skip this one
--
-- Otherwise, we start or continue a progress reporting session, telling it
-- about progress so far and the current file we are attempting to index. Then
-- we can go ahead and call in to hiedb to actually do the indexing operation
--
-- Once this completes, we have to update the 'IndexQueue' state. First, we
-- must remove the just indexed file from 'indexPending' Then we check if
-- 'indexPending' is now empty. In that case, we end the progress session and
-- report the total number of file indexed. We also set the 'indexCompleted'
-- TVar to 0 in order to set it up for a fresh indexing session. Otherwise, we
-- can just increment the 'indexCompleted' TVar and exit.
--
indexHieFile :: ShakeExtras -> ModSummary -> NormalizedFilePath -> Util.Fingerprint -> Compat.HieFile -> IO ()
indexHieFile se mod_summary srcPath !hash hf = do
 atomically $ do
  pending <- readTVar indexPending
  case HashMap.lookup srcPath pending of
    Just pendingHash | pendingHash == hash -> pure () -- An index is already scheduled
    _ -> do
      -- hiedb doesn't use the Haskell src, so we clear it to avoid unnecessarily keeping it around
      let !hf' = hf{hie_hs_src = mempty}
      modifyTVar' indexPending $ HashMap.insert srcPath hash
      writeTQueue indexQueue $ \withHieDb -> do
        -- We are now in the worker thread
        -- Check if a newer index of this file has been scheduled, and if so skip this one
        newerScheduled <- atomically $ do
          pendingOps <- readTVar indexPending
          pure $ case HashMap.lookup srcPath pendingOps of
            Nothing          -> False
            -- If the hash in the pending list doesn't match the current hash, then skip
            Just pendingHash -> pendingHash /= hash
        unless newerScheduled $ do
          -- Using bracket, so even if an exception happen during withHieDb call,
          -- the `post` (which clean the progress indicator) will still be called.
          bracket_ pre post $
            withHieDb (\db -> HieDb.addRefsFromLoaded db targetPath (HieDb.RealFile $ fromNormalizedFilePath srcPath) hash hf')
  where
    mod_location    = ms_location mod_summary
    targetPath      = Compat.ml_hie_file mod_location
    HieDbWriter{..} = hiedbWriter se

    pre = progressUpdate indexProgressReporting ProgressStarted
    -- Report the progress once we are done indexing this file
    post = do
      mdone <- atomically $ do
        -- Remove current element from pending
        pending <- stateTVar indexPending $
          dupe . HashMap.update (\pendingHash -> guard (pendingHash /= hash) $> pendingHash) srcPath
        modifyTVar' indexCompleted (+1)
        -- If we are done, report and reset completed
        whenMaybe (HashMap.null pending) $
          swapTVar indexCompleted 0
      whenJust (lspEnv se) $ \env -> LSP.runLspT env $
        when (coerce $ ideTesting se) $
          LSP.sendNotification (LSP.SMethod_CustomMethod (Proxy @"ghcide/reference/ready")) $
            toJSON $ fromNormalizedFilePath srcPath
      whenJust mdone $ \_ -> progressUpdate indexProgressReporting ProgressCompleted

writeAndIndexHieFile
  :: HscEnv -> ShakeExtras -> ModSummary -> NormalizedFilePath -> [GHC.AvailInfo]
#if MIN_VERSION_ghc(9,11,0)
  -> (HieASTs Type, NameEntityInfo)
#else
  -> HieASTs Type
#endif
  -> BS.ByteString -> IO [FileDiagnostic]
writeAndIndexHieFile hscEnv se mod_summary srcPath exports ast source =
  handleGenerationErrors dflags "extended interface write/compression" $ do
    hf <- runHsc hscEnv $
      GHC.mkHieFile' mod_summary exports ast source
    atomicFileWrite se targetPath $ flip GHC.writeHieFile hf
    hash <- Util.getFileHash targetPath
    indexHieFile se mod_summary srcPath hash hf
  where
    dflags       = hsc_dflags hscEnv
    mod_location = ms_location mod_summary
    targetPath   = Compat.ml_hie_file mod_location

writeHiFile :: ShakeExtras -> HscEnv -> HiFileResult -> IO [FileDiagnostic]
writeHiFile se hscEnv tc =
  handleGenerationErrors dflags "interface write" $ do
    atomicFileWrite se targetPath $ \fp ->
      writeIfaceFile hscEnv fp modIface
  where
    modIface = hirModIface tc
    targetPath = ml_hi_file $ ms_location $ hirModSummary tc
    dflags = hsc_dflags hscEnv

handleGenerationErrors :: DynFlags -> T.Text -> IO () -> IO [FileDiagnostic]
handleGenerationErrors dflags source action =
  action >> return [] `catches`
    [ Handler $ return . diagFromGhcException source dflags
    , Handler $ \(exception :: SomeException) -> return $
        diagFromString
          source DiagnosticSeverity_Error (noSpan "<internal>")
          ("Error during " ++ T.unpack source ++ show exception)
          Nothing
    ]

handleGenerationErrors' :: DynFlags -> T.Text -> IO (Maybe a) -> IO ([FileDiagnostic], Maybe a)
handleGenerationErrors' dflags source action =
  fmap ([],) action `catches`
    [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
    , Handler $ \(exception :: SomeException) ->
        return
          ( diagFromString
              source DiagnosticSeverity_Error (noSpan "<internal>")
              ("Error during " ++ T.unpack source ++ show exception)
              Nothing
          , Nothing
          )
    ]

-- Merge the HPTs, module graphs and FinderCaches
-- See Note [GhcSessionDeps] in Development.IDE.Core.Rules
-- Add the current ModSummary to the graph, along with the
-- HomeModInfo's of all direct dependencies (by induction hypothesis all
-- transitive dependencies will be contained in envs)
#if MIN_VERSION_ghc(9,11,0)
mergeEnvs :: HscEnv
          -> ModuleGraph
          -> DependencyInformation
          -> ModSummary
          -> [HomeModInfo]
          -> [HscEnv]
          -> IO HscEnv
mergeEnvs env mg dep_info ms extraMods envs = do
    return $! loadModulesHome extraMods $
      let newHug = foldl' mergeHUG (hsc_HUG env) (map hsc_HUG envs) in
      (hscUpdateHUG (const newHug) env){
          hsc_mod_graph = mg,
          hsc_FC = (hsc_FC env)
            { addToFinderCache = \gwib@(GWIB im _) val ->
                  if moduleUnit im `elem` hsc_all_home_unit_ids env
                  then pure ()
                  else addToFinderCache (hsc_FC env) gwib val
            , lookupFinderCache = \gwib@(GWIB im _) ->
                  if moduleUnit im `elem` hsc_all_home_unit_ids env
                  then case lookupModuleFile (im { moduleUnit = RealUnit (Definite $ moduleUnit im) }) dep_info of
                         Nothing -> pure Nothing
                         Just fs -> let ml = fromJust $ do
                                              id <- lookupPathToId (depPathIdMap dep_info) fs
                                              artifactModLocation (idToModLocation (depPathIdMap dep_info) id)
                                    in pure $ Just $ InstalledFound ml im
                  else lookupFinderCache (hsc_FC env) gwib
            }
      }

    where
        mergeHUG (UnitEnvGraph a) (UnitEnvGraph b) = UnitEnvGraph $ Map.unionWith mergeHUE a b
        mergeHUE a b = a { homeUnitEnv_hpt = mergeUDFM (homeUnitEnv_hpt a) (homeUnitEnv_hpt b) }
        mergeUDFM = plusUDFM_C combineModules

        combineModules a b
          | HsSrcFile <- mi_hsc_src (hm_iface a) = a
          | otherwise = b

#else
mergeEnvs :: HscEnv
          -> ModuleGraph
          -> DependencyInformation
          -> ModSummary
          -> [HomeModInfo]
          -> [HscEnv]
          -> IO HscEnv
mergeEnvs env mg _dep_info ms extraMods envs = do
    let im  = Compat.installedModule (toUnitId $ moduleUnit $ ms_mod ms) (moduleName (ms_mod ms))
        ifr = InstalledFound (ms_location ms) im
        curFinderCache = Compat.extendInstalledModuleEnv Compat.emptyInstalledModuleEnv im ifr
    newFinderCache <- concatFC curFinderCache (map hsc_FC envs)
    return $! loadModulesHome extraMods $
      let newHug = foldl' mergeHUG (hsc_HUG env) (map hsc_HUG envs) in
      (hscUpdateHUG (const newHug) env){
          hsc_FC = newFinderCache,
          hsc_mod_graph = mg
      }

    where
        mergeHUG (UnitEnvGraph a) (UnitEnvGraph b) = UnitEnvGraph $ Map.unionWith mergeHUE a b
        mergeHUE a b = a { homeUnitEnv_hpt = mergeUDFM (homeUnitEnv_hpt a) (homeUnitEnv_hpt b) }
        mergeUDFM = plusUDFM_C combineModules

        combineModules a b
          | HsSrcFile <- mi_hsc_src (hm_iface a) = a
          | otherwise = b

        -- Prefer non-boot files over non-boot files
        -- otherwise we can get errors like https://gitlab.haskell.org/ghc/ghc/-/issues/19816
        -- if a boot file shadows over a non-boot file
        combineModuleLocations a@(InstalledFound ml _) _ | Just fp <- ml_hs_file ml, not ("boot" `isSuffixOf` fp) = a
        combineModuleLocations _ b = b

        concatFC :: FinderCacheState -> [FinderCache] -> IO FinderCache
        concatFC cur xs = do
          fcModules <- mapM (readIORef . fcModuleCache) xs
          fcFiles <- mapM (readIORef . fcFileCache) xs
          fcModules' <- newIORef $! foldl' (plusInstalledModuleEnv combineModuleLocations) cur fcModules
          fcFiles' <- newIORef $! Map.unions fcFiles
          pure $ FinderCache fcModules' fcFiles'
#endif


withBootSuffix :: HscSource -> ModLocation -> ModLocation
withBootSuffix HsBootFile = addBootSuffixLocnOut
withBootSuffix _          = id

-- | Given a buffer, env and filepath, produce a module summary by parsing only the imports.
--   Runs preprocessors as needed.
getModSummaryFromImports
  :: HscEnv
  -> FilePath
  -> UTCTime
  -> Maybe Util.StringBuffer
  -> ExceptT [FileDiagnostic] IO ModSummaryResult
-- modTime is only used in GHC < 9.4
getModSummaryFromImports env fp _modTime mContents = do
-- src_hash is only used in GHC >= 9.4
    (contents, opts, ppEnv, _src_hash) <- preprocessor env fp mContents

    let dflags = hsc_dflags ppEnv

    -- The warns will hopefully be reported when we actually parse the module
    (_warns, L main_loc hsmod) <- parseHeader dflags fp contents

    -- Copied from `HeaderInfo.getImports`, but we also need to keep the parsed imports
    let mb_mod = hsmodName hsmod
        imps = hsmodImports hsmod

        mod = fmap unLoc mb_mod `Util.orElse` mAIN_NAME

        (src_idecls, ord_idecls) = partition ((== IsBoot) . ideclSource.unLoc) imps

        -- GHC.Prim doesn't exist physically, so don't go looking for it.
        -- ghc_prim_imports is only used in GHC >= 9.4
        (ordinary_imps, _ghc_prim_imports)
          = partition ((/= moduleName gHC_PRIM) . unLoc
                      . ideclName . unLoc)
                      ord_idecls

        implicit_prelude = xopt LangExt.ImplicitPrelude dflags
        implicit_imports = mkPrelImports mod main_loc
                                         implicit_prelude imps


        convImport (L _ i) = (
                               (ideclPkgQual i)
                             , reLoc $ ideclName i)

        msrImports = implicit_imports ++ imps

        rn_pkg_qual = renameRawPkgQual (hsc_unit_env ppEnv)
        rn_imps = fmap (\(pk, lmn@(L _ mn)) -> (rn_pkg_qual mn pk, lmn))
        srcImports = rn_imps $ map convImport src_idecls
        textualImports = rn_imps $ map convImport (implicit_imports ++ ordinary_imps)
        ghc_prim_import = not (null _ghc_prim_imports)


    -- Force bits that might keep the string buffer and DynFlags alive unnecessarily
    liftIO $ evaluate $ rnf srcImports
    liftIO $ evaluate $ rnf textualImports


    modLoc <- liftIO $ if mod == mAIN_NAME
        -- specially in tests it's common to have lots of nameless modules
        -- mkHomeModLocation will map them to the same hi/hie locations
        then mkHomeModLocation dflags (pathToModuleName fp) fp
        else mkHomeModLocation dflags mod fp

    let modl = mkHomeModule (hscHomeUnit ppEnv) mod
        sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile
        msrModSummary =
            ModSummary
                { ms_mod          = modl
                , ms_hie_date     = Nothing
                , ms_dyn_obj_date    = Nothing
                , ms_ghc_prim_import = ghc_prim_import
                , ms_hs_hash      = _src_hash

                , ms_hsc_src      = sourceType
                -- The contents are used by the GetModSummary rule
                , ms_hspp_buf     = Just contents
                , ms_hspp_file    = fp
                , ms_hspp_opts    = dflags
                , ms_iface_date   = Nothing
                , ms_location     = withBootSuffix sourceType modLoc
                , ms_obj_date     = Nothing
                , ms_parsed_mod   = Nothing
                , ms_srcimps      = srcImports
                , ms_textual_imps = textualImports
                }

    msrFingerprint <- liftIO $ computeFingerprint opts msrModSummary
    msrHscEnv <- liftIO $ Loader.initializePlugins (hscSetFlags (ms_hspp_opts msrModSummary) ppEnv)
    return ModSummaryResult{..}
    where
        -- Compute a fingerprint from the contents of `ModSummary`,
        -- eliding the timestamps, the preprocessed source and other non relevant fields
        computeFingerprint opts ModSummary{..} = do
            fingerPrintImports <- fingerprintFromPut $ do
                  put $ Util.uniq $ moduleNameFS $ moduleName ms_mod
                  forM_ (ms_srcimps ++ ms_textual_imps) $ \(mb_p, m) -> do
                    put $ Util.uniq $ moduleNameFS $ unLoc m
                    case mb_p of
                      G.NoPkgQual    -> pure ()
                      G.ThisPkg uid  -> put $ getKey $ getUnique uid
                      G.OtherPkg uid -> put $ getKey $ getUnique uid
            return $! Util.fingerprintFingerprints $
                    [ Util.fingerprintString fp
                    , fingerPrintImports
                    , modLocationFingerprint ms_location
                    ] ++ map Util.fingerprintString opts

        modLocationFingerprint :: ModLocation -> Util.Fingerprint
        modLocationFingerprint ModLocation{..} = Util.fingerprintFingerprints $
            Util.fingerprintString <$> [ fromMaybe "" ml_hs_file
                                         , ml_hi_file
                                         , ml_dyn_hi_file
                                         , ml_obj_file
                                         , ml_dyn_obj_file
                                         , ml_hie_file]

-- | Parse only the module header
parseHeader
       :: Monad m
       => DynFlags -- ^ flags to use
       -> FilePath  -- ^ the filename (for source locations)
       -> Util.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located (HsModule GhcPs))
parseHeader dflags filename contents = do
   let loc  = mkRealSrcLoc (Util.mkFastString filename) 1 1
   case unP Compat.parseHeader (initParserState (initParserOpts dflags) contents loc) of
     PFailedWithErrorMessages msgs ->
        throwE $ diagFromGhcErrorMessages sourceParser dflags $ msgs dflags
     POk pst rdr_module -> do
        let (warns, errs) = renderMessages $ getPsMessages pst

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
            throwE $ diagFromGhcErrorMessages sourceParser dflags errs

        let warnings = diagFromGhcErrorMessages sourceParser dflags warns
        return (warnings, rdr_module)

-- | Given a buffer, flags, and file path, produce a
-- parsed module (or errors) and any parse warnings. Does not run any preprocessors
-- ModSummary must contain the (preprocessed) contents of the buffer
parseFileContents
       :: HscEnv
       -> (GHC.ParsedSource -> IdePreprocessedSource)
       -> FilePath  -- ^ the filename (for source locations)
       -> ModSummary
       -> ExceptT [FileDiagnostic] IO ([FileDiagnostic], ParsedModule)
parseFileContents env customPreprocessor filename ms = do
   let loc  = mkRealSrcLoc (Util.mkFastString filename) 1 1
       dflags = ms_hspp_opts ms
       contents = fromJust $ ms_hspp_buf ms
   case unP Compat.parseModule (initParserState (initParserOpts dflags) contents loc) of
     PFailedWithErrorMessages msgs ->
       throwE $ diagFromGhcErrorMessages sourceParser dflags $ msgs dflags
     POk pst rdr_module ->
         let
             psMessages = getPsMessages pst
         in
           do
               let IdePreprocessedSource preproc_warns preproc_errs parsed = customPreprocessor rdr_module
               let attachNoStructuredError (span, msg) = (span, msg, Nothing)

               unless (null preproc_errs) $
                  throwE $
                    diagFromStrings
                      sourceParser
                      DiagnosticSeverity_Error
                      (fmap attachNoStructuredError preproc_errs)

               let preproc_warning_file_diagnostics =
                     diagFromStrings
                      sourceParser
                      DiagnosticSeverity_Warning
                      (fmap attachNoStructuredError preproc_warns)
               (parsed', msgs) <- liftIO $ applyPluginsParsedResultAction env ms parsed psMessages
               let (warns, errors) = renderMessages msgs

               -- Just because we got a `POk`, it doesn't mean there
               -- weren't errors! To clarify, the GHC parser
               -- distinguishes between fatal and non-fatal
               -- errors. Non-fatal errors are the sort that don't
               -- prevent parsing from continuing (that is, a parse
               -- tree can still be produced despite the error so that
               -- further errors/warnings can be collected). Fatal
               -- errors are those from which a parse tree just can't
               -- be produced.
               unless (null errors) $
                 throwE $ diagFromGhcErrorMessages sourceParser dflags errors

               -- To get the list of extra source files, we take the list
               -- that the parser gave us,
               --   - eliminate files beginning with '<'.  gcc likes to use
               --     pseudo-filenames like "<built-in>" and "<command-line>"
               --   - normalise them (eliminate differences between ./f and f)
               --   - filter out the preprocessed source file
               --   - filter out anything beginning with tmpdir
               --   - remove duplicates
               --   - filter out the .hs/.lhs source filename if we have one
               --
               let n_hspp  = normalise filename
                   TempDir tmp_dir = tmpDir dflags
                   srcs0 = nubOrd $ filter (not . (tmp_dir `isPrefixOf`))
                                  $ filter (/= n_hspp)
                                  $ map normalise
                                  $ filter (not . isPrefixOf "<")
                                  $ map Util.unpackFS
                                  $ srcfiles pst
                   srcs1 = case ml_hs_file (ms_location ms) of
                             Just f  -> filter (/= normalise f) srcs0
                             Nothing -> srcs0

               -- sometimes we see source files from earlier
               -- preprocessing stages that cannot be found, so just
               -- filter them out:
               srcs2 <- liftIO $ filterM doesFileExist srcs1

               let pm = ParsedModule ms parsed' srcs2
                   warnings = diagFromGhcErrorMessages sourceParser dflags warns
               pure (warnings ++ preproc_warning_file_diagnostics, pm)

loadHieFile :: Compat.NameCacheUpdater -> FilePath -> IO GHC.HieFile
loadHieFile ncu f = do
  GHC.hie_file_result <$> GHC.readHieFile ncu f


{- Note [Recompilation avoidance in the presence of TH]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Most versions of GHC we currently support don't have a working implementation of
code unloading for object code, and no version of GHC supports this on certain
platforms like Windows. This makes it completely infeasible for interactive use,
as symbols from previous compiles will shadow over all future compiles.

This means that we need to use bytecode when generating code for Template
Haskell. Unfortunately, we can't serialize bytecode, so we will always need
to recompile when the IDE starts. However, we can put in place a much tighter
recompilation avoidance scheme for subsequent compiles:

1. If the source file changes, then we always need to recompile
   a. For files of interest, we will get explicit `textDocument/change` events
   that will let us invalidate our build products
   b. For files we read from disk, we can detect source file changes by
   comparing the `mtime` of the source file with the build product (.hi/.o) file
   on disk.
2. If GHC's recompilation avoidance scheme based on interface file hashes says
   that we need to recompile, the we need to recompile.
3. If the file in question requires code generation then, we need to recompile
   if we don't have the appropriate kind of build products.
   a. If we already have the build products in memory, and the conditions 1 and
   2 above hold, then we don't need to recompile
   b. If we are generating object code, then we can also search for it on
   disk and ensure it is up to date. Notably, we did _not_ previously re-use
   old bytecode from memory when `hls-graph`/`shake` decided to rebuild the
   `HiFileResult` for some reason

4. If the file in question used Template Haskell on the previous compile, then
we need to recompile if any `Linkable` in its transitive closure changed. This
sounds bad, but it is possible to make some improvements. In particular, we only
need to recompile if any of the `Linkable`s actually used during the previous
compile change.

How can we tell if a `Linkable` was actually used while running some TH?

GHC provides a `hscCompileCoreExprHook` which lets us intercept bytecode as
it is being compiled and linked. We can inspect the bytecode to see which
`Linkable` dependencies it requires, and record this for use in
recompilation checking.
We record all the home package modules of the free names that occur in the
bytecode. The `Linkable`s required are then the transitive closure of these
modules in the home-package environment. This is the same scheme as used by
GHC to find the correct things to link in before running bytecode.

This works fine if we already have previous build products in memory, but
what if we are reading an interface from disk? Well, we can smuggle in the
necessary information (linkable `Module`s required as well as the time they
were generated) using `Annotation`s, which provide a somewhat general purpose
way to serialise arbitrary information along with interface files.

Then when deciding whether to recompile, we need to check that the versions
(i.e. hashes) of the linkables used during a previous compile match whatever is
currently in the HPT.

As we always generate Linkables from core files, we use the core file hash
as a (hopefully) deterministic measure of whether the Linkable has changed.
This is better than using the object file hash (if we have one) because object
file generation is not deterministic.
-}

data RecompilationInfo m
  = RecompilationInfo
  { source_version :: FileVersion
  , old_value   :: Maybe (HiFileResult, FileVersion)
  , get_file_version :: NormalizedFilePath -> m (Maybe FileVersion)
  , get_linkable_hashes :: [NormalizedFilePath] -> m [BS.ByteString]
  , get_module_graph :: m DependencyInformation
  , regenerate  :: Maybe LinkableType -> m ([FileDiagnostic], Maybe HiFileResult) -- ^ Action to regenerate an interface
  }

-- | Either a regular GHC linkable or a core file that
-- can be later turned into a proper linkable
data IdeLinkable = GhcLinkable !Linkable | CoreLinkable !UTCTime !CoreFile

instance NFData IdeLinkable where
  rnf (GhcLinkable lb)      = rnf lb
  rnf (CoreLinkable time _) = rnf time

ml_core_file :: ModLocation -> FilePath
ml_core_file ml = ml_hi_file ml <.> "core"

-- | Returns an up-to-date module interface, regenerating if needed.
--   Assumes file exists.
--   Requires the 'HscEnv' to be set up with dependencies
-- See Note [Recompilation avoidance in the presence of TH]
loadInterface
  :: (MonadIO m, MonadMask m)
  => HscEnv
  -> ModSummary
  -> Maybe LinkableType
  -> RecompilationInfo m
  -> m ([FileDiagnostic], Maybe HiFileResult)
loadInterface session ms linkableNeeded RecompilationInfo{..} = do
    let sessionWithMsDynFlags = hscSetFlags (ms_hspp_opts ms) session
        mb_old_iface = hirModIface . fst <$> old_value
        mb_old_version = snd <$> old_value

        core_file = ml_core_file (ms_location ms)
        iface_file = ml_hi_file (ms_location ms)

        !mod = ms_mod ms

    mb_dest_version <- case mb_old_version of
      Just ver -> pure $ Just ver
      Nothing  -> get_file_version (toNormalizedFilePath' iface_file)

    -- The source is modified if it is newer than the destination (iface file)
    -- A more precise check for the core file is performed later
    let _sourceMod = case mb_dest_version of -- sourceMod is only used in GHC < 9.4
          Nothing -> SourceModified -- destination file doesn't exist, assume modified source
          Just dest_version
            | source_version <= dest_version -> SourceUnmodified
            | otherwise -> SourceModified

    -- old_iface is only used in GHC >= 9.4
    _old_iface <- case mb_old_iface of
      Just iface -> pure (Just iface)
      Nothing -> do
        -- ncu and read_dflags are only used in GHC >= 9.4
        let _ncu = hsc_NC sessionWithMsDynFlags
            _read_dflags = hsc_dflags sessionWithMsDynFlags
        read_result <- liftIO $ readIface _read_dflags _ncu mod iface_file
        case read_result of
          Util.Failed{}        -> return Nothing
          -- important to call `shareUsages` here before checkOldIface
          -- consults `mi_usages`
          Util.Succeeded iface -> return $ Just (shareUsages iface)

    -- If mb_old_iface is nothing then checkOldIface will load it for us
    -- given that the source is unmodified
    (recomp_iface_reqd, mb_checked_iface)
      <- liftIO $ checkOldIface sessionWithMsDynFlags ms _old_iface >>= \case
        UpToDateItem x -> pure (UpToDate, Just x)
        OutOfDateItem reason x -> pure (NeedsRecompile reason, x)

    let do_regenerate _reason = withTrace "regenerate interface" $ \setTag -> do
          setTag "Module" $ moduleNameString $ moduleName mod
          setTag "Reason" $ showReason _reason
          liftIO $ traceMarkerIO $ "regenerate interface " ++ show (moduleNameString $ moduleName mod, showReason _reason)
          regenerate linkableNeeded

    case (mb_checked_iface, recomp_iface_reqd) of
      (Just iface, UpToDate) -> do
             details <- liftIO $ mkDetailsFromIface sessionWithMsDynFlags iface
             -- parse the runtime dependencies from the annotations
             let runtime_deps
                   | not (mi_used_th iface) = emptyModuleEnv
                   | otherwise = parseRuntimeDeps (md_anns details)
             -- Peform the fine grained recompilation check for TH
             maybe_recomp <- checkLinkableDependencies get_linkable_hashes get_module_graph runtime_deps
             case maybe_recomp of
               Just msg -> do_regenerate msg
               Nothing
                 | isJust linkableNeeded -> handleErrs $ do
                   (coreFile@CoreFile{cf_iface_hash}, core_hash) <- liftIO $
                     readBinCoreFile (mkUpdater $ hsc_NC session) core_file
                   if cf_iface_hash == getModuleHash iface
                   then return ([], Just $ mkHiFileResult ms iface details runtime_deps (Just (coreFile, fingerprintToBS core_hash)))
                   else do_regenerate (recompBecause "Core file out of date (doesn't match iface hash)")
                 | otherwise -> return ([], Just $ mkHiFileResult ms iface details runtime_deps Nothing)
                 where handleErrs = flip catches
                         [Handler $ \(e :: IOException) -> do_regenerate (recompBecause $ "Reading core file failed (" ++ show e ++ ")")
                         ,Handler $ \(e :: GhcException) -> case e of
                            Signal _ -> throw e
                            Panic _  -> throw e
                            _        -> do_regenerate (recompBecause $ "Reading core file failed (" ++ show e ++ ")")
                         ]
      (_, _reason) -> do_regenerate _reason

-- | Find the runtime dependencies by looking at the annotations
-- serialized in the iface
-- The bytestrings are the hashes of the core files for modules we
-- required to run the TH splices in the given module.
-- See Note [Recompilation avoidance in the presence of TH]
parseRuntimeDeps :: [ModIfaceAnnotation] -> ModuleEnv BS.ByteString
parseRuntimeDeps anns = mkModuleEnv $ mapMaybe go anns
  where
    go (Annotation (ModuleTarget mod) payload)
      | Just bs <- fromSerialized BS.pack payload
      = Just (mod, bs)
    go _ = Nothing

-- | checkLinkableDependencies compares the core files in the build graph to
-- the runtime dependencies of the module, to check if any of them are out of date
-- Hopefully 'runtime_deps' will be empty if the module didn't actually use TH
-- See Note [Recompilation avoidance in the presence of TH]
checkLinkableDependencies :: MonadIO m => ([NormalizedFilePath] -> m [BS.ByteString]) -> m DependencyInformation -> ModuleEnv BS.ByteString -> m (Maybe RecompileRequired)
checkLinkableDependencies get_linkable_hashes get_module_graph runtime_deps = do
  graph <- get_module_graph
  let go (mod, hash) = (,hash) <$> lookupModuleFile mod graph
      hs_files = mapM go (moduleEnvToList runtime_deps)
  case hs_files of
    Nothing -> error "invalid module graph"
    Just fs -> do
      store_hashes <- get_linkable_hashes (map fst fs)
      let out_of_date = [core_file | ((core_file, expected_hash), actual_hash) <- zip fs store_hashes, expected_hash /= actual_hash]
      case out_of_date of
        [] -> pure Nothing
        _ -> pure $ Just $ recompBecause
              $ "out of date runtime dependencies: " ++ intercalate ", " (map show out_of_date)

recompBecause :: String -> RecompileRequired
recompBecause =
                NeedsRecompile .
                RecompBecause
              . CustomReason

data SourceModified = SourceModified | SourceUnmodified deriving (Eq, Ord, Show)

showReason :: RecompileRequired -> String
showReason UpToDate                     = "UpToDate"
showReason (NeedsRecompile MustCompile) = "MustCompile"
showReason (NeedsRecompile s)           = printWithoutUniques s

mkDetailsFromIface :: HscEnv -> ModIface -> IO ModDetails
mkDetailsFromIface session iface = do
  fixIO $ \details -> do
    let !hsc' = hscUpdateHPT (\hpt -> addToHpt hpt (moduleName $ mi_module iface) (HomeModInfo iface details emptyHomeModInfoLinkable)) session
    initIfaceLoad hsc' (typecheckIface iface)

coreFileToCgGuts :: HscEnv -> ModIface -> ModDetails -> CoreFile -> IO CgGuts
coreFileToCgGuts session iface details core_file = do
  let act hpt = addToHpt hpt (moduleName this_mod)
                             (HomeModInfo iface details emptyHomeModInfoLinkable)
      this_mod = mi_module iface
  types_var <- newIORef (md_types details)
  let hsc_env' = hscUpdateHPT act (session {
        hsc_type_env_vars = knotVarsFromModuleEnv (mkModuleEnv [(this_mod, types_var)])
        })
  core_binds <- initIfaceCheck (text "l") hsc_env' $ typecheckCoreFile this_mod types_var core_file
      -- Implicit binds aren't saved, so we need to regenerate them ourselves.
  let _implicit_binds = concatMap getImplicitBinds tyCons -- only used if GHC < 9.6
      tyCons = typeEnvTyCons (md_types details)
  -- In GHC 9.6, the implicit binds are tidied and part of core_binds
  pure $ CgGuts this_mod tyCons core_binds [] NoStubs [] mempty
#if !MIN_VERSION_ghc(9,11,0)
                (emptyHpcInfo False)
#endif
                Nothing []

coreFileToLinkable :: LinkableType -> HscEnv -> ModSummary -> ModIface -> ModDetails -> CoreFile -> UTCTime -> IO ([FileDiagnostic], Maybe HomeModInfo)
coreFileToLinkable linkableType session ms iface details core_file t = do
  cgi_guts <- coreFileToCgGuts session iface details core_file
  (warns, lb) <- case linkableType of
    BCOLinkable    -> fmap (maybe emptyHomeModInfoLinkable justBytecode) <$> generateByteCode (CoreFileTime t) session ms cgi_guts
    ObjectLinkable -> fmap (maybe emptyHomeModInfoLinkable justObjects) <$> generateObjectCode session ms cgi_guts
  pure (warns, Just $ HomeModInfo iface details lb) -- TODO wz1000 handle emptyHomeModInfoLinkable

-- | Non-interactive, batch version of 'InteractiveEval.getDocs'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
getDocsBatch
  :: HscEnv
  -> [Name]
  -> IO [Either String (Maybe [HsDoc GhcRn], IntMap (HsDoc GhcRn))]
getDocsBatch hsc_env _names = do
    res <- initIfaceLoad hsc_env $ forM _names $ \name ->
        case nameModule_maybe name of
            Nothing -> return (Left $ NameHasNoModule name)
            Just mod -> do
             ModIface {
                        mi_docs = Just Docs{ docs_mod_hdr = mb_doc_hdr
                                      , docs_decls = dmap
                                      , docs_args = amap
                                      }
                      } <- loadSysInterface (text "getModuleInterface") mod
             if isNothing mb_doc_hdr && isNullUniqMap dmap && isNullUniqMap amap
               then pure (Left (NoDocsInIface mod $ compiled name))
               else pure (Right (
                                  lookupUniqMap dmap name,
                                  lookupWithDefaultUniqMap amap mempty name))
    return $ map (first $ T.unpack . printOutputable) res
  where
    compiled n =
      -- TODO: Find a more direct indicator.
      case nameSrcLoc n of
        RealSrcLoc {}   -> False
        UnhelpfulLoc {} -> True

-- | Non-interactive, batch version of 'InteractiveEval.lookupNames'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
lookupName :: HscEnv
           -> Name
           -> IO (Maybe TyThing)
lookupName _ name
  | Nothing <- nameModule_maybe name = pure Nothing
lookupName hsc_env name = exceptionHandle $ do
  mb_thing <- liftIO $ lookupType hsc_env name
  case mb_thing of
    x@(Just _) -> return x
    Nothing
      | x@(Just thing) <- wiredInNameTyThing_maybe name
      -> do when (needWiredInHomeIface thing)
                 (initIfaceLoad hsc_env (loadWiredInHomeIface name))
            return x
      | otherwise -> do
        res <- initIfaceLoad hsc_env $ importDecl name
        case res of
          Util.Succeeded x -> return (Just x)
          _                -> return Nothing
  where
    exceptionHandle x = x `catch` \(_ :: IOEnvFailure) -> pure Nothing

pathToModuleName :: FilePath -> ModuleName
pathToModuleName = mkModuleName . map rep
  where
      rep c | isPathSeparator c = '_'
      rep ':' = '_'
      rep c = c

-- | Initialising plugins looks in the finder cache, but we know that the plugin doesn't come from a home module, so don't
-- error out when we don't find it
setNonHomeFCHook :: HscEnv -> HscEnv
setNonHomeFCHook hsc_env =
#if MIN_VERSION_ghc(9,11,0)
  hsc_env { hsc_FC = (hsc_FC hsc_env)
                        { lookupFinderCache = \m@(GWIB im _) ->
                            if moduleUnit im `elem` hsc_all_home_unit_ids hsc_env
                            then pure (Just $ InstalledNotFound [] Nothing)
                            else lookupFinderCache (hsc_FC hsc_env) m
                        }
          }
#else
  hsc_env
#endif

{- Note [Guidelines For Using CPP In GHCIDE Import Statements]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  GHCIDE's interface with GHC is extensive, and unfortunately, because we have
  to work with multiple versions of GHC, we have several files that need to use
  a lot of CPP. In order to simplify the CPP in the import section of every file
  we have a few specific guidelines for using CPP in these sections.

  - We don't want to nest CPP clauses, nor do we want to use else clauses. Both
  nesting and else clauses end up drastically complicating the code, and require
  significant mental stack to unwind.

  - CPP clauses should be placed at the end of the imports section. The clauses
  should be ordered by the GHC version they target from earlier to later versions,
  with negative if clauses coming before positive if clauses of the same
  version. (If you think about which GHC version a clause activates for this
  should make sense `!MIN_VERSION_GHC(9,0,0)` refers to 8.10 and lower which is
  an earlier version than `MIN_VERSION_GHC(9,0,0)` which refers to versions 9.0
  and later). In addition there should be a space before and after each CPP
  clause.

  - In if clauses that use `&&` and depend on more than one statement, the
  positive statement should come before the negative statement. In addition the
  clause should come after the single positive clause for that GHC version.

  - There shouldn't be multiple identical CPP statements. The use of odd or even
  GHC numbers is identical, with the only preference being to use what is
  already there. (i.e. (`MIN_VERSION_GHC(9,2,0)` and `MIN_VERSION_GHC(9,1,0)`
  are functionally equivalent)
-}
