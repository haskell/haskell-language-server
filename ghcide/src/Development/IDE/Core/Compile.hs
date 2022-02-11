-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP        #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

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
  ) where

import           Control.Concurrent.Extra
import           Control.Concurrent.STM.Stats      hiding (orElse)
import           Control.DeepSeq                   (force, liftRnf, rnf, rwhnf, NFData(..))
import           Control.Exception                 (evaluate)
import           Control.Exception.Safe
import           Control.Lens                      hiding (List, (<.>))
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.Trans.Except
import           Data.Aeson                        (toJSON)
import           Data.Bifunctor                    (first, second)
import           Data.Binary
import qualified Data.Binary                       as B
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.Coerce
import qualified Data.DList                        as DL
import           Data.Functor
import qualified Data.HashMap.Strict               as HashMap
import           Data.IORef
import           Data.IntMap                       (IntMap)
import qualified Data.IntMap.Strict                as IntMap
import           Data.List.Extra
import           Data.Map                          (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Text                         as T
import           Data.Time                         (UTCTime (..),
                                                    getCurrentTime)
import           Data.Time.Clock.POSIX             (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import           Data.Tuple.Extra                  (dupe)
import           Data.Unique                       as Unique
import           Debug.Trace
import           Development.IDE.Core.FileStore    (resetInterfaceStore)
import           Development.IDE.Core.Preprocessor
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing      (withTrace)
import           Development.IDE.GHC.Compat        hiding (loadInterface,
                                                    parseHeader, parseModule,
                                                    tcRnModule, writeHieFile)
import qualified Development.IDE.GHC.Compat        as Compat
import qualified Development.IDE.GHC.Compat        as GHC
import qualified Development.IDE.GHC.Compat.Util   as Util
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.GHC.Util
import           Development.IDE.GHC.Warnings
import           Development.IDE.Spans.Common
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import           Development.IDE.GHC.CoreFile
import           GHC                               (ForeignHValue,
                                                    GetDocsFailure (..),
                                                    mgModSummaries,
                                                    parsedSource)
import qualified GHC.LanguageExtensions            as LangExt
import           GHC.Serialized
import           HieDb
import qualified Language.LSP.Server               as LSP
import           Language.LSP.Types                (DiagnosticTag (..))
import qualified Language.LSP.Types                as LSP
import           System.Directory
import           System.FilePath
import           System.IO.Extra                   (fixIO, newTempFileWithin)
import           Unsafe.Coerce

#if !MIN_VERSION_ghc(8,10,0)
import           ErrUtils
#endif

#if MIN_VERSION_ghc(9,0,1)
import           GHC.Tc.Gen.Splice

#if MIN_VERSION_ghc(9,2,1)
import           GHC.Types.HpcInfo
import           GHC.Types.ForeignStubs
import           GHC.Types.TypeEnv
#else
import           GHC.Driver.Types
#endif

#else
import           TcSplice
import           HscTypes
#endif

import           Development.IDE.GHC.Compat.Util   (emptyUDFM, fsLit,
                                                    plusUDFM_C)
#if MIN_VERSION_ghc(9,2,0)
import           GHC                               (Anchor (anchor),
                                                    EpaComment (EpaComment),
                                                    EpaCommentTok (EpaBlockComment, EpaLineComment),
                                                    epAnnComments,
                                                    priorComments)
import qualified GHC                               as G
import           GHC.Hs                            (LEpaComment)
import qualified GHC.Types.Error                   as Error
#endif
import qualified Control.Monad.Trans.State.Strict as S
import Data.Generics.Schemes
import Data.Generics.Aliases

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
        Nothing -> return $ Left [ideErrorText (toNormalizedFilePath' noFilePath) $
            T.pack $ "unknown package: " ++ show pkg]
        Just pkgInfo -> return $ Right $ unitDepends pkgInfo

typecheckModule :: IdeDefer
                -> HscEnv
                -> ModuleEnv UTCTime -- ^ linkables not to unload
                -> ParsedModule
                -> IO (IdeResult TcModuleResult)
typecheckModule (IdeDefer defer) hsc keep_lbls pm = do
        let modSummary = pm_mod_summary pm
            dflags = ms_hspp_opts modSummary
        mmodSummary' <- catchSrcErrors (hsc_dflags hsc) "typecheck (initialize plugins)"
                                      (initPlugins hsc modSummary)
        case mmodSummary' of
          Left errs -> return (errs, Nothing)
          Right modSummary' -> do
            (warnings, etcm) <- withWarnings "typecheck" $ \tweak ->
                let
                  session = tweak (hscSetFlags dflags hsc)
                   -- TODO: maybe settings ms_hspp_opts is unnecessary?
                  mod_summary'' = modSummary' { ms_hspp_opts = hsc_dflags session}
                in
                  catchSrcErrors (hsc_dflags hsc) "typecheck" $ do
                    tcRnModule session keep_lbls $ demoteIfDefer pm{pm_mod_summary = mod_summary''}
            let errorPipeline = unDefer . hideDiag dflags . tagDiag
                diags = map errorPipeline warnings
                deferedError = any fst diags
            case etcm of
              Left errs -> return (map snd diags ++ errs, Nothing)
              Right tcm -> return (map snd diags, Just $ tcm{tmrDeferedError = deferedError})
    where
        demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id

-- | Install hooks to capture the splices as well as the runtime module dependencies
captureSplicesAndDeps :: HscEnv -> (HscEnv -> IO a) -> IO (a, Splices, UniqSet ModuleName)
captureSplicesAndDeps env k = do
  splice_ref <- newIORef mempty
  dep_ref <- newIORef emptyUniqSet
  res <- k (hscSetHooks (addSpliceHook splice_ref . addLinkableDepHook dep_ref $ hsc_hooks env) env)
  splices <- readIORef splice_ref
  needed_mods <- readIORef dep_ref
  return (res, splices, needed_mods)
  where
    addLinkableDepHook :: IORef (UniqSet ModuleName) -> Hooks -> Hooks
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
    --
    -- Only compute direct dependencies instead of transitive dependencies.
    -- It is much cheaper to store the direct dependencies, we can compute
    -- the transitive ones when required.
    -- Also only record dependencies from the home package
    compile_bco_hook :: IORef (UniqSet ModuleName) -> HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue
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


#if MIN_VERSION_ghc(9,2,0)
           ; let iNTERACTIVELoc = G.ModLocation{ ml_hs_file   = Nothing,
                                        ml_hi_file   = panic "hscCompileCoreExpr':ml_hi_file",
                                        ml_obj_file  = panic "hscCompileCoreExpr':ml_obj_file",
                                        ml_hie_file  = panic "hscCompileCoreExpr':ml_hie_file" }
           ; let ictxt = hsc_IC hsc_env

           ; (binding_id, stg_expr, _, _) <-
               myCoreToStgExpr (hsc_logger hsc_env)
                               (hsc_dflags hsc_env)
                               ictxt
                               (icInteractiveModule ictxt)
                               iNTERACTIVELoc
                               prepd_expr

             {- Convert to BCOs -}
           ; bcos <- byteCodeGen hsc_env
                       (icInteractiveModule ictxt)
                       stg_expr
                       [] Nothing
           ; let needed_mods = mkUniqSet [ moduleName mod | n <- concatMap (uniqDSetToList . bcoFreeNames) $ bc_bcos bcos
                                         , Just mod <- [nameModule_maybe n] -- Names from other modules
                                         , not (isWiredInName n) -- Exclude wired-in names
                                         , moduleUnitId mod == homeUnitId_ dflags -- Only care about stuff from the home package
                                         ]
            -- Exclude wired-in names because we may not have read
            -- their interface files, so getLinkDeps will fail
            -- All wired-in names are in the base package, which we link
            -- by default, so we can safely ignore them here.

             {- load it -}
           ; fv_hvs <- loadDecls (hscInterp hsc_env) hsc_env srcspan bcos
           ; let hval = (expectJust "hscCompileCoreExpr'" $ lookup (idName binding_id) fv_hvs)
#else
             {- Convert to BCOs -}
           ; bcos <- coreExprToBCOs hsc_env
                       (icInteractiveModule (hsc_IC hsc_env)) prepd_expr

           ; let needed_mods = mkUniqSet [ moduleName mod | n <- uniqDSetToList (bcoFreeNames bcos)
                                         , Just mod <- [nameModule_maybe n] -- Names from other modules
                                         , not (isWiredInName n) -- Exclude wired-in names
                                         , moduleUnitId mod == homeUnitId_ dflags -- Only care about stuff from the home package
                                         ]
            -- Exclude wired-in names because we may not have read
            -- their interface files, so getLinkDeps will fail
            -- All wired-in names are in the base package, which we link
            -- by default, so we can safely ignore them here.

             {- link it -}
           ; hval <- linkExpr hsc_env srcspan bcos
#endif

           ; modifyIORef' var (unionUniqSets needed_mods)
           ; return hval }


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
  -> ModuleEnv UTCTime -- ^ Program linkables not to unload
  -> ParsedModule
  -> IO TcModuleResult
tcRnModule hsc_env keep_lbls pmod = do
  let ms = pm_mod_summary pmod
      hsc_env_tmp = hscSetFlags (ms_hspp_opts ms) hsc_env
      hpt = hsc_HPT hsc_env

  unload hsc_env_tmp $ map (\(mod, time) -> LM time mod []) $ moduleEnvToList keep_lbls

  ((tc_gbl_env', mrn_info), splices, mods)
      <- captureSplicesAndDeps hsc_env_tmp $ \hsc_env_tmp ->
             do  hscTypecheckRename hsc_env_tmp ms $
                          HsParsedModule { hpm_module = parsedSource pmod,
                                           hpm_src_files = pm_extra_src_files pmod,
                                           hpm_annotations = pm_annotations pmod }
  let rn_info = case mrn_info of
        Just x  -> x
        Nothing -> error "no renamed info tcRnModule"

      -- Compute the transitive set of linkables required
      mods_transitive = go emptyUniqSet mods
        where
          go seen new
            | isEmptyUniqSet new = seen
            | otherwise = go seen' new'
              where
                seen' = seen `unionUniqSets` new
                new'  = new_deps `minusUniqSet` seen'
                new_deps = unionManyUniqSets [ mkUniqSet $ getDependentMods $ hm_iface mod_info
                                             | mod_info <- eltsUDFM $ udfmIntersectUFM hpt (getUniqSet new)]

      -- The linkables we depend on at runtime are the transitive closure of 'mods'
      -- restricted to the home package
      -- See Note [Recompilation avoidance in the presence of TH]
      mod_env = filterModuleEnv (\m _ -> elementOfUniqSet (moduleName m) mods_transitive) keep_lbls -- Could use restrictKeys if the constructors were exported

      -- Serialize mod_env so we can read it from the interface
      mod_env_anns = map (\(mod, time) -> Annotation (ModuleTarget mod) $ toSerialized serializeModDepTime (ModDepTime time))
                         (moduleEnvToList mod_env)
      tc_gbl_env = tc_gbl_env' { tcg_ann_env = extendAnnEnvList (tcg_ann_env tc_gbl_env') mod_env_anns }
  pure (TcModuleResult pmod rn_info tc_gbl_env splices False mod_env)

mkHiFileResultNoCompile :: HscEnv -> TcModuleResult -> IO HiFileResult
mkHiFileResultNoCompile session tcm = do
  let hsc_env_tmp = hscSetFlags (ms_hspp_opts ms) session
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm
  details <- makeSimpleDetails hsc_env_tmp tcGblEnv
  sf <- finalSafeMode (ms_hspp_opts ms) tcGblEnv
#if MIN_VERSION_ghc(8,10,0)
  iface <- mkIfaceTc hsc_env_tmp sf details tcGblEnv
#else
  (iface, _) <- mkIfaceTc hsc_env_tmp Nothing sf details tcGblEnv
#endif
  let mod_info = HomeModInfo iface details Nothing
  pure $! mkHiFileResult ms mod_info (tmrRuntimeModules tcm)

mkHiFileResultCompile
    :: ShakeExtras
    -> HscEnv
    -> TcModuleResult
    -> ModGuts
    -> LinkableType -- ^ use object code or byte code?
    -> IO (IdeResult HiFileResult)
mkHiFileResultCompile se session' tcm simplified_guts ltype = catchErrs $ do
  let session = hscSetFlags (ms_hspp_opts ms) session'
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm

  let genLinkable = case ltype of
        ObjectLinkable -> generateObjectCode
        BCOLinkable    -> generateByteCode se WriteCoreFile

  (linkable, details, mguts, diags) <-
    if mg_hsc_src simplified_guts == HsBootFile
    then do
        -- give variables unique OccNames
        details <- mkBootModDetailsTc session tcGblEnv
        pure (Nothing, details, Nothing, [])
    else do
        -- give variables unique OccNames
        (guts, details) <- tidyProgram session simplified_guts
        (diags, linkable) <- genLinkable session ms guts
        pure (linkable, details, Just guts, diags)
#if MIN_VERSION_ghc(9,0,1)
  let !partial_iface = force (mkPartialIface session details simplified_guts)
  final_iface <- mkFullIface session partial_iface Nothing
#elif MIN_VERSION_ghc(8,10,0)
  let !partial_iface = force (mkPartialIface session details simplified_guts)
  final_iface <- mkFullIface session partial_iface
#else
  (final_iface,_) <- mkIface session Nothing details simplified_guts
#endif
  let mod_info = HomeModInfo final_iface details linkable

  -- Verify core file by rountrip testing and comparison
  IdeOptions{optVerifyCoreFile} <- getIdeOptionsIO se
  when (maybe False (not . isObjectLinkable) linkable && optVerifyCoreFile) $ do
    let core_fp = ml_core_file $ ms_location ms
    traceIO $ "Verifying " ++ core_fp
    core <- readBinCoreFile (mkUpdater $ hsc_NC session) core_fp
    let CgGuts{cg_binds = unprep_binds, cg_tycons = tycons } = case mguts of
          Nothing -> error "invariant optVerifyCoreFile: guts must exist if linkable exists)"
          Just g -> g
        mod = ms_mod ms
        data_tycons = filter isDataTyCon tycons
    CgGuts{cg_binds = unprep_binds'} <- coreFileToCgGuts session final_iface details core

    -- Run corePrep first as we want to test the final version of the program that will
    -- get translated to STG/Bytecode
    (prepd_binds , _) <- corePrepPgm session mod (ms_location ms) unprep_binds data_tycons
    (prepd_binds', _) <- corePrepPgm session mod (ms_location ms) unprep_binds' data_tycons
    let binds  = noUnfoldings $ (map flattenBinds . (:[])) $ prepd_binds
        binds' = noUnfoldings $ (map flattenBinds . (:[])) $ prepd_binds'

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
            let v' = if isOtherUnfolding (realIdUnfolding v) then (setIdUnfolding v noUnfolding) else v
              in setIdOccInfo v' noOccInfo
          else v
        isOtherUnfolding (OtherCon _) = True
        isOtherUnfolding _ = False


    when (not $ null diffs) $
      panicDoc "verify core failed!" (vcat $ punctuate (text "\n\n") (diffs )) -- ++ [ppr binds , ppr binds']))

  pure (diags, Just $! mkHiFileResult ms mod_info (tmrRuntimeModules tcm))

  where
    dflags = hsc_dflags session'
    source = "compile"
    catchErrs x = x `catches`
      [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
      , Handler $ return . (,Nothing) . diagFromString source DsError (noSpan "<internal>")
      . (("Error during " ++ T.unpack source) ++) . show @SomeException
      ]

initPlugins :: HscEnv -> ModSummary -> IO ModSummary
initPlugins session modSummary = do
    session1 <- liftIO $ initializePlugins (hscSetFlags (ms_hspp_opts modSummary) session)
    return modSummary{ms_hspp_opts = hsc_dflags session1}

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
               let session' = tweak (hscSetFlags (ms_hspp_opts ms) session)
               -- TODO: maybe settings ms_hspp_opts is unnecessary?
               -- MP: the flags in ModSummary should be right, if they are wrong then
               -- the correct place to fix this is when the ModSummary is created.
               desugar <- hscDesugar session' (ms { ms_hspp_opts = hsc_dflags session' })  tcg
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
                          newFlags = setBackend target $ updOptLevel 0 $ setOutputFile dot_o $ hsc_dflags env'
                          session' = hscSetFlags newFlags session
#if MIN_VERSION_ghc(9,0,1)
                      (outputFilename, _mStub, _foreign_files, _cinfos) <- hscGenHardCode session' guts
#else
                      (outputFilename, _mStub, _foreign_files) <- hscGenHardCode session' guts
#endif
#if MIN_VERSION_ghc(8,10,0)
                                (ms_location summary)
#else
                                summary
#endif
                                fp
                      compileFile session' StopLn (outputFilename, Just (As False))
              let unlinked = DotO dot_o_fp
              -- Need time to be the modification time for recompilation checking
              t <- liftIO $ getModificationTime dot_o_fp
              let linkable = LM t mod [unlinked]

              pure (map snd warnings, linkable)

data WriteCoreFile = WriteCoreFile | CoreFileExists !UTCTime

generateByteCode :: ShakeExtras -> WriteCoreFile -> HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateByteCode se write_core hscEnv summary guts = do
    fmap (either (, Nothing) (second Just)) $
          catchSrcErrors (hsc_dflags hscEnv) "bytecode" $ do
              (warnings, (_, bytecode, sptEntries)) <-
                withWarnings "bytecode" $ \_tweak -> do
                      let session = _tweak (hscSetFlags (ms_hspp_opts summary) hscEnv)
                          -- TODO: maybe settings ms_hspp_opts is unnecessary?
                          summary' = summary { ms_hspp_opts = hsc_dflags session }
                      hscInteractive session guts
#if MIN_VERSION_ghc(8,10,0)
                                (ms_location summary')
#else
                                summary'
#endif
              let unlinked = BCOs bytecode sptEntries
              time <- case write_core of
                CoreFileExists time -> pure time
                WriteCoreFile -> liftIO $ do
                  let core_fp = ml_core_file $ ms_location summary
                      core_file = codeGutsToCoreFile guts
                  atomicFileWrite se core_fp $ \fp ->
                    writeBinCoreFile fp core_file
                  getModificationTime core_fp
              let linkable = LM time (ms_mod summary) [unlinked]

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

unDefer :: (WarnReason, FileDiagnostic) -> (Bool, FileDiagnostic)
unDefer (Reason Opt_WarnDeferredTypeErrors         , fd) = (True, upgradeWarningToError fd)
unDefer (Reason Opt_WarnTypedHoles                 , fd) = (True, upgradeWarningToError fd)
unDefer (Reason Opt_WarnDeferredOutOfScopeVariables, fd) = (True, upgradeWarningToError fd)
unDefer ( _                                        , fd) = (False, fd)

upgradeWarningToError :: FileDiagnostic -> FileDiagnostic
upgradeWarningToError (nfp, sh, fd) =
  (nfp, sh, fd{_severity = Just DsError, _message = warn2err $ _message fd}) where
  warn2err :: T.Text -> T.Text
  warn2err = T.intercalate ": error:" . T.splitOn ": warning:"

hideDiag :: DynFlags -> (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
hideDiag originalFlags (Reason warning, (nfp, _sh, fd))
  | not (wopt warning originalFlags)
  = (Reason warning, (nfp, HideDiag, fd))
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
#if MIN_VERSION_ghc(8,10,0)
    , Opt_WarnUnusedRecordWildcards
#endif
    , Opt_WarnInaccessibleCode
    , Opt_WarnWarningsDeprecations
    ]

-- | Add a unnecessary/deprecated tag to the required diagnostics.
tagDiag :: (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
tagDiag (Reason warning, (nfp, sh, fd))
  | Just tag <- requiresTag warning
  = (Reason warning, (nfp, sh, fd { _tags = addTag tag (_tags fd) }))
  where
    requiresTag :: WarningFlag -> Maybe DiagnosticTag
    requiresTag Opt_WarnWarningsDeprecations
      = Just DtDeprecated
    requiresTag wflag  -- deprecation was already considered above
      | wflag `elem` unnecessaryDeprecationWarningFlags
      = Just DtUnnecessary
    requiresTag _ = Nothing
    addTag :: DiagnosticTag -> Maybe (List DiagnosticTag) -> Maybe (List DiagnosticTag)
    addTag t Nothing          = Just (List [t])
    addTag t (Just (List ts)) = Just (List (t : ts))
-- other diagnostics are left unaffected
tagDiag t = t

addRelativeImport :: NormalizedFilePath -> ModuleName -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

-- | Also resets the interface store
atomicFileWrite :: ShakeExtras -> FilePath -> (FilePath -> IO a) -> IO ()
atomicFileWrite se targetPath write = do
  let dir = takeDirectory targetPath
  createDirectoryIfMissing True dir
  (tempFilePath, cleanUp) <- newTempFileWithin dir
  (write tempFilePath >> renameFile tempFilePath targetPath >> atomically (resetInterfaceStore se (toNormalizedFilePath' targetPath)))
    `onException` cleanUp

generateHieAsts :: HscEnv -> TcModuleResult -> IO ([FileDiagnostic], Maybe (HieASTs Type))
generateHieAsts hscEnv tcm =
  handleGenerationErrors' dflags "extended interface generation" $ runHsc hscEnv $ do
    -- These varBinds use unitDataConId but it could be anything as the id name is not used
    -- during the hie file generation process. It's a workaround for the fact that the hie modules
    -- don't export an interface which allows for additional information to be added to hie files.
    let fake_splice_binds = Util.listToBag (map (mkVarBind unitDataConId) (spliceExpresions $ tmrTopLevelSplices tcm))
        real_binds = tcg_binds $ tmrTypechecked tcm
#if MIN_VERSION_ghc(9,0,1)
        ts = tmrTypechecked tcm :: TcGblEnv
        top_ev_binds = tcg_ev_binds ts :: Util.Bag EvBind
        insts = tcg_insts ts :: [ClsInst]
        tcs = tcg_tcs ts :: [TyCon]
    run ts $
      Just <$> GHC.enrichHie (fake_splice_binds `Util.unionBags` real_binds) (tmrRenamed tcm) top_ev_binds insts tcs
#else
    Just <$> GHC.enrichHie (fake_splice_binds `Util.unionBags` real_binds) (tmrRenamed tcm)
#endif
  where
    dflags = hsc_dflags hscEnv
    run ts =
#if MIN_VERSION_ghc(9,2,0)
        fmap (join . snd) . liftIO . initDs hscEnv ts
#else
        id
#endif

spliceExpresions :: Splices -> [LHsExpr GhcTc]
spliceExpresions Splices{..} =
    DL.toList $ mconcat
        [ DL.fromList $ map fst exprSplices
        , DL.fromList $ map fst patSplices
        , DL.fromList $ map fst typeSplices
        , DL.fromList $ map fst declSplices
        , DL.fromList $ map fst awSplices
        ]

-- | In addition to indexing the `.hie` file, this function is responsible for
-- maintaining the 'IndexQueue' state and notfiying the user about indexing
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
 IdeOptions{optProgressStyle} <- getIdeOptionsIO se
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
          pending <- readTVar indexPending
          pure $ case HashMap.lookup srcPath pending of
            Nothing          -> False
            -- If the hash in the pending list doesn't match the current hash, then skip
            Just pendingHash -> pendingHash /= hash
        unless newerScheduled $ do
          pre optProgressStyle
          withHieDb (\db -> HieDb.addRefsFromLoaded db targetPath (HieDb.RealFile $ fromNormalizedFilePath srcPath) hash hf')
          post
  where
    mod_location    = ms_location mod_summary
    targetPath      = Compat.ml_hie_file mod_location
    HieDbWriter{..} = hiedbWriter se

    -- Get a progress token to report progress and update it for the current file
    pre style = do
      tok <- modifyVar indexProgressToken $ fmap dupe . \case
        x@(Just _) -> pure x
        -- Create a token if we don't already have one
        Nothing -> do
          case lspEnv se of
            Nothing -> pure Nothing
            Just env -> LSP.runLspT env $ do
              u <- LSP.ProgressTextToken . T.pack . show . hashUnique <$> liftIO Unique.newUnique
              -- TODO: Wait for the progress create response to use the token
              _ <- LSP.sendRequest LSP.SWindowWorkDoneProgressCreate (LSP.WorkDoneProgressCreateParams u) (const $ pure ())
              LSP.sendNotification LSP.SProgress $ LSP.ProgressParams u $
                LSP.Begin $ LSP.WorkDoneProgressBeginParams
                  { _title = "Indexing"
                  , _cancellable = Nothing
                  , _message = Nothing
                  , _percentage = Nothing
                  }
              pure (Just u)

      (!done, !remaining) <- atomically $ do
        done <- readTVar indexCompleted
        remaining <- HashMap.size <$> readTVar indexPending
        pure (done, remaining)
      let
        progressFrac :: Double
        progressFrac = fromIntegral done / fromIntegral (done + remaining)
        progressPct :: LSP.UInt
        progressPct = floor $ 100 * progressFrac

      whenJust (lspEnv se) $ \env -> whenJust tok $ \tok -> LSP.runLspT env $
        LSP.sendNotification LSP.SProgress $ LSP.ProgressParams tok $
          LSP.Report $
            case style of
                Percentage -> LSP.WorkDoneProgressReportParams
                    { _cancellable = Nothing
                    , _message = Nothing
                    , _percentage = Just progressPct
                    }
                Explicit -> LSP.WorkDoneProgressReportParams
                    { _cancellable = Nothing
                    , _message = Just $
                        T.pack " (" <> T.pack (show done) <> "/" <> T.pack (show $ done + remaining) <> ")..."
                    , _percentage = Nothing
                    }
                NoProgress -> LSP.WorkDoneProgressReportParams
                  { _cancellable = Nothing
                  , _message = Nothing
                  , _percentage = Nothing
                  }

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
          LSP.sendNotification (LSP.SCustomMethod "ghcide/reference/ready") $
            toJSON $ fromNormalizedFilePath srcPath
      whenJust mdone $ \done ->
        modifyVar_ indexProgressToken $ \tok -> do
          whenJust (lspEnv se) $ \env -> LSP.runLspT env $
            whenJust tok $ \tok ->
              LSP.sendNotification LSP.SProgress $ LSP.ProgressParams tok $
                LSP.End $ LSP.WorkDoneProgressEndParams
                  { _message = Just $ "Finished indexing " <> T.pack (show done) <> " files"
                  }
          -- We are done with the current indexing cycle, so destroy the token
          pure Nothing

writeAndIndexHieFile :: HscEnv -> ShakeExtras -> ModSummary -> NormalizedFilePath -> [GHC.AvailInfo] -> HieASTs Type -> BS.ByteString -> IO [FileDiagnostic]
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
    modIface = hm_iface $ hirHomeMod tc
    targetPath = ml_hi_file $ ms_location $ hirModSummary tc
    dflags = hsc_dflags hscEnv

handleGenerationErrors :: DynFlags -> T.Text -> IO () -> IO [FileDiagnostic]
handleGenerationErrors dflags source action =
  action >> return [] `catches`
    [ Handler $ return . diagFromGhcException source dflags
    , Handler $ return . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]

handleGenerationErrors' :: DynFlags -> T.Text -> IO (Maybe a) -> IO ([FileDiagnostic], Maybe a)
handleGenerationErrors' dflags source action =
  fmap ([],) action `catches`
    [ Handler $ return . (,Nothing) . diagFromGhcException source dflags
    , Handler $ return . (,Nothing) . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]

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
  let !new_modules = addListToHpt (hsc_HPT e) [(mod_name x, x) | x <- mod_infos]
  in e { hsc_HPT = new_modules
      , hsc_type_env_var = Nothing }
    where
      mod_name = moduleName . mi_module . hm_iface

-- Merge the HPTs, module graphs and FinderCaches
mergeEnvs :: HscEnv -> [ModSummary] -> [HomeModInfo] -> [HscEnv] -> IO HscEnv
mergeEnvs env extraModSummaries extraMods envs = do
    prevFinderCache <- concatFC <$> mapM (readIORef . hsc_FC) envs
    let ims  = map (\ms -> Compat.installedModule (toUnitId $ moduleUnit $ ms_mod ms)  (moduleName (ms_mod ms))) extraModSummaries
        ifrs = zipWith (\ms -> InstalledFound (ms_location ms)) extraModSummaries ims
        -- Very important to force this as otherwise the hsc_mod_graph field is not
        -- forced and ends up retaining a reference to all the old hsc_envs we have merged to get
        -- this new one, which in turn leads to the EPS referencing the HPT.
        module_graph_nodes =
#if MIN_VERSION_ghc(9,2,0)
        -- We don't do any instantiation for backpack at this point of time, so it is OK to use
        -- 'extendModSummaryNoDeps'.
        -- This may have to change in the future.
          map extendModSummaryNoDeps $
#endif
          extraModSummaries ++ nubOrdOn ms_mod (concatMap (mgModSummaries . hsc_mod_graph) envs)

    newFinderCache <- newIORef $
            foldl'
                (\fc (im, ifr) -> Compat.extendInstalledModuleEnv fc im ifr) prevFinderCache
                $ zip ims ifrs
    liftRnf rwhnf module_graph_nodes `seq` (return $ loadModulesHome extraMods $ env{
        hsc_HPT = foldMapBy mergeUDFM emptyUDFM hsc_HPT envs,
        hsc_FC = newFinderCache,
        hsc_mod_graph = mkModuleGraph module_graph_nodes
    })
    where
        mergeUDFM = plusUDFM_C combineModules
        combineModules a b
          | HsSrcFile <- mi_hsc_src (hm_iface a) = a
          | otherwise = b
    -- required because 'FinderCache':
    --  1) doesn't have a 'Monoid' instance,
    --  2) is abstract and doesn't export constructors
    -- To work around this, we coerce to the underlying type
    -- To remove this, I plan to upstream the missing Monoid instance
        concatFC :: [FinderCache] -> FinderCache
        concatFC = unsafeCoerce (mconcat @(Map InstalledModule InstalledFindResult))

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
getModSummaryFromImports env fp modTime contents = do
    (contents, opts, dflags) <- preprocessor env fp contents

    -- The warns will hopefully be reported when we actually parse the module
    (_warns, L main_loc hsmod) <- parseHeader dflags fp contents

    -- Copied from `HeaderInfo.getImports`, but we also need to keep the parsed imports
    let mb_mod = hsmodName hsmod
        imps = hsmodImports hsmod

        mod = fmap unLoc mb_mod `Util.orElse` mAIN_NAME

        (src_idecls, ord_idecls) = partition ((== IsBoot) . ideclSource.unLoc) imps

        -- GHC.Prim doesn't exist physically, so don't go looking for it.
        ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc
                                . ideclName . unLoc)
                               ord_idecls

        implicit_prelude = xopt LangExt.ImplicitPrelude dflags
        implicit_imports = mkPrelImports mod main_loc
                                         implicit_prelude imps

        convImport (L _ i) = (fmap sl_fs (ideclPkgQual i)
                                         , reLoc $ ideclName i)

        srcImports = map convImport src_idecls
        textualImports = map convImport (implicit_imports ++ ordinary_imps)

        msrImports = implicit_imports ++ imps

    -- Force bits that might keep the string buffer and DynFlags alive unnecessarily
    liftIO $ evaluate $ rnf srcImports
    liftIO $ evaluate $ rnf textualImports

    modLoc <- liftIO $ if mod == mAIN_NAME
        -- specially in tests it's common to have lots of nameless modules
        -- mkHomeModLocation will map them to the same hi/hie locations
        then mkHomeModLocation dflags (pathToModuleName fp) fp
        else mkHomeModLocation dflags mod fp

    let modl = mkHomeModule (hscHomeUnit (hscSetFlags dflags env)) mod
        sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile
        msrModSummary =
            ModSummary
                { ms_mod          = modl
#if MIN_VERSION_ghc(8,8,0)
                , ms_hie_date     = Nothing
#endif
                , ms_hs_date      = modTime
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
    return ModSummaryResult{..}
    where
        -- Compute a fingerprint from the contents of `ModSummary`,
        -- eliding the timestamps, the preprocessed source and other non relevant fields
        computeFingerprint opts ModSummary{..} = do
            fingerPrintImports <- fingerprintFromPut $ do
                  put $ Util.uniq $ moduleNameFS $ moduleName ms_mod
                  forM_ (ms_srcimps ++ ms_textual_imps) $ \(mb_p, m) -> do
                    put $ Util.uniq $ moduleNameFS $ unLoc m
                    whenJust mb_p $ put . Util.uniq
            return $! Util.fingerprintFingerprints $
                    [ Util.fingerprintString fp
                    , fingerPrintImports
                    ] ++ map Util.fingerprintString opts


-- | Parse only the module header
parseHeader
       :: Monad m
       => DynFlags -- ^ flags to use
       -> FilePath  -- ^ the filename (for source locations)
       -> Util.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
#if MIN_VERSION_ghc(9,0,1)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule))
#else
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule GhcPs))
#endif
parseHeader dflags filename contents = do
   let loc  = mkRealSrcLoc (Util.mkFastString filename) 1 1
   case unP Compat.parseHeader (initParserState (initParserOpts dflags) contents loc) of
     PFailedWithErrorMessages msgs ->
        throwE $ diagFromErrMsgs "parser" dflags $ msgs dflags
     POk pst rdr_module -> do
        let (warns, errs) = getMessages' pst dflags

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
            throwE $ diagFromErrMsgs "parser" dflags errs

        let warnings = diagFromErrMsgs "parser" dflags warns
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
     PFailedWithErrorMessages msgs -> throwE $ diagFromErrMsgs "parser" dflags $ msgs dflags
     POk pst rdr_module ->
         let
             hpm_annotations = mkApiAnns pst
             (warns, errs) = getMessages' pst dflags
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
                 throwE $ diagFromErrMsgs "parser" dflags errs

               -- Ok, we got here. It's safe to continue.
               let IdePreprocessedSource preproc_warns errs parsed = customPreprocessor rdr_module

               unless (null errs) $
                  throwE $ diagFromStrings "parser" DsError errs

               let preproc_warnings = diagFromStrings "parser" DsWarning preproc_warns
               parsed' <- liftIO $ applyPluginsParsedResultAction env dflags ms hpm_annotations parsed

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
                   srcs0 = nubOrd $ filter (not . (tmpDir dflags `isPrefixOf`))
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

               let pm = ParsedModule ms parsed' srcs2 hpm_annotations
                   warnings = diagFromErrMsgs "parser" dflags warns
               pure (warnings ++ preproc_warnings, pm)

loadHieFile :: Compat.NameCacheUpdater -> FilePath -> IO GHC.HieFile
loadHieFile ncu f = do
  GHC.hie_file_result <$> GHC.readHieFile ncu f


{- Note [Recompilation avoidance in the presence of TH]

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
  sounds bad, but it is possible to make some improvements.
  In particular, we only need to recompile if any of the `Linkable`s actually used during the previous compile change.

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
of the linkables used during a previous compile match whatever is currently
in the HPT.
-}

data RecompilationInfo m
  = RecompilationInfo
  { source_version :: FileVersion
  , old_value   :: Maybe (HiFileResult, FileVersion)
  , get_file_version :: NormalizedFilePath -> m (Maybe FileVersion)
  , regenerate  :: Maybe LinkableType -> m ([FileDiagnostic], Maybe HiFileResult) -- ^ Action to regenerate an interface
  }

-- | Either a regular GHC linkable or a core file that
-- can be later turned into a proper linkable
data IdeLinkable = GhcLinkable !Linkable | CoreLinkable !UTCTime !CoreFile

instance NFData IdeLinkable where
  rnf (GhcLinkable lb) = rnf lb
  rnf (CoreLinkable time _) = rnf time

ml_core_file :: ModLocation -> FilePath
ml_core_file ml = ml_hi_file ml <.> "core"

-- | Retuns an up-to-date module interface, regenerating if needed.
--   Assumes file exists.
--   Requires the 'HscEnv' to be set up with dependencies
-- See Note [Recompilation avoidance in the presence of TH]
loadInterface
  :: (MonadIO m, MonadMask m)
  => ShakeExtras
  -> HscEnv
  -> ModSummary
  -> Maybe LinkableType
  -> RecompilationInfo m
  -> m ([FileDiagnostic], Maybe HiFileResult)
loadInterface se session ms linkableNeeded RecompilationInfo{..} = do
    let sessionWithMsDynFlags = hscSetFlags (ms_hspp_opts ms) session
        mb_old_iface = hm_iface    . hirHomeMod . fst <$> old_value
        mb_old_version = snd <$> old_value

        obj_file = ml_obj_file (ms_location ms)
        core_file = ml_core_file (ms_location ms)
        iface_file = ml_hi_file (ms_location ms)

        !mod = ms_mod ms

    mb_dest_version <- case mb_old_version of
      Just ver -> pure $ Just ver
      Nothing ->  do
        let file = case linkableNeeded of
              Just ObjectLinkable -> obj_file
              Just BCOLinkable    -> core_file
              Nothing             -> iface_file
        get_file_version (toNormalizedFilePath' file)

    -- The source is modified if it is newer than the destination
    let sourceMod = case mb_dest_version of
          Nothing -> SourceModified -- desitination file doesn't exist, assume modified source
          Just dest_version
            | source_version <= dest_version -> SourceUnmodified
            | otherwise -> SourceModified

    -- If mb_old_iface is nothing then checkOldIface will load it for us
    (recomp_iface_reqd, mb_checked_iface)
      <- liftIO $ checkOldIface sessionWithMsDynFlags ms sourceMod mb_old_iface


    (recomp_obj_reqd, mb_linkable) <- case linkableNeeded of
      Nothing -> pure (UpToDate, Nothing)
      Just linkableType -> case old_value of
        -- We don't have an old result
        Nothing -> recompMaybeBecause "missing"
        -- We have an old result
        Just (old_hir, old_file_version) ->
          case hm_linkable $ hirHomeMod old_hir of
            Nothing -> recompMaybeBecause "missing [not needed before]"
            Just old_lb
              | Just True <- mi_used_th <$> mb_checked_iface -- No need to recompile if TH wasn't used
              , old_file_version /= source_version -> recompMaybeBecause "out of date"

              -- Check if it is the correct type
              -- Ideally we could use object-code in case we already have
              -- it when we are generating bytecode, but this is difficult because something
              -- below us may be bytecode, and object code can't depend on bytecode
              | ObjectLinkable <- linkableType, isObjectLinkable old_lb
              -> pure (UpToDate, Just $ GhcLinkable old_lb)

              | BCOLinkable    <- linkableType , not (isObjectLinkable old_lb)
              -> pure (UpToDate, Just $ GhcLinkable old_lb)

              | otherwise -> recompMaybeBecause "missing [wrong type]"
        where
          recompMaybeBecause msg =
            case mb_dest_version of -- The destination file should be the object code or the core file
              Nothing -> pure (RecompBecause msg', Nothing)
              Just disk_obj_version@(ModificationTime t) ->
                if (disk_obj_version >= source_version)
                then case linkableType of
                  ObjectLinkable -> pure (UpToDate, Just $ GhcLinkable $ LM (posixSecondsToUTCTime t) mod [DotO obj_file])
                  BCOLinkable -> liftIO $ do
                    core <- readBinCoreFile (mkUpdater $ hsc_NC session)  core_file
                    pure (UpToDate, Just $ CoreLinkable (posixSecondsToUTCTime t) core)
                else pure (RecompBecause msg', Nothing)
              Just (VFSVersion _) -> pure (RecompBecause msg', Nothing)
           where
             msg' = case linkableType of
               BCOLinkable -> "bytecode " ++ msg
               ObjectLinkable -> "Object code " ++ msg

    let do_regenerate _reason = withTrace "regenerate interface" $ \setTag -> do
          setTag "Module" $ moduleNameString $ moduleName mod
          setTag "Reason" $ showReason _reason
          liftIO $ traceMarkerIO $ "regenerate interface " ++ show (moduleNameString $ moduleName mod, showReason _reason)
          regenerate linkableNeeded

    case (mb_checked_iface, recomp_iface_reqd <> recomp_obj_reqd) of
      (Just iface, UpToDate) -> do
         -- Force it because we don't want to retain old modsummaries or linkables
         lb <- liftIO $ evaluate $ force mb_linkable

         -- If we have an old value, just return it
         case old_value of
           Just (old_hir, _)
             | Just msg <- checkLinkableDependencies (hsc_HPT sessionWithMsDynFlags) (hirRuntimeModules old_hir)
             -> do_regenerate msg
             | otherwise -> return ([], Just old_hir)
           Nothing -> do
             (warns, hmi) <- liftIO $ mkDetailsFromIface se sessionWithMsDynFlags ms iface lb
             -- parse the runtime dependencies from the annotations
             let runtime_deps
                   | not (mi_used_th iface) = emptyModuleEnv
                   | otherwise = parseRuntimeDeps (md_anns (hm_details hmi))
             return (warns, Just $ mkHiFileResult ms hmi runtime_deps)
      (_, _reason) -> do_regenerate _reason

-- | ModDepTime is stored as an annotation in the iface to
-- keep track of runtime dependencies
newtype ModDepTime = ModDepTime UTCTime

deserializeModDepTime :: [Word8] -> ModDepTime
deserializeModDepTime xs = ModDepTime $ case decode (LBS.pack xs) of
  (a,b) -> UTCTime (toEnum a) (toEnum b)

serializeModDepTime :: ModDepTime -> [Word8]
serializeModDepTime (ModDepTime l) = LBS.unpack $
  B.encode (fromEnum $ utctDay l, fromEnum $ utctDayTime l)

-- | Find the runtime dependencies by looking at the annotations
-- serialized in the iface
parseRuntimeDeps :: [ModIfaceAnnotation] -> ModuleEnv UTCTime
parseRuntimeDeps anns = mkModuleEnv $ mapMaybe go anns
  where
    go (Annotation (ModuleTarget mod) payload)
      | Just (ModDepTime t) <- fromSerialized deserializeModDepTime payload
      = Just (mod, t)
    go _ = Nothing

-- | checkLinkableDependencies compares the linkables in the home package to
-- the runtime dependencies of the module, to check if any of them are out of date
-- Hopefully 'runtime_deps' will be empty if the module didn't actually use TH
-- See Note [Recompilation avoidance in the presence of TH]
checkLinkableDependencies :: HomePackageTable -> ModuleEnv UTCTime -> Maybe RecompileRequired
checkLinkableDependencies hpt runtime_deps
  | isEmptyModuleEnv out_of_date = Nothing -- Nothing out of date, so don't recompile
  | otherwise = Just $
      RecompBecause $ "out of date runtime dependencies: " ++ intercalate ", " (map show (moduleEnvKeys out_of_date))
  where
    out_of_date = filterModuleEnv (\mod time -> case lookupHpt hpt (moduleName mod) of
                                                  Nothing -> False
                                                  Just hm -> case hm_linkable hm of
                                                    Nothing -> False
                                                    Just lm -> linkableTime lm /= time)
                                  runtime_deps

showReason :: RecompileRequired -> String
showReason UpToDate          = "UpToDate"
showReason MustCompile       = "MustCompile"
showReason (RecompBecause s) = s

mkDetailsFromIface :: ShakeExtras -> HscEnv -> ModSummary -> ModIface -> Maybe IdeLinkable -> IO ([FileDiagnostic], HomeModInfo)
mkDetailsFromIface se session ms iface ide_linkable = do
  details <- liftIO $ fixIO $ \details -> do
    let hsc' = session { hsc_HPT = addToHpt (hsc_HPT session) (moduleName $ mi_module iface) (HomeModInfo iface details Nothing) }
    initIfaceLoad hsc' (typecheckIface iface)
  (warns, linkable) <- liftIO $ case ide_linkable of
    Nothing -> pure ([], Nothing)
    Just (GhcLinkable lb) -> pure ([], Just lb)
    Just (CoreLinkable t core_file) -> do
      cgi_guts <- coreFileToCgGuts session iface details core_file
      generateByteCode se (CoreFileExists t) session ms cgi_guts

  return (warns, HomeModInfo iface details linkable)

coreFileToCgGuts :: HscEnv -> ModIface -> ModDetails -> CoreFile -> IO CgGuts
coreFileToCgGuts session iface details core_file = do
  let act hpt = addToHpt hpt (moduleName this_mod)
                             (HomeModInfo iface details Nothing)
      this_mod = mi_module iface
  types_var <- newIORef (md_types details)
  let kv = Just (this_mod, types_var)
      hsc_env' = session { hsc_HPT = act (hsc_HPT session)
                         , hsc_type_env_var = kv }
  core_binds <- initIfaceCheck (text "l") hsc_env' $ typecheckCoreFile this_mod types_var core_file
      -- Implicit binds aren't saved, so we need to regenerate them ourselves.
  let implicit_binds = concatMap getImplicitBinds tyCons
      tyCons = typeEnvTyCons (md_types details)
  pure $ CgGuts this_mod tyCons (implicit_binds ++ core_binds) NoStubs [] [] (emptyHpcInfo False) Nothing []

-- | Non-interactive, batch version of 'InteractiveEval.getDocs'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
getDocsBatch
  :: HscEnv
  -> Module  -- ^ a moudle where the names are in scope
  -> [Name]
  -> IO [Either String (Maybe HsDocString, IntMap HsDocString)]
getDocsBatch hsc_env _mod _names = do
    (msgs, res) <- initTc hsc_env HsSrcFile False _mod fakeSpan $ forM _names $ \name ->
        case nameModule_maybe name of
            Nothing -> return (Left $ NameHasNoModule name)
            Just mod -> do
             ModIface { mi_doc_hdr = mb_doc_hdr
                      , mi_decl_docs = DeclDocMap dmap
                      , mi_arg_docs = ArgDocMap amap
                      } <- loadModuleInterface "getModuleInterface" mod
             if isNothing mb_doc_hdr && Map.null dmap && null amap
               then pure (Left (NoDocsInIface mod $ compiled name))
               else pure (Right ( Map.lookup name dmap ,
#if !MIN_VERSION_ghc(9,2,0)
                                  IntMap.fromAscList $ Map.toAscList $
#endif
                                  Map.findWithDefault mempty name amap))
    case res of
        Just x  -> return $ map (first $ T.unpack . printOutputable) x
        Nothing -> throwErrors
#if MIN_VERSION_ghc(9,2,0)
                     $ Error.getErrorMessages msgs
#else
                     $ snd msgs
#endif
  where
    throwErrors = liftIO . throwIO . mkSrcErr
    compiled n =
      -- TODO: Find a more direct indicator.
      case nameSrcLoc n of
        RealSrcLoc {}   -> False
        UnhelpfulLoc {} -> True

fakeSpan :: RealSrcSpan
fakeSpan = realSrcLocSpan $ mkRealSrcLoc (Util.fsLit "<ghcide>") 1 1

-- | Non-interactive, batch version of 'InteractiveEval.lookupNames'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
lookupName :: HscEnv
           -> Module -- ^ A module where the Names are in scope
           -> Name
           -> IO (Maybe TyThing)
lookupName hsc_env mod name = do
    (_messages, res) <- initTc hsc_env HsSrcFile False mod fakeSpan $ do
        tcthing <- tcLookup name
        case tcthing of
            AGlobal thing    -> return thing
            ATcId{tct_id=id} -> return (AnId id)
            _                -> panic "tcRnLookupName'"
    return res


pathToModuleName :: FilePath -> ModuleName
pathToModuleName = mkModuleName . map rep
  where
      rep c | isPathSeparator c = '_'
      rep ':' = '_'
      rep c = c
