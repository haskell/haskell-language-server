-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

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
  , mkTcModuleResult
  , generateByteCode
  , loadHieFile
  ) where

import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Preprocessor
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.GHC.Warnings
import Development.IDE.Types.Diagnostics
import Development.IDE.GHC.Orphans()
import Development.IDE.GHC.Util
import qualified GHC.LanguageExtensions.Type as GHC
import Development.IDE.Types.Options
import Development.IDE.Types.Location

#if MIN_GHC_API_VERSION(8,6,0)
import           DynamicLoading (initializePlugins)
#endif

import           GHC hiding (parseModule, typecheckModule)
import qualified Parser
import           Lexer
#if MIN_GHC_API_VERSION(8,10,0)
#else
import ErrUtils
#endif

import           Finder
import qualified Development.IDE.GHC.Compat     as GHC
import           GhcMonad
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified HeaderInfo                     as Hdr
import           HscMain                        (hscInteractive, hscSimplify)
import           MkIface
import           NameCache
import           StringBuffer                   as SB
import           TcRnMonad (tcg_th_coreplugins)
import           TidyPgm

import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Trans.Except
import           Data.Function
import           Data.Ord
import qualified Data.Text as T
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import           Data.Tuple.Extra
import qualified Data.Map.Strict                          as Map
import           System.FilePath


-- | Given a string buffer, return the string (after preprocessing) and the 'ParsedModule'.
parseModule
    :: IdeOptions
    -> HscEnv
    -> FilePath
    -> Maybe SB.StringBuffer
    -> IO (IdeResult (StringBuffer, ParsedModule))
parseModule IdeOptions{..} env filename mbContents =
    fmap (either (, Nothing) id) $
    runGhcEnv env $ runExceptT $ do
        (contents, dflags) <- preprocessor filename mbContents
        (diag, modu) <- parseFileContents optPreprocessor dflags filename contents
        return (diag, Just (contents, modu))


-- | Given a package identifier, what packages does it depend on
computePackageDeps
    :: HscEnv
    -> InstalledUnitId
    -> IO (Either [FileDiagnostic] [InstalledUnitId])
computePackageDeps env pkg = do
    let dflags = hsc_dflags env
    case lookupInstalledPackage dflags pkg of
        Nothing -> return $ Left [ideErrorText (toNormalizedFilePath' noFilePath) $
            T.pack $ "unknown package: " ++ show pkg]
        Just pkgInfo -> return $ Right $ depends pkgInfo


-- | Typecheck a single module using the supplied dependencies and packages.
typecheckModule
    :: IdeDefer
    -> HscEnv
    -> [TcModuleResult]
    -> ParsedModule
    -> IO (IdeResult TcModuleResult)
typecheckModule (IdeDefer defer) packageState deps pm =
    let demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id
    in
    fmap (either (, Nothing) (second Just)) $
    runGhcEnv packageState $
        catchSrcErrors "typecheck" $ do
            setupEnv deps
            let modSummary = pm_mod_summary pm
                dflags = ms_hspp_opts modSummary
            modSummary' <- initPlugins modSummary
            (warnings, tcm) <- withWarnings "typecheck" $ \tweak ->
                GHC.typecheckModule $ enableTopLevelWarnings
                                    $ demoteIfDefer pm{pm_mod_summary = tweak modSummary'}
            tcm2 <- mkTcModuleResult tcm
            let errorPipeline = unDefer . hideDiag dflags
            return (map errorPipeline warnings, tcm2)

initPlugins :: GhcMonad m => ModSummary -> m ModSummary
initPlugins modSummary = do
#if MIN_GHC_API_VERSION(8,6,0)
    session <- getSession
    dflags <- liftIO $ initializePlugins session (ms_hspp_opts modSummary)
    return modSummary{ms_hspp_opts = dflags}
#else
    return modSummary
#endif

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
    -> [TcModuleResult]
    -> TcModuleResult
    -> IO (IdeResult (SafeHaskellMode, CgGuts, ModDetails))
compileModule (RunSimplifier simplify) packageState deps tmr =
    fmap (either (, Nothing) (second Just)) $
    runGhcEnv packageState $
        catchSrcErrors "compile" $ do
            setupEnv (deps ++ [tmr])

            let tm = tmrModule tmr
            session <- getSession
            (warnings,desugar) <- withWarnings "compile" $ \tweak -> do
                let pm = tm_parsed_module tm
                let pm' = pm{pm_mod_summary = tweak $ pm_mod_summary pm}
                let tm' = tm{tm_parsed_module  = pm'}
                GHC.dm_core_module <$> GHC.desugarModule tm'
            let tc_result = fst (tm_internals_ (tmrModule tmr))
            desugared_guts <-
                if simplify
                    then do
                        plugins <- liftIO $ readIORef (tcg_th_coreplugins tc_result)
                        liftIO $ hscSimplify session plugins desugar
                    else pure desugar
            -- give variables unique OccNames
            (guts, details) <- liftIO $ tidyProgram session desugared_guts
            return (map snd warnings, (mg_safe_haskell desugar, guts, details))

generateByteCode :: HscEnv -> [TcModuleResult] -> TcModuleResult -> CgGuts -> IO (IdeResult Linkable)
generateByteCode hscEnv deps tmr guts =
    fmap (either (, Nothing) (second Just)) $
    runGhcEnv hscEnv $
      catchSrcErrors "bytecode" $ do
          setupEnv (deps ++ [tmr])
          session <- getSession
          (warnings, (_, bytecode, sptEntries)) <- withWarnings "bytecode" $ \tweak ->
#if MIN_GHC_API_VERSION(8,10,0)
                liftIO $ hscInteractive session guts (GHC.ms_location $ tweak $ GHC.pm_mod_summary $ GHC.tm_parsed_module $ tmrModule tmr)
#else
                liftIO $ hscInteractive session guts (tweak $ GHC.pm_mod_summary $ GHC.tm_parsed_module $ tmrModule tmr)
#endif
          let summary = pm_mod_summary $ tm_parsed_module $ tmrModule tmr
          let unlinked = BCOs bytecode sptEntries
          let linkable = LM (ms_hs_date summary) (ms_mod summary) [unlinked]
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

enableTopLevelWarnings :: ParsedModule -> ParsedModule
enableTopLevelWarnings =
  (update_pm_mod_summary . update_hspp_opts)
  ((`wopt_set` Opt_WarnMissingPatternSynonymSignatures) .
   (`wopt_set` Opt_WarnMissingSignatures))
  -- the line below would show also warnings for let bindings without signature
  -- ((`wopt_set` Opt_WarnMissingSignatures) . (`wopt_set` Opt_WarnMissingLocalSignatures)))

update_hspp_opts :: (DynFlags -> DynFlags) -> ModSummary -> ModSummary
update_hspp_opts up ms = ms{ms_hspp_opts = up $ ms_hspp_opts ms}

update_pm_mod_summary :: (ModSummary -> ModSummary) -> ParsedModule -> ParsedModule
update_pm_mod_summary up pm =
  pm{pm_mod_summary = up $ pm_mod_summary pm}

unDefer :: (WarnReason, FileDiagnostic) -> FileDiagnostic
unDefer (Reason Opt_WarnDeferredTypeErrors         , fd) = upgradeWarningToError fd
unDefer (Reason Opt_WarnTypedHoles                 , fd) = upgradeWarningToError fd
unDefer (Reason Opt_WarnDeferredOutOfScopeVariables, fd) = upgradeWarningToError fd
unDefer ( _                                        , fd) = fd

upgradeWarningToError :: FileDiagnostic -> FileDiagnostic
upgradeWarningToError (nfp, sh, fd) =
  (nfp, sh, fd{_severity = Just DsError, _message = warn2err $ _message fd}) where
  warn2err :: T.Text -> T.Text
  warn2err = T.intercalate ": error:" . T.splitOn ": warning:"

hideDiag :: DynFlags -> (WarnReason, FileDiagnostic) -> (WarnReason, FileDiagnostic)
hideDiag originalFlags (Reason warning, (nfp, _sh, fd))
  | not (wopt warning originalFlags) = (Reason warning, (nfp, HideDiag, fd))
hideDiag _originalFlags t = t

addRelativeImport :: NormalizedFilePath -> ParsedModule -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

mkTcModuleResult
    :: GhcMonad m
    => TypecheckedModule
    -> m TcModuleResult
mkTcModuleResult tcm = do
    session <- getSession
    let sf = modInfoSafe (tm_checked_module_info tcm)
#if MIN_GHC_API_VERSION(8,10,0)
    iface <- liftIO $ mkIfaceTc session sf details tcGblEnv
#else
    (iface, _) <- liftIO $ mkIfaceTc session Nothing sf details tcGblEnv
#endif
    let mod_info = HomeModInfo iface details Nothing
    return $ TcModuleResult tcm mod_info
  where
    (tcGblEnv, details) = tm_internals_ tcm

-- | Setup the environment that GHC needs according to our
-- best understanding (!)
setupEnv :: GhcMonad m => [TcModuleResult] -> m ()
setupEnv tmsIn = do
    -- if both a .hs-boot file and a .hs file appear here, we want to make sure that the .hs file
    -- takes precedence, so put the .hs-boot file earlier in the list
    let isSourceFile = (==HsBootFile) . ms_hsc_src . pm_mod_summary . tm_parsed_module . tmrModule
        tms = sortBy (compare `on` Down . isSourceFile) tmsIn

    session <- getSession

    let mss = map (pm_mod_summary . tm_parsed_module . tmrModule) tms

    -- set the target and module graph in the session
    let graph = mkModuleGraph mss
    setSession session { hsc_mod_graph = graph }

    -- Make modules available for others that import them,
    -- by putting them in the finder cache.
    let ims  = map (InstalledModule (thisInstalledUnitId $ hsc_dflags session) . moduleName . ms_mod) mss
        ifrs = zipWith (\ms -> InstalledFound (ms_location ms)) mss ims
    -- We have to create a new IORef here instead of modifying the existing IORef as
    -- it is shared between concurrent compilations.
    prevFinderCache <- liftIO $ readIORef $ hsc_FC session
    let newFinderCache =
            foldl'
                (\fc (im, ifr) -> GHC.extendInstalledModuleEnv fc im ifr) prevFinderCache
                $ zip ims ifrs
    newFinderCacheVar <- liftIO $ newIORef $! newFinderCache
    modifySession $ \s -> s { hsc_FC = newFinderCacheVar }

    -- load dependent modules, which must be in topological order.
    mapM_ loadModuleHome tms


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



-- | GhcMonad function to chase imports of a module given as a StringBuffer. Returns given module's
-- name and its imports.
getImportsParsed ::  DynFlags ->
               GHC.ParsedSource ->
               Either [FileDiagnostic] (GHC.ModuleName, [(Bool, (Maybe FastString, Located GHC.ModuleName))])
getImportsParsed dflags (L loc parsed) = do
  let modName = maybe (GHC.mkModuleName "Main") GHC.unLoc $ GHC.hsmodName parsed

  -- most of these corner cases are also present in https://hackage.haskell.org/package/ghc-8.6.1/docs/src/HeaderInfo.html#getImports
  -- but we want to avoid parsing the module twice
  let implicit_prelude = xopt GHC.ImplicitPrelude dflags
      implicit_imports = Hdr.mkPrelImports modName loc implicit_prelude $ GHC.hsmodImports parsed

  -- filter out imports that come from packages
  return (modName, [(ideclSource i, (fmap sl_fs $ ideclPkgQual i, ideclName i))
    | i <- map GHC.unLoc $ implicit_imports ++ GHC.hsmodImports parsed
    , GHC.moduleNameString (GHC.unLoc $ ideclName i) /= "GHC.Prim"
    ])


-- | Produce a module summary from a StringBuffer.
getModSummaryFromBuffer
    :: GhcMonad m
    => FilePath
    -> SB.StringBuffer
    -> DynFlags
    -> GHC.ParsedSource
    -> ExceptT [FileDiagnostic] m ModSummary
getModSummaryFromBuffer fp contents dflags parsed = do
  (modName, imports) <- liftEither $ getImportsParsed dflags parsed

  modLoc <- liftIO $ mkHomeModLocation dflags modName fp
  let InstalledUnitId unitId = thisInstalledUnitId dflags
  return $ ModSummary
    { ms_mod          = mkModule (fsToUnitId unitId) modName
    , ms_location     = modLoc
    , ms_hs_date      = error "Rules should not depend on ms_hs_date"
        -- When we are working with a virtual file we do not have a file date.
        -- To avoid silent issues where something is not processed because the date
        -- has not changed, we make sure that things blow up if they depend on the
        -- date.
    , ms_textual_imps = [imp | (False, imp) <- imports]
    , ms_hspp_file    = fp
    , ms_hspp_opts    = dflags
    , ms_hspp_buf     = Just contents

    -- defaults:
    , ms_hsc_src      = sourceType
    , ms_obj_date     = Nothing
    , ms_iface_date   = Nothing
#if MIN_GHC_API_VERSION(8,8,0)
    , ms_hie_date     = Nothing
#endif
    , ms_srcimps      = [imp | (True, imp) <- imports]
    , ms_parsed_mod   = Nothing
    }
    where
      sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile


-- | Given a buffer, flags, file path and module summary, produce a
-- parsed module (or errors) and any parse warnings. Does not run any preprocessors
parseFileContents
       :: GhcMonad m
       => (GHC.ParsedSource -> IdePreprocessedSource)
       -> DynFlags -- ^ flags to use
       -> FilePath  -- ^ the filename (for source locations)
       -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], ParsedModule)
parseFileContents customPreprocessor dflags filename contents = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   case unP Parser.parseModule (mkPState dflags contents loc) of
#if MIN_GHC_API_VERSION(8,10,0)
     PFailed pst -> throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
#else
     PFailed _ locErr msgErr ->
      throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
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
                 throwE $ diagFromErrMsgs "parser" dflags $ snd $ getMessages pst dflags

               -- Ok, we got here. It's safe to continue.
               let IdePreprocessedSource preproc_warns errs parsed = customPreprocessor rdr_module
               unless (null errs) $ throwE $ diagFromStrings "parser" DsError errs
               let preproc_warnings = diagFromStrings "parser" DsWarning preproc_warns
               ms <- getModSummaryFromBuffer filename contents dflags parsed
               let pm =
                     ParsedModule {
                         pm_mod_summary = ms
                       , pm_parsed_source = parsed
                       , pm_extra_src_files=[] -- src imports not allowed
                       , pm_annotations = hpm_annotations
                      }
                   warnings = diagFromErrMsgs "parser" dflags warns
               pure (warnings ++ preproc_warnings, pm)

loadHieFile :: FilePath -> IO GHC.HieFile
loadHieFile f = do
        u <- mkSplitUniqSupply 'a'
        let nameCache = initNameCache u []
        fmap (GHC.hie_file_result . fst) $ GHC.readHieFile nameCache f
