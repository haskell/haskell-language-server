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
  , parseHeader
  , typecheckModule
  , computePackageDeps
  , addRelativeImport
  , mkTcModuleResult
  , generateByteCode
  , generateAndWriteHieFile
  , writeHiFile
  , getModSummaryFromImports
  , loadHieFile
  , loadInterface
  , loadDepModule
  , loadModuleHome
  , setupFinderCache
  , getDocsBatch
  , lookupName
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
import DynamicLoading (initializePlugins)
import LoadIface (loadModuleInterface)
#endif

import qualified Parser
import           Lexer
#if MIN_GHC_API_VERSION(8,10,0)
#else
import ErrUtils
#endif

import           Finder
import           Development.IDE.GHC.Compat hiding (parseModule, typecheckModule)
import qualified Development.IDE.GHC.Compat     as GHC
import qualified Development.IDE.GHC.Compat     as Compat
import           GhcMonad
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified HeaderInfo                     as Hdr
import           HscMain                        (hscInteractive, hscSimplify)
import           MkIface
import           StringBuffer                   as SB
import           TcRnMonad (tct_id, TcTyThing(AGlobal, ATcId), initTc, initIfaceLoad, tcg_th_coreplugins)
import           TcIface                        (typecheckIface)
import           TidyPgm

import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Bifunctor                           (first, second)
import qualified Data.Text as T
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Map.Strict                          as Map
import           System.FilePath
import           System.Directory
import           System.IO.Extra
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Exception (ExceptionMonad)
import TcEnv (tcLookup)
import Data.Time (UTCTime)


-- | Given a string buffer, return the string (after preprocessing) and the 'ParsedModule'.
parseModule
    :: IdeOptions
    -> HscEnv
    -> [PackageName]
    -> FilePath
    -> UTCTime
    -> Maybe SB.StringBuffer
    -> IO (IdeResult (StringBuffer, ParsedModule))
parseModule IdeOptions{..} env comp_pkgs filename modTime mbContents =
    fmap (either (, Nothing) id) $
    evalGhcEnv env $ runExceptT $ do
        (contents, dflags) <- preprocessor filename mbContents
        (diag, modu) <- parseFileContents optPreprocessor dflags comp_pkgs filename modTime contents
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

typecheckModule :: IdeDefer
                -> HscEnv
                -> ParsedModule
                -> IO (IdeResult (HscEnv, TcModuleResult))
typecheckModule (IdeDefer defer) hsc pm = do
    fmap (either (, Nothing) (second Just . sequence) . sequence) $
      runGhcEnv hsc $
      catchSrcErrors "typecheck" $ do

        let modSummary = pm_mod_summary pm
            dflags = ms_hspp_opts modSummary

        modSummary' <- initPlugins modSummary
        (warnings, tcm) <- withWarnings "typecheck" $ \tweak ->
            GHC.typecheckModule $ enableTopLevelWarnings
                                $ demoteIfDefer pm{pm_mod_summary = tweak modSummary'}
        let errorPipeline = unDefer . hideDiag dflags
            diags = map errorPipeline warnings
        tcm2 <- mkTcModuleResult tcm (any fst diags)
        return (map snd diags, tcm2)
    where
        demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id

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
    -> [(ModSummary, HomeModInfo)]
    -> TcModuleResult
    -> IO (IdeResult (SafeHaskellMode, CgGuts, ModDetails))
compileModule (RunSimplifier simplify) packageState deps tmr =
    fmap (either (, Nothing) (second Just)) $
    evalGhcEnv packageState $
        catchSrcErrors "compile" $ do
            setupEnv (deps ++ [(tmrModSummary tmr, tmrModInfo tmr)])

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

generateByteCode :: HscEnv -> [(ModSummary, HomeModInfo)] -> TcModuleResult -> CgGuts -> IO (IdeResult Linkable)
generateByteCode hscEnv deps tmr guts =
    fmap (either (, Nothing) (second Just)) $
    evalGhcEnv hscEnv $
      catchSrcErrors "bytecode" $ do
          setupEnv (deps ++ [(tmrModSummary tmr, tmrModInfo tmr)])
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
  | not (wopt warning originalFlags) = (Reason warning, (nfp, HideDiag, fd))
hideDiag _originalFlags t = t

addRelativeImport :: NormalizedFilePath -> ModuleName -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

mkTcModuleResult
    :: GhcMonad m
    => TypecheckedModule
    -> Bool
    -> m TcModuleResult
mkTcModuleResult tcm upgradedError = do
    session <- getSession
    let sf = modInfoSafe (tm_checked_module_info tcm)
#if MIN_GHC_API_VERSION(8,10,0)
    iface <- liftIO $ mkIfaceTc session sf details tcGblEnv
#else
    (iface, _) <- liftIO $ mkIfaceTc session Nothing sf details tcGblEnv
#endif
    let mod_info = HomeModInfo iface details Nothing
    return $ TcModuleResult tcm mod_info upgradedError
  where
    (tcGblEnv, details) = tm_internals_ tcm

atomicFileWrite :: FilePath -> (FilePath -> IO a) -> IO ()
atomicFileWrite targetPath write = do
  let dir = takeDirectory targetPath
  createDirectoryIfMissing True dir
  (tempFilePath, cleanUp) <- newTempFileWithin dir
  (write tempFilePath >> renameFile tempFilePath targetPath) `onException` cleanUp

generateAndWriteHieFile :: HscEnv -> TypecheckedModule -> IO [FileDiagnostic]
generateAndWriteHieFile hscEnv tcm =
  handleGenerationErrors dflags "extended interface generation" $ do
    case tm_renamed_source tcm of
      Just rnsrc -> do
        hf <- runHsc hscEnv $
          GHC.mkHieFile mod_summary (fst $ tm_internals_ tcm) rnsrc ""
        atomicFileWrite targetPath $ flip GHC.writeHieFile hf
      _ ->
        return ()
  where
    dflags       = hsc_dflags hscEnv
    mod_summary  = pm_mod_summary $ tm_parsed_module tcm
    mod_location = ms_location mod_summary
    targetPath   = Compat.ml_hie_file mod_location

writeHiFile :: HscEnv -> TcModuleResult -> IO [FileDiagnostic]
writeHiFile hscEnv tc =
  handleGenerationErrors dflags "interface generation" $ do
    atomicFileWrite targetPath $ \fp ->
      writeIfaceFile dflags fp modIface
  where
    modIface = hm_iface $ tmrModInfo tc
    modSummary = tmrModSummary tc
    targetPath = withBootSuffix $ ml_hi_file $ ms_location $ tmrModSummary tc
    withBootSuffix = case ms_hsc_src modSummary of
                HsBootFile -> addBootSuffix
                _ -> id
    dflags = hsc_dflags hscEnv

handleGenerationErrors :: DynFlags -> T.Text -> IO () -> IO [FileDiagnostic]
handleGenerationErrors dflags source action =
  action >> return [] `catches`
    [ Handler $ return . diagFromGhcException source dflags
    , Handler $ return . diagFromString source DsError (noSpan "<internal>")
    . (("Error during " ++ T.unpack source) ++) . show @SomeException
    ]


-- | Setup the environment that GHC needs according to our
-- best understanding (!)
--
-- This involves setting up the finder cache and populating the
-- HPT.
setupEnv :: GhcMonad m => [(ModSummary, HomeModInfo)] -> m ()
setupEnv tms = do
    setupFinderCache (map fst tms)
    -- load dependent modules, which must be in topological order.
    modifySession $ \e ->
      foldl' (\e (_, hmi) -> loadModuleHome hmi e) e tms

-- | Initialise the finder cache, dependencies should be topologically
-- sorted.
setupFinderCache :: GhcMonad m => [ModSummary] -> m ()
setupFinderCache mss = do
    session <- getSession

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


-- | Load a module, quickly. Input doesn't need to be desugared.
-- A module must be loaded before dependent modules can be typechecked.
-- This variant of loadModuleHome will *never* cause recompilation, it just
-- modifies the session.
--
-- The order modules are loaded is important when there are hs-boot files.
-- In particular you should make sure to load the .hs version of a file after the
-- .hs-boot version.
loadModuleHome
    :: HomeModInfo
    -> HscEnv
    -> HscEnv
loadModuleHome mod_info e =
    e { hsc_HPT = addToHpt (hsc_HPT e) mod_name mod_info }
    where
      mod_name = moduleName $ mi_module $ hm_iface mod_info

-- | Load module interface.
loadDepModuleIO :: ModIface -> Maybe Linkable -> HscEnv -> IO HscEnv
loadDepModuleIO iface linkable hsc = do
    details <- liftIO $ fixIO $ \details -> do
        let hsc' = hsc { hsc_HPT = addToHpt (hsc_HPT hsc) mod (HomeModInfo iface details linkable) }
        initIfaceLoad hsc' (typecheckIface iface)
    let mod_info = HomeModInfo iface details linkable
    return $ loadModuleHome mod_info hsc
    where
      mod = moduleName $ mi_module iface

loadDepModule :: GhcMonad m => ModIface -> Maybe Linkable -> m ()
loadDepModule iface linkable = do
  e <- getSession
  e' <- liftIO $ loadDepModuleIO iface linkable e
  setSession e'

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
    -> UTCTime
    -> DynFlags
    -> GHC.ParsedSource
    -> StringBuffer
    -> ExceptT [FileDiagnostic] m ModSummary
getModSummaryFromBuffer fp modTime dflags parsed contents = do
  (modName, imports) <- liftEither $ getImportsParsed dflags parsed

  modLoc <- liftIO $ mkHomeModLocation dflags modName fp
  let InstalledUnitId unitId = thisInstalledUnitId dflags
  return $ ModSummary
    { ms_mod          = mkModule (fsToUnitId unitId) modName
    , ms_location     = modLoc
    , ms_hs_date      = modTime
    , ms_textual_imps = [imp | (False, imp) <- imports]
    , ms_hspp_file    = fp
    , ms_hspp_opts    = dflags
        -- NOTE: It's /vital/ we set the 'StringBuffer' here, to give any
        -- registered GHC plugins access to the /updated/ in-memory content
        -- of a module being edited. Without this line, any plugin wishing to
        -- parse an input module and perform operations on the /current/ state
        -- of a file wouldn't work properly, as it would \"see\" a stale view of
        -- the file (i.e., the on-disk content of the latter).
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

-- | Given a buffer, env and filepath, produce a module summary by parsing only the imports.
--   Runs preprocessors as needed.
getModSummaryFromImports
  :: (HasDynFlags m, ExceptionMonad m, MonadIO m)
  => FilePath
  -> UTCTime
  -> Maybe SB.StringBuffer
  -> ExceptT [FileDiagnostic] m ModSummary
getModSummaryFromImports fp modTime contents = do
    (contents, dflags) <- preprocessor fp contents
    (srcImports, textualImports, L _ moduleName) <-
        ExceptT $ liftIO $ first (diagFromErrMsgs "parser" dflags) <$> GHC.getHeaderImports dflags contents fp fp

    -- Force bits that might keep the string buffer and DynFlags alive unnecessarily
    liftIO $ evaluate $ rnf srcImports
    liftIO $ evaluate $ rnf textualImports

    modLoc <- liftIO $ mkHomeModLocation dflags moduleName fp

    let mod = mkModule (thisPackage dflags) moduleName
        sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile
        summary =
            ModSummary
                { ms_mod          = mod
#if MIN_GHC_API_VERSION(8,8,0)
                , ms_hie_date     = Nothing
#endif
                , ms_hs_date      = modTime
                , ms_hsc_src      = sourceType
                -- The contents are used by the GetModSummary rule
                , ms_hspp_buf     = Just contents
                , ms_hspp_file    = fp
                , ms_hspp_opts    = dflags
                , ms_iface_date   = Nothing
                , ms_location     = modLoc
                , ms_obj_date     = Nothing
                , ms_parsed_mod   = Nothing
                , ms_srcimps      = srcImports
                , ms_textual_imps = textualImports
                }
    return summary

-- | Parse only the module header
parseHeader
       :: GhcMonad m
       => DynFlags -- ^ flags to use
       -> FilePath  -- ^ the filename (for source locations)
       -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], Located(HsModule GhcPs))
parseHeader dflags filename contents = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   case unP Parser.parseHeader (mkPState dflags contents loc) of
#if MIN_GHC_API_VERSION(8,10,0)
     PFailed pst ->
        throwE $ diagFromErrMsgs "parser" dflags $ getErrorMessages pst dflags
#else
     PFailed _ locErr msgErr ->
        throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
#endif
     POk pst rdr_module -> do
        let (warns, errs) = getMessages pst dflags
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
parseFileContents
       :: GhcMonad m
       => (GHC.ParsedSource -> IdePreprocessedSource)
       -> DynFlags -- ^ flags to use
       -> [PackageName] -- ^ The package imports to ignore
       -> FilePath  -- ^ the filename (for source locations)
       -> UTCTime   -- ^ the modification timestamp
       -> SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], ParsedModule)
parseFileContents customPreprocessor dflags comp_pkgs filename modTime contents = do
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
                 throwE $ diagFromErrMsgs "parser" dflags errs

               -- Ok, we got here. It's safe to continue.
               let IdePreprocessedSource preproc_warns errs parsed = customPreprocessor rdr_module
               unless (null errs) $ throwE $ diagFromStrings "parser" DsError errs
               let parsed' = removePackageImports comp_pkgs parsed
               let preproc_warnings = diagFromStrings "parser" DsWarning preproc_warns
               ms <- getModSummaryFromBuffer filename modTime dflags parsed' contents
               let pm =
                     ParsedModule {
                         pm_mod_summary = ms
                       , pm_parsed_source = parsed'
                       , pm_extra_src_files=[] -- src imports not allowed
                       , pm_annotations = hpm_annotations
                      }
                   warnings = diagFromErrMsgs "parser" dflags warns
               pure (warnings ++ preproc_warnings, pm)


-- | After parsing the module remove all package imports referring to
-- these packages as we have already dealt with what they map to.
removePackageImports :: [PackageName] -> GHC.ParsedSource -> GHC.ParsedSource
removePackageImports pkgs (L l h@HsModule {hsmodImports} ) = L l (h { hsmodImports = imports' })
  where
    imports' = map do_one_import hsmodImports
    do_one_import (L l i@ImportDecl{ideclPkgQual}) =
      case PackageName . sl_fs <$> ideclPkgQual of
        Just pn | pn `elem` pkgs -> L l (i { ideclPkgQual = Nothing })
        _ -> L l i
#if MIN_GHC_API_VERSION(8,6,0)
    do_one_import l = l
#endif

loadHieFile :: Compat.NameCacheUpdater -> FilePath -> IO GHC.HieFile
loadHieFile ncu f = do
  GHC.hie_file_result <$> GHC.readHieFile ncu f

-- | Retuns an up-to-date module interface, regenerating if needed.
--   Assumes file exists.
--   Requires the 'HscEnv' to be set up with dependencies
loadInterface
  :: MonadIO m => HscEnv
  -> ModSummary
  -> SourceModified
  -> m ([FileDiagnostic], Maybe HiFileResult) -- ^ Action to regenerate an interface
  -> m ([FileDiagnostic], Maybe HiFileResult)
loadInterface session ms sourceMod regen = do
    res <- liftIO $ checkOldIface session ms sourceMod Nothing
    case res of
          (UpToDate, Just x)
            -- If the module used TH splices when it was last
            -- compiled, then the recompilation check is not
            -- accurate enough (https://gitlab.haskell.org/ghc/ghc/-/issues/481)
            -- and we must ignore
            -- it.  However, if the module is stable (none of
            -- the modules it depends on, directly or
            -- indirectly, changed), then we *can* skip
            -- recompilation. This is why the SourceModified
            -- type contains SourceUnmodifiedAndStable, and
            -- it's pretty important: otherwise ghc --make
            -- would always recompile TH modules, even if
            -- nothing at all has changed. Stability is just
            -- the same check that make is doing for us in
            -- one-shot mode.
            | not (mi_used_th x) || SourceUnmodifiedAndStable == sourceMod
            -> return ([], Just $ HiFileResult ms x)
          (_reason, _) -> regen

-- | Non-interactive, batch version of 'InteractiveEval.getDocs'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
getDocsBatch :: GhcMonad m
        => Module  -- ^ a moudle where the names are in scope
        -> [Name]
        -> m [Either String (Maybe HsDocString, Map.Map Int HsDocString)]
getDocsBatch _mod _names =
#if MIN_GHC_API_VERSION(8,6,0)
  withSession $ \hsc_env -> liftIO $ do
    ((_warns,errs), res) <- initTc hsc_env HsSrcFile False _mod fakeSpan $ forM _names $ \name ->
        case nameModule_maybe name of
            Nothing -> return (Left $ NameHasNoModule name)
            Just mod -> do
             ModIface { mi_doc_hdr = mb_doc_hdr
                      , mi_decl_docs = DeclDocMap dmap
                      , mi_arg_docs = ArgDocMap amap
                      } <- loadModuleInterface "getModuleInterface" mod
             if isNothing mb_doc_hdr && Map.null dmap && Map.null amap
               then pure (Left (NoDocsInIface mod $ compiled name))
               else pure (Right ( Map.lookup name dmap
                                , Map.findWithDefault Map.empty name amap))
    case res of
        Just x -> return $ map (first prettyPrint) x
        Nothing -> throwErrors errs
  where
    throwErrors = liftIO . throwIO . mkSrcErr
    compiled n =
      -- TODO: Find a more direct indicator.
      case nameSrcLoc n of
        RealSrcLoc {} -> False
        UnhelpfulLoc {} -> True
#else
    return []
#endif

fakeSpan :: RealSrcSpan
fakeSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<ghcide>") 1 1

-- | Non-interactive, batch version of 'InteractiveEval.lookupNames'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
lookupName :: GhcMonad m
           => Module -- ^ A module where the Names are in scope
           -> Name
           -> m (Maybe TyThing)
lookupName mod name = withSession $ \hsc_env -> liftIO $ do
    (_messages, res) <- initTc hsc_env HsSrcFile False mod fakeSpan $ do
        tcthing <- tcLookup name
        case tcthing of
            AGlobal thing    -> return thing
            ATcId{tct_id=id} -> return (AnId id)
            _ -> panic "tcRnLookupName'"
    return res
