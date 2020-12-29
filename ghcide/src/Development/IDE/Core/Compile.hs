-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
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
  , mkHiFileResultCompile
  , mkHiFileResultNoCompile
  , generateObjectCode
  , generateByteCode
  , generateHieAsts
  , writeHieFile
  , writeHiFile
  , getModSummaryFromImports
  , loadHieFile
  , loadInterface
  , loadModulesHome
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
import Development.IDE.Types.Options
import Development.IDE.Types.Location

import Language.Haskell.LSP.Types (DiagnosticTag(..))

import LoadIface (loadModuleInterface)
import DriverPhases
import HscTypes
import DriverPipeline hiding (unP)

import qualified Parser
import           Lexer
#if MIN_GHC_API_VERSION(8,10,0)
import Control.DeepSeq (force, rnf)
#else
import Control.DeepSeq (rnf)
import ErrUtils
#endif

import           Finder
import           Development.IDE.GHC.Compat hiding (parseModule, typecheckModule, writeHieFile)
import qualified Development.IDE.GHC.Compat     as GHC
import qualified Development.IDE.GHC.Compat     as Compat
import           GhcMonad
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import           HscMain                        (makeSimpleDetails, hscDesugar, hscTypecheckRename, hscSimplify, hscGenHardCode, hscInteractive)
import           MkIface
import           StringBuffer                   as SB
import           TcRnMonad
import           TcIface                        (typecheckIface)
import           TidyPgm

import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Bifunctor                           (first, second)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Map.Strict                          as Map
import           System.FilePath
import           System.Directory
import           System.IO.Extra
import Control.Exception (evaluate)
import TcEnv (tcLookup)
import Data.Time (UTCTime, getCurrentTime)
import Linker (unload)
import qualified GHC.LanguageExtensions as LangExt
import PrelNames
import HeaderInfo
import Maybes (orElse)

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
                -> [Linkable] -- ^ linkables not to unload
                -> ParsedModule
                -> IO (IdeResult TcModuleResult)
typecheckModule (IdeDefer defer) hsc keep_lbls pm = do
    fmap (either (,Nothing) id) $
      catchSrcErrors (hsc_dflags hsc) "typecheck" $ do

        let modSummary = pm_mod_summary pm
            dflags = ms_hspp_opts modSummary

        modSummary' <- initPlugins hsc modSummary
        (warnings, tcm) <- withWarnings "typecheck" $ \tweak ->
            tcRnModule hsc keep_lbls $ enableTopLevelWarnings
                                     $ demoteIfDefer pm{pm_mod_summary = tweak modSummary'}
        let errorPipeline = unDefer . hideDiag dflags . tagDiag
            diags = map errorPipeline warnings
            deferedError = any fst diags
        return (map snd diags, Just $ tcm{tmrDeferedError = deferedError})
    where
        demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id

tcRnModule :: HscEnv -> [Linkable] -> ParsedModule -> IO TcModuleResult
tcRnModule hsc_env keep_lbls pmod = do
  let ms = pm_mod_summary pmod
      hsc_env_tmp = hsc_env { hsc_dflags = ms_hspp_opts ms }

  unload hsc_env_tmp keep_lbls
  (tc_gbl_env, mrn_info) <-
      hscTypecheckRename hsc_env_tmp ms $
                HsParsedModule { hpm_module = parsedSource pmod,
                                 hpm_src_files = pm_extra_src_files pmod,
                                 hpm_annotations = pm_annotations pmod }
  let rn_info = case mrn_info of
        Just x -> x
        Nothing -> error "no renamed info tcRnModule"
  pure (TcModuleResult pmod rn_info tc_gbl_env False)

mkHiFileResultNoCompile :: HscEnv -> TcModuleResult -> IO HiFileResult
mkHiFileResultNoCompile session tcm = do
  let hsc_env_tmp = session { hsc_dflags = ms_hspp_opts ms }
      ms = pm_mod_summary $ tmrParsed tcm
      tcGblEnv = tmrTypechecked tcm
  details <- makeSimpleDetails hsc_env_tmp tcGblEnv
  sf <- finalSafeMode (ms_hspp_opts ms) tcGblEnv
#if MIN_GHC_API_VERSION(8,10,0)
  iface <- mkIfaceTc session sf details tcGblEnv
#else
  (iface, _) <- mkIfaceTc session Nothing sf details tcGblEnv
#endif
  let mod_info = HomeModInfo iface details Nothing
  pure $! HiFileResult ms mod_info

mkHiFileResultCompile
    :: HscEnv
    -> TcModuleResult
    -> ModGuts
    -> LinkableType -- ^ use object code or byte code?
    -> IO (IdeResult HiFileResult)
mkHiFileResultCompile session' tcm simplified_guts ltype = catchErrs $ do
  let session = session' { hsc_dflags = ms_hspp_opts ms }
      ms = pm_mod_summary $ tmrParsed tcm
  -- give variables unique OccNames
  (guts, details) <- tidyProgram session simplified_guts

  let genLinkable = case ltype of
        ObjectLinkable -> generateObjectCode
        BCOLinkable -> generateByteCode

  (diags, linkable) <- genLinkable session ms guts
#if MIN_GHC_API_VERSION(8,10,0)
  let !partial_iface = force (mkPartialIface session details simplified_guts)
  final_iface <- mkFullIface session partial_iface
#else
  (final_iface,_) <- mkIface session Nothing details simplified_guts
#endif
  let mod_info = HomeModInfo final_iface details linkable
  pure (diags, Just $! HiFileResult ms mod_info)

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
    dflags <- liftIO $ initializePlugins session $ ms_hspp_opts modSummary
    return modSummary{ms_hspp_opts = dflags}

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
               let ms' = tweak ms
                   session' = session{ hsc_dflags = ms_hspp_opts ms'}
               desugar <- hscDesugar session' ms' tcg
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
                withWarnings "object" $ \_tweak -> do
                      let summary' = _tweak summary
                          session' = session { hsc_dflags = (ms_hspp_opts summary') { outputFile = Just dot_o }}
                      (outputFilename, _mStub, _foreign_files) <- hscGenHardCode session' guts
#if MIN_GHC_API_VERSION(8,10,0)
                                (ms_location summary')
#else
                                summary'
#endif
                                fp
                      compileFile session' StopLn (outputFilename, Just (As False))
              let unlinked = DotO dot_o_fp
              -- Need time to be the modification time for recompilation checking
              t <- liftIO $ getModificationTime dot_o_fp
              let linkable = LM t mod [unlinked]

              pure (map snd warnings, linkable)

generateByteCode :: HscEnv -> ModSummary -> CgGuts -> IO (IdeResult Linkable)
generateByteCode hscEnv summary guts = do
    fmap (either (, Nothing) (second Just)) $
          catchSrcErrors (hsc_dflags hscEnv) "bytecode" $ do
              (warnings, (_, bytecode, sptEntries)) <-
                withWarnings "bytecode" $ \_tweak -> do
                      let summary' = _tweak summary
                          session = hscEnv { hsc_dflags = ms_hspp_opts summary' }
                      hscInteractive session guts
#if MIN_GHC_API_VERSION(8,10,0)
                                (ms_location summary')
#else
                                summary'
#endif
              let unlinked = BCOs bytecode sptEntries
              time <- liftIO getCurrentTime
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
#if MIN_GHC_API_VERSION(8,10,0)
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

atomicFileWrite :: FilePath -> (FilePath -> IO a) -> IO ()
atomicFileWrite targetPath write = do
  let dir = takeDirectory targetPath
  createDirectoryIfMissing True dir
  (tempFilePath, cleanUp) <- newTempFileWithin dir
  (write tempFilePath >> renameFile tempFilePath targetPath) `onException` cleanUp

generateHieAsts :: HscEnv -> TcModuleResult -> IO ([FileDiagnostic], Maybe (HieASTs Type))
generateHieAsts hscEnv tcm =
  handleGenerationErrors' dflags "extended interface generation" $ runHsc hscEnv $
    Just <$> GHC.enrichHie (tcg_binds $ tmrTypechecked tcm) (tmrRenamed tcm)
  where
    dflags = hsc_dflags hscEnv

writeHieFile :: HscEnv -> ModSummary -> [GHC.AvailInfo] -> HieASTs Type -> BS.ByteString -> IO [FileDiagnostic]
writeHieFile hscEnv mod_summary exports ast source =
  handleGenerationErrors dflags "extended interface write/compression" $ do
    hf <- runHsc hscEnv $
      GHC.mkHieFile' mod_summary exports ast source
    atomicFileWrite targetPath $ flip GHC.writeHieFile hf
  where
    dflags       = hsc_dflags hscEnv
    mod_location = ms_location mod_summary
    targetPath   = Compat.ml_hie_file mod_location

writeHiFile :: HscEnv -> HiFileResult -> IO [FileDiagnostic]
writeHiFile hscEnv tc =
  handleGenerationErrors dflags "interface generation" $ do
    atomicFileWrite targetPath $ \fp ->
      writeIfaceFile dflags fp modIface
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

-- | Initialise the finder cache, dependencies should be topologically
-- sorted.
setupFinderCache :: [ModSummary] -> HscEnv -> IO HscEnv
setupFinderCache mss session = do

    -- Make modules available for others that import them,
    -- by putting them in the finder cache.
    let ims  = map (InstalledModule (thisInstalledUnitId $ hsc_dflags session) . moduleName . ms_mod) mss
        ifrs = zipWith (\ms -> InstalledFound (ms_location ms)) mss ims
    -- set the target and module graph in the session
        graph = mkModuleGraph mss

    -- We have to create a new IORef here instead of modifying the existing IORef as
    -- it is shared between concurrent compilations.
    prevFinderCache <- readIORef $ hsc_FC session
    let newFinderCache =
            foldl'
                (\fc (im, ifr) -> GHC.extendInstalledModuleEnv fc im ifr) prevFinderCache
                $ zip ims ifrs
    newFinderCacheVar <- newIORef $! newFinderCache

    pure $ session { hsc_FC = newFinderCacheVar, hsc_mod_graph = graph }


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
    e { hsc_HPT = addListToHpt (hsc_HPT e) [(mod_name x, x) | x <- mod_infos]
      , hsc_type_env_var = Nothing }
    where
      mod_name = moduleName . mi_module . hm_iface

withBootSuffix :: HscSource -> ModLocation -> ModLocation
withBootSuffix HsBootFile = addBootSuffixLocnOut
withBootSuffix _ = id

-- | Given a buffer, env and filepath, produce a module summary by parsing only the imports.
--   Runs preprocessors as needed.
getModSummaryFromImports
  :: HscEnv
  -> FilePath
  -> UTCTime
  -> Maybe SB.StringBuffer
  -> ExceptT [FileDiagnostic] IO (ModSummary,[LImportDecl GhcPs])
getModSummaryFromImports env fp modTime contents = do
    (contents, dflags) <- preprocessor env fp contents

    -- The warns will hopefully be reported when we actually parse the module
    (_warns, L main_loc hsmod) <- parseHeader dflags fp contents

    -- Copied from `HeaderInfo.getImports`, but we also need to keep the parsed imports
    let mb_mod = hsmodName hsmod
        imps = hsmodImports hsmod

        mod = fmap unLoc mb_mod `orElse` mAIN_NAME

        (src_idecls, ord_idecls) = partition (ideclSource.unLoc) imps

        -- GHC.Prim doesn't exist physically, so don't go looking for it.
        ordinary_imps = filter ((/= moduleName gHC_PRIM) . unLoc
                                . ideclName . unLoc)
                               ord_idecls

        implicit_prelude = xopt LangExt.ImplicitPrelude dflags
        implicit_imports = mkPrelImports mod main_loc
                                         implicit_prelude imps
        convImport (L _ i) = (fmap sl_fs (ideclPkgQual i)
                                         , ideclName i)

        srcImports = map convImport src_idecls
        textualImports = map convImport (implicit_imports ++ ordinary_imps)

        allImps = implicit_imports ++ imps

    -- Force bits that might keep the string buffer and DynFlags alive unnecessarily
    liftIO $ evaluate $ rnf srcImports
    liftIO $ evaluate $ rnf textualImports

    modLoc <- liftIO $ mkHomeModLocation dflags mod fp

    let modl = mkModule (thisPackage dflags) mod
        sourceType = if "-boot" `isSuffixOf` takeExtension fp then HsBootFile else HsSrcFile
        summary =
            ModSummary
                { ms_mod          = modl
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
                , ms_location     = withBootSuffix sourceType modLoc
                , ms_obj_date     = Nothing
                , ms_parsed_mod   = Nothing
                , ms_srcimps      = srcImports
                , ms_textual_imps = textualImports
                }
    return (summary, allImps)

-- | Parse only the module header
parseHeader
       :: Monad m
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
-- ModSummary must contain the (preprocessed) contents of the buffer
parseFileContents
       :: HscEnv
       -> (GHC.ParsedSource -> IdePreprocessedSource)
       -> FilePath  -- ^ the filename (for source locations)
       -> ModSummary
       -> ExceptT [FileDiagnostic] IO ([FileDiagnostic], ParsedModule)
parseFileContents env customPreprocessor filename ms = do
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
       dflags = ms_hspp_opts ms
       contents = fromJust $ ms_hspp_buf ms
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
                                  $ map unpackFS
                                  $ srcfiles pst
                   srcs1 = case ml_hs_file (ms_location ms) of
                             Just f  -> filter (/= normalise f) srcs0
                             Nothing -> srcs0

               -- sometimes we see source files from earlier
               -- preprocessing stages that cannot be found, so just
               -- filter them out:
               srcs2 <- liftIO $ filterM doesFileExist srcs1

               let pm =
                     ParsedModule {
                         pm_mod_summary = ms
                       , pm_parsed_source = parsed'
                       , pm_extra_src_files = srcs2
                       , pm_annotations = hpm_annotations
                      }
                   warnings = diagFromErrMsgs "parser" dflags warns
               pure (warnings ++ preproc_warnings, pm)

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
  -> Maybe LinkableType
  -> (Maybe LinkableType -> m ([FileDiagnostic], Maybe HiFileResult)) -- ^ Action to regenerate an interface
  -> m ([FileDiagnostic], Maybe HiFileResult)
loadInterface session ms sourceMod linkableNeeded regen = do
    res <- liftIO $ checkOldIface session ms sourceMod Nothing
    case res of
          (UpToDate, Just iface)
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
            | not (mi_used_th iface) || SourceUnmodifiedAndStable == sourceMod
            -> do
             linkable <- case linkableNeeded of
               Just ObjectLinkable -> liftIO $ findObjectLinkableMaybe (ms_mod ms) (ms_location ms)
               _ -> pure Nothing

             -- We don't need to regenerate if the object is up do date, or we don't need one
             let objUpToDate = isNothing linkableNeeded || case linkable of
                   Nothing -> False
                   Just (LM obj_time _ _) -> obj_time > ms_hs_date ms
             if objUpToDate
             then do
               hmi <- liftIO $ mkDetailsFromIface session iface linkable
               return ([], Just $ HiFileResult ms hmi)
             else regen linkableNeeded
          (_reason, _) -> regen linkableNeeded

mkDetailsFromIface :: HscEnv -> ModIface -> Maybe Linkable -> IO HomeModInfo
mkDetailsFromIface session iface linkable = do
  details <- liftIO $ fixIO $ \details -> do
    let hsc' = session { hsc_HPT = addToHpt (hsc_HPT session) (moduleName $ mi_module iface) (HomeModInfo iface details linkable) }
    initIfaceLoad hsc' (typecheckIface iface)
  return (HomeModInfo iface details linkable)

-- | Non-interactive, batch version of 'InteractiveEval.getDocs'.
--   The interactive paths create problems in ghc-lib builds
--- and leads to fun errors like "Cannot continue after interface file error".
getDocsBatch
  :: HscEnv
  -> Module  -- ^ a moudle where the names are in scope
  -> [Name]
  -> IO [Either String (Maybe HsDocString, Map.Map Int HsDocString)]
getDocsBatch hsc_env _mod _names = do
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

fakeSpan :: RealSrcSpan
fakeSpan = realSrcLocSpan $ mkRealSrcLoc (fsLit "<ghcide>") 1 1

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
            _ -> panic "tcRnLookupName'"
    return res
