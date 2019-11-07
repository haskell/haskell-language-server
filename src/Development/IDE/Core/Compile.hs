-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

-- | Based on https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/API.
--   Given a list of paths to find libraries, and a file to compile, produce a list of 'CoreModule' values.
module Development.IDE.Core.Compile
  ( TcModuleResult(..)
  , compileModule
  , parseModule
  , typecheckModule
  , computePackageDeps
  , addRelativeImport
  , mkTcModuleResult
  ) where

import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Preprocessor
import Development.IDE.GHC.Error
import Development.IDE.GHC.Warnings
import Development.IDE.Types.Diagnostics
import Development.IDE.GHC.Orphans()
import Development.IDE.GHC.Util
import qualified GHC.LanguageExtensions.Type as GHC
import Development.IDE.Types.Options
import Development.IDE.Types.Location

import           GHC hiding (parseModule, typecheckModule)
import qualified Parser
import           Lexer
import ErrUtils

import qualified GHC
import           GhcMonad
import           GhcPlugins                     as GHC hiding (fst3, (<>))
import qualified HeaderInfo                     as Hdr
import           MkIface
import           StringBuffer                   as SB
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


-- | Given a string buffer, return a pre-processed @ParsedModule@.
parseModule
    :: IdeOptions
    -> HscEnv
    -> FilePath
    -> Maybe SB.StringBuffer
    -> IO ([FileDiagnostic], Maybe ParsedModule)
parseModule IdeOptions{..} env file =
    fmap (either (, Nothing) (second Just)) .
    -- We need packages since imports fail to resolve otherwise.
    runGhcEnv env . runExceptT . parseFileContents optPreprocessor file


-- | Given a package identifier, what packages does it depend on
computePackageDeps
    :: HscEnv
    -> InstalledUnitId
    -> IO (Either [FileDiagnostic] [InstalledUnitId])
computePackageDeps env pkg = do
    let dflags = hsc_dflags env
    case lookupInstalledPackage dflags pkg of
        Nothing -> return $ Left [ideErrorText (toNormalizedFilePath noFilePath) $
            T.pack $ "unknown package: " ++ show pkg]
        Just pkgInfo -> return $ Right $ depends pkgInfo


-- | Typecheck a single module using the supplied dependencies and packages.
typecheckModule
    :: IdeDefer
    -> HscEnv
    -> [TcModuleResult]
    -> ParsedModule
    -> IO ([FileDiagnostic], Maybe TcModuleResult)
typecheckModule (IdeDefer defer) packageState deps pm =
    let demoteIfDefer = if defer then demoteTypeErrorsToWarnings else id
    in
    fmap (either (, Nothing) (second Just)) $
    runGhcEnv packageState $
        catchSrcErrors "typecheck" $ do
            setupEnv deps
            (warnings, tcm) <- withWarnings "typecheck" $ \tweak ->
                GHC.typecheckModule $ demoteIfDefer pm{pm_mod_summary = tweak $ pm_mod_summary pm}
            tcm2 <- mkTcModuleResult tcm
            return (map unDefer warnings, tcm2)

-- | Compile a single type-checked module to a 'CoreModule' value, or
-- provide errors.
compileModule
    :: HscEnv
    -> [TcModuleResult]
    -> TcModuleResult
    -> IO ([FileDiagnostic], Maybe CoreModule)
compileModule packageState deps tmr =
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

            -- give variables unique OccNames
            (tidy, details) <- liftIO $ tidyProgram session desugar

            let core = CoreModule
                         (cg_module tidy)
                         (md_types details)
                         (cg_binds tidy)
                         (mg_safe_haskell desugar)

            return (map snd warnings, core)

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

unDefer :: (WarnReason, FileDiagnostic) -> FileDiagnostic
unDefer (Reason Opt_WarnDeferredTypeErrors         , fd) = upgradeWarningToError fd
unDefer (Reason Opt_WarnTypedHoles                 , fd) = upgradeWarningToError fd
unDefer (Reason Opt_WarnDeferredOutOfScopeVariables, fd) = upgradeWarningToError fd
unDefer ( _                                        , fd) = fd

upgradeWarningToError :: FileDiagnostic -> FileDiagnostic
upgradeWarningToError (nfp, fd) =
  (nfp, fd{_severity = Just DsError, _message = warn2err $ _message fd}) where
  warn2err :: T.Text -> T.Text
  warn2err = T.intercalate ": error:" . T.splitOn ": warning:"

addRelativeImport :: NormalizedFilePath -> ParsedModule -> DynFlags -> DynFlags
addRelativeImport fp modu dflags = dflags
    {importPaths = nubOrd $ maybeToList (moduleImportPath fp modu) ++ importPaths dflags}

mkTcModuleResult
    :: GhcMonad m
    => TypecheckedModule
    -> m TcModuleResult
mkTcModuleResult tcm = do
    session <- getSession
    (iface, _) <- liftIO $ mkIfaceTc session Nothing Sf_None details tcGblEnv
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

  let modLoc = ModLocation
          { ml_hs_file  = Just fp
          , ml_hi_file  = derivedFile "hi"
          , ml_obj_file = derivedFile "o"
#if MIN_GHC_API_VERSION(8,8,0)
          , ml_hie_file = derivedFile "hie"
#endif
          -- This does not consider the dflags configuration
          -- (-osuf and -hisuf, object and hi dir.s).
          -- However, we anyway don't want to generate them.
          }
      InstalledUnitId unitId = thisInstalledUnitId dflags
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
      (sourceType, derivedFile) =
          let (stem, ext) = splitExtension fp in
          if "-boot" `isSuffixOf` ext
          then (HsBootFile, \newExt -> stem <.> newExt ++ "-boot")
          else (HsSrcFile , \newExt -> stem <.> newExt)


-- | Given a buffer, flags, file path and module summary, produce a
-- parsed module (or errors) and any parse warnings.
parseFileContents
       :: GhcMonad m
       => (GHC.ParsedSource -> IdePreprocessedSource)
       -> FilePath  -- ^ the filename (for source locations)
       -> Maybe SB.StringBuffer -- ^ Haskell module source text (full Unicode is supported)
       -> ExceptT [FileDiagnostic] m ([FileDiagnostic], ParsedModule)
parseFileContents customPreprocessor filename mbContents = do
   (contents, dflags) <- preprocessor filename mbContents
   let loc  = mkRealSrcLoc (mkFastString filename) 1 1
   case unP Parser.parseModule (mkPState dflags contents loc) of
     PFailed _ locErr msgErr ->
      throwE $ diagFromErrMsg "parser" dflags $ mkPlainErrMsg dflags locErr msgErr
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
