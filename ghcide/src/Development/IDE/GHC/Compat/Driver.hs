-- ============================================================================
-- DO NOT EDIT
-- This module copies parts of the driver code in GHC.Driver.Main to provide
-- `hscTypecheckRenameWithDiagnostics`.
-- Issue to add this function: https://gitlab.haskell.org/ghc/ghc/-/issues/24996
-- MR to add this function: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12891
-- ============================================================================

{-# LANGUAGE CPP #-}

module Development.IDE.GHC.Compat.Driver
    ( hscTypecheckRenameWithDiagnostics
    ) where

#if MIN_VERSION_ghc(9,11,0)

import           GHC.Driver.Main            (hscTypecheckRenameWithDiagnostics)

#else

import           Control.Monad
import           GHC.Core
import           GHC.Data.FastString
import           GHC.Data.Maybe
import           GHC.Driver.Env
import           GHC.Driver.Errors.Types
import           GHC.Driver.Main
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Hs.Dump
import           GHC.Iface.Ext.Ast          (mkHieFile)
import           GHC.Iface.Ext.Binary       (hie_file_result, readHieFile,
                                             writeHieFile)
import           GHC.Iface.Ext.Debug        (diffFile, validateScopes)
import           GHC.Iface.Ext.Types        (getAsts, hie_asts, hie_module)
import           GHC.Tc.Module
import           GHC.Tc.Utils.Monad
import           GHC.Types.SourceFile
import           GHC.Types.SrcLoc
import           GHC.Unit
import           GHC.Unit.Module.ModDetails
import           GHC.Unit.Module.ModIface
import           GHC.Unit.Module.ModSummary
import           GHC.Utils.Error
import           GHC.Utils.Logger
import           GHC.Utils.Outputable
import           GHC.Utils.Panic.Plain

hscTypecheckRenameWithDiagnostics :: HscEnv -> ModSummary -> HsParsedModule
                   -> IO ((TcGblEnv, RenamedStuff), Messages GhcMessage)
hscTypecheckRenameWithDiagnostics hsc_env mod_summary rdr_module =
    runHsc' hsc_env $ hsc_typecheck True mod_summary (Just rdr_module)

-- ============================================================================
-- DO NOT EDIT - Refer to top of file
-- ============================================================================
hsc_typecheck :: Bool -- ^ Keep renamed source?
              -> ModSummary -> Maybe HsParsedModule
              -> Hsc (TcGblEnv, RenamedStuff)
hsc_typecheck keep_rn mod_summary mb_rdr_module = do
    hsc_env <- getHscEnv
    let hsc_src = ms_hsc_src mod_summary
        dflags = hsc_dflags hsc_env
        home_unit = hsc_home_unit hsc_env
        outer_mod = ms_mod mod_summary
        mod_name = moduleName outer_mod
        outer_mod' = mkHomeModule home_unit mod_name
        inner_mod = homeModuleNameInstantiation home_unit mod_name
        src_filename  = ms_hspp_file mod_summary
        real_loc = realSrcLocSpan $ mkRealSrcLoc (mkFastString src_filename) 1 1
        keep_rn' = gopt Opt_WriteHie dflags || keep_rn
    massert (isHomeModule home_unit outer_mod)
    tc_result <- if hsc_src == HsigFile && not (isHoleModule inner_mod)
        then ioMsgMaybe $ hoistTcRnMessage $ tcRnInstantiateSignature hsc_env outer_mod' real_loc
        else
         do hpm <- case mb_rdr_module of
                    Just hpm -> return hpm
                    Nothing  -> hscParse' mod_summary
            tc_result0 <- tcRnModule' mod_summary keep_rn' hpm
            if hsc_src == HsigFile
                then
                     do (iface, _) <- liftIO $ hscSimpleIface hsc_env Nothing tc_result0 mod_summary
                        ioMsgMaybe $ hoistTcRnMessage $
                            tcRnMergeSignatures hsc_env hpm tc_result0 iface
                else return tc_result0
    rn_info <- extract_renamed_stuff mod_summary tc_result
    return (tc_result, rn_info)

-- ============================================================================
-- DO NOT EDIT - Refer to top of file
-- ============================================================================
extract_renamed_stuff :: ModSummary -> TcGblEnv -> Hsc RenamedStuff
extract_renamed_stuff mod_summary tc_result = do
    let rn_info = getRenamedStuff tc_result

    dflags <- getDynFlags
    logger <- getLogger
    liftIO $ putDumpFileMaybe logger Opt_D_dump_rn_ast "Renamer"
                FormatHaskell (showAstData NoBlankSrcSpan NoBlankEpAnnotations rn_info)

    -- Create HIE files
    when (gopt Opt_WriteHie dflags) $ do
        -- I assume this fromJust is safe because `-fwrite-hie-file`
        -- enables the option which keeps the renamed source.
        hieFile <- mkHieFile mod_summary tc_result (fromJust rn_info)
        let out_file = ml_hie_file $ ms_location mod_summary
        liftIO $ writeHieFile out_file hieFile
        liftIO $ putDumpFileMaybe logger Opt_D_dump_hie "HIE AST" FormatHaskell (ppr $ hie_asts hieFile)

        -- Validate HIE files
        when (gopt Opt_ValidateHie dflags) $ do
            hs_env <- Hsc $ \e w -> return (e, w)
            liftIO $ do
              -- Validate Scopes
              case validateScopes (hie_module hieFile) $ getAsts $ hie_asts hieFile of
                  [] -> putMsg logger $ text "Got valid scopes"
                  xs -> do
                    putMsg logger $ text "Got invalid scopes"
                    mapM_ (putMsg logger) xs
              -- Roundtrip testing
              file' <- readHieFile (hsc_NC hs_env) out_file
              case diffFile hieFile (hie_file_result file') of
                [] ->
                  putMsg logger $ text "Got no roundtrip errors"
                xs -> do
                  putMsg logger $ text "Got roundtrip errors"
                  let logger' = updateLogFlags logger (log_set_dopt Opt_D_ppr_debug)
                  mapM_ (putMsg logger') xs
    return rn_info

-- ============================================================================
-- DO NOT EDIT - Refer to top of file
-- ============================================================================
hscSimpleIface :: HscEnv
               -> Maybe CoreProgram
               -> TcGblEnv
               -> ModSummary
               -> IO (ModIface, ModDetails)
hscSimpleIface hsc_env mb_core_program tc_result summary
    = runHsc hsc_env $ hscSimpleIface' mb_core_program tc_result summary

#endif
