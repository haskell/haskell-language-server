-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Core.Preprocessor
  ( preprocessor
  ) where

import           Development.IDE.GHC.CPP
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util   as Util
import           Development.IDE.GHC.Orphans       ()

import           Control.DeepSeq                   (NFData (rnf))
import           Control.Exception                 (evaluate)
import           Control.Exception.Safe            (catch, throw)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Char
import           Data.IORef                        (IORef, modifyIORef,
                                                    newIORef, readIORef)
import           Data.List.Extra
import           Data.Maybe
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import qualified GHC.LanguageExtensions            as LangExt
import           System.FilePath
import           System.IO.Extra

-- | Given a file and some contents, apply any necessary preprocessors,
--   e.g. unlit/cpp. Return the resulting buffer and the DynFlags it implies.
preprocessor :: HscEnv -> FilePath -> Maybe Util.StringBuffer -> ExceptT [FileDiagnostic] IO (Util.StringBuffer, [String], DynFlags)
preprocessor env0 filename mbContents = do
    -- Perform unlit
    (isOnDisk, contents) <-
        if isLiterate filename then do
            newcontent <- liftIO $ runLhs env0 filename mbContents
            return (False, newcontent)
        else do
            contents <- liftIO $ maybe (Util.hGetStringBuffer filename) return mbContents
            let isOnDisk = isNothing mbContents
            return (isOnDisk, contents)

    -- Perform cpp
    (opts, dflags) <- ExceptT $ parsePragmasIntoDynFlags env0 filename contents
    let env1 = hscSetFlags dflags env0
    let logger = hsc_logger env1
    (isOnDisk, contents, opts, dflags) <-
        if not $ xopt LangExt.Cpp dflags then
            return (isOnDisk, contents, opts, dflags)
        else do
            cppLogs <- liftIO $ newIORef []
            let newLogger = pushLogHook (const (logActionCompat $ logAction cppLogs)) logger
            contents <- ExceptT
                        $ (Right <$> (runCpp (putLogHook newLogger env1) filename
                                       $ if isOnDisk then Nothing else Just contents))
                            `catch`
                            ( \(e :: Util.GhcException) -> do
                                logs <- readIORef cppLogs
                                case diagsFromCPPLogs filename (reverse logs) of
                                  []    -> throw e
                                  diags -> return $ Left diags
                            )
            (opts, dflags) <- ExceptT $ parsePragmasIntoDynFlags env1 filename contents
            return (False, contents, opts, dflags)

    -- Perform preprocessor
    if not $ gopt Opt_Pp dflags then
        return (contents, opts, dflags)
    else do
        contents <- liftIO $ runPreprocessor env1 filename $ if isOnDisk then Nothing else Just contents
        (opts, dflags) <- ExceptT $ parsePragmasIntoDynFlags env1 filename contents
        return (contents, opts, dflags)
  where
    logAction :: IORef [CPPLog] -> LogActionCompat
    logAction cppLogs dflags _reason severity srcSpan _style msg = do
      let log = CPPLog severity srcSpan $ T.pack $ showSDoc dflags msg
      modifyIORef cppLogs (log :)


data CPPLog = CPPLog Severity SrcSpan Text
  deriving (Show)


data CPPDiag
  = CPPDiag
      { cdRange    :: Range,
        cdSeverity :: Maybe DiagnosticSeverity,
        cdMessage  :: [Text]
      }
  deriving (Show)


diagsFromCPPLogs :: FilePath -> [CPPLog] -> [FileDiagnostic]
diagsFromCPPLogs filename logs =
  map (\d -> (toNormalizedFilePath' filename, ShowDiag, cppDiagToDiagnostic d)) $
    go [] logs
  where
    -- On errors, CPP calls logAction with a real span for the initial log and
    -- then additional informational logs with `UnhelpfulSpan`. Collect those
    -- informational log messages and attaches them to the initial log message.
    go :: [CPPDiag] -> [CPPLog] -> [CPPDiag]
    go acc [] = reverse $ map (\d -> d {cdMessage = reverse $ cdMessage d}) acc
    go acc (CPPLog sev (RealSrcSpan span _) msg : logs) =
      let diag = CPPDiag (realSrcSpanToRange span) (toDSeverity sev) [msg]
       in go (diag : acc) logs
    go (diag : diags) (CPPLog _sev (UnhelpfulSpan _) msg : logs) =
      go (diag {cdMessage = msg : cdMessage diag} : diags) logs
    go [] (CPPLog _sev (UnhelpfulSpan _) _msg : logs) = go [] logs
    cppDiagToDiagnostic :: CPPDiag -> Diagnostic
    cppDiagToDiagnostic d =
      Diagnostic
        { _range = cdRange d,
          _severity = cdSeverity d,
          _code = Nothing,
          _source = Just "CPP",
          _message = T.unlines $ cdMessage d,
          _relatedInformation = Nothing,
          _tags = Nothing
        }


isLiterate :: FilePath -> Bool
isLiterate x = takeExtension x `elem` [".lhs",".lhs-boot"]


-- | This reads the pragma information directly from the provided buffer.
parsePragmasIntoDynFlags
    :: HscEnv
    -> FilePath
    -> Util.StringBuffer
    -> IO (Either [FileDiagnostic] ([String], DynFlags))
parsePragmasIntoDynFlags env fp contents = catchSrcErrors dflags0 "pragmas" $ do
    let opts = getOptions dflags0 contents fp

    -- Force bits that might keep the dflags and stringBuffer alive unnecessarily
    evaluate $ rnf opts

    (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
    hsc_env' <- initializePlugins (hscSetFlags dflags env)
    return (map unLoc opts, disableWarningsAsErrors (hsc_dflags hsc_env'))
  where dflags0 = hsc_dflags env

-- | Run (unlit) literate haskell preprocessor on a file, or buffer if set
runLhs :: HscEnv -> FilePath -> Maybe Util.StringBuffer -> IO Util.StringBuffer
runLhs env filename contents = withTempDir $ \dir -> do
    let fout = dir </> takeFileName filename <.> "unlit"
    filesrc <- case contents of
        Nothing   -> return filename
        Just cnts -> do
            let fsrc = dir </> takeFileName filename <.> "literate"
            withBinaryFile fsrc WriteMode $ \h ->
                hPutStringBuffer h cnts
            return fsrc
    unlit filesrc fout
    Util.hGetStringBuffer fout
  where
    logger = hsc_logger env
    dflags = hsc_dflags env

    unlit filein fileout = runUnlit logger dflags (args filein fileout)
    args filein fileout = [
                      Option     "-h"
                    , Option     (escape filename) -- name this file
                    , FileOption "" filein       -- input file
                    , FileOption "" fileout ]    -- output file
    -- taken from ghc's DriverPipeline.hs
    escape ('\\':cs) = '\\':'\\': escape cs
    escape ('\"':cs) = '\\':'\"': escape cs
    escape ('\'':cs) = '\\':'\'': escape cs
    escape (c:cs)    = c : escape cs
    escape []        = []

-- | Run CPP on a file
runCpp :: HscEnv -> FilePath -> Maybe Util.StringBuffer -> IO Util.StringBuffer
runCpp env0 filename contents = withTempDir $ \dir -> do
    let out = dir </> takeFileName filename <.> "out"
    let dflags1 = addOptP "-D__GHCIDE__" (hsc_dflags env0)
    let env1 = hscSetFlags dflags1 env0

    case contents of
        Nothing -> do
            -- Happy case, file is not modified, so run CPP on it in-place
            -- which also makes things like relative #include files work
            -- and means location information is correct
            doCpp env1 True filename out
            liftIO $ Util.hGetStringBuffer out

        Just contents -> do
            -- Sad path, we have to create a version of the path in a temp dir
            -- __FILE__ macro is wrong, ignoring that for now (likely not a real issue)

            -- Relative includes aren't going to work, so we fix that by adding to the include path.
            let dflags2 = addIncludePathsQuote (takeDirectory filename) dflags1
            let env2 = hscSetFlags dflags2 env0
            -- Location information is wrong, so we fix that by patching it afterwards.
            let inp = dir </> "___GHCIDE_MAGIC___"
            withBinaryFile inp WriteMode $ \h ->
                hPutStringBuffer h contents
            doCpp env2 True inp out

            -- Fix up the filename in lines like:
            -- # 1 "C:/Temp/extra-dir-914611385186/___GHCIDE_MAGIC___"
            let tweak x
                    | Just x <- stripPrefix "# " x
                    , "___GHCIDE_MAGIC___" `isInfixOf` x
                    , let num = takeWhile (not . isSpace) x
                    -- important to use /, and never \ for paths, even on Windows, since then C escapes them
                    -- and GHC gets all confused
                        = "# " <> num <> " \"" <> map (\x -> if isPathSeparator x then '/' else x) filename <> "\""
                    | otherwise = x
            Util.stringToStringBuffer . unlines . map tweak . lines <$> readFileUTF8' out


-- | Run a preprocessor on a file
runPreprocessor :: HscEnv -> FilePath -> Maybe Util.StringBuffer -> IO Util.StringBuffer
runPreprocessor env filename contents = withTempDir $ \dir -> do
    let out = dir </> takeFileName filename <.> "out"
    inp <- case contents of
        Nothing -> return filename
        Just contents -> do
            let inp = dir </> takeFileName filename <.> "hs"
            withBinaryFile inp WriteMode $ \h ->
                hPutStringBuffer h contents
            return inp
    runPp logger dflags [Option filename, Option inp, FileOption "" out]
    Util.hGetStringBuffer out
  where
    logger = hsc_logger env
    dflags = hsc_dflags env
