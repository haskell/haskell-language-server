-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Demo(main) where

import Control.Concurrent.Extra
import Control.Monad
import System.Time.Extra
import Development.IDE.State.FileStore
import Development.IDE.State.Service
import Development.IDE.State.Rules
import Development.IDE.State.Shake
import Development.IDE.State.RuleTypes
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Logger
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Development.IDE.LSP.LanguageServer
import System.Environment
import Data.List
import Data.Maybe
import System.FilePath
import Data.Tuple.Extra
import System.IO.Extra
import Development.IDE.Types.LSP
import Development.Shake hiding (Env)
import qualified Data.Set as Set

import CmdLineParser
import DynFlags
import Panic
import GHC
import qualified GHC.Paths

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

main :: IO ()
main = do
    (ghcOptions, map toNormalizedFilePath -> files, isIde) <- getCmdLine

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger = makeOneHandle $ withLock lock . T.putStrLn

    let options = IdeOptions
            {optPreprocessor = (,) []
            ,optWriteIface = False
            ,optGhcSession = liftIO $ newSession ghcOptions
            ,optExtensions = ["hs"]
            ,optPkgLocationOpts = error "optPkgLocationOpts not implemented yet"
            ,optThreads = 0
            ,optShakeProfiling = Nothing -- Just "output.html"
            }

    if isIde then do
        putStrLn "Starting IDE server"
        runLanguageServer logger $ \event vfs -> do
            putStrLn "Server started"
            initialise mainRule event logger options vfs
    else do
        vfs <- makeVFSHandle
        ide <- initialise mainRule (showEvent lock) logger options vfs
        setFilesOfInterest ide $ Set.fromList files
        _ <- runAction ide $ uses_ TypeCheck files
        -- shake now writes an async message that it is completed with timing info,
        -- so we sleep briefly to wait for it to have been written
        sleep 0.01
        putStrLn "Done"


-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,) diags
showEvent lock e = withLock lock $ print e


-- | Create a GHC session that will be subsequently reused.
newSession :: [String] -> IO HscEnv
newSession flags = getLibdir >>= \libdir -> runGhc (Just libdir) $ do
    damlDFlags <- getSessionDynFlags
    (dflags', leftover, warns) <- parseDynamicFlagsCmdLine damlDFlags $ map noLoc flags

    let leftoverError = CmdLineError $
            (unlines . ("Unable to parse custom flags:":) . map unLoc) leftover
    unless (null leftover) $ liftIO $ throwGhcExceptionIO leftoverError

    unless (null warns) $
        liftIO $ putStrLn $ unlines $ "Warnings:" : map (unLoc . warnMsg) warns

    _ <- setSessionDynFlags dflags'
    getSession


-- | Convert the command line into GHC options and files to load.
getCmdLine :: IO ([String], [FilePath], Bool)
getCmdLine = do
    args <- getArgs
    let isIde = "--ide" `elem` args
    args <- return $ delete "--ide" $ if null args then [".ghci"] else args
    let (flags, files) = partition ("-" `isPrefixOf`) args
    let (ghci, hs) = partition ((==) ".ghci" . takeExtension) files
    (flags, files) <- both concat . unzip . ((flags,hs):) <$> mapM readGhci ghci
    when (null files) $
        fail "Expected some files to load, but didn't find any"
    return (flags, files, isIde)

readGhci :: FilePath -> IO ([String], [FilePath])
readGhci file = do
    xs <- lines <$> readFileUTF8' file
    let flags = concatMap words $ mapMaybe (stripPrefix ":set ") xs
    let files = concatMap words $ mapMaybe (stripPrefix ":load ") xs
    return (flags, files)
