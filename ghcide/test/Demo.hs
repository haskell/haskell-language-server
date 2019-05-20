-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Demo(main) where

import Control.Concurrent.Extra
import Control.Monad
import System.Time.Extra
import Development.IDE.State.Service
import Development.IDE.State.Rules
import Development.IDE.State.Shake
import Development.IDE.State.RuleTypes
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Logger
import qualified Data.Text.IO as T
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
import GHC.Paths


main :: IO ()
main = do
    (ghcOptions, files) <- getCmdLine

    -- lock to avoid overlapping output on stdout
    lock <- newLock

    ide <- initialise
        mainRule
        (Just $ showEvent lock)
        (makeOneHandle $ withLock lock . T.putStrLn)
        IdeOptions
            {optPreprocessor = (,) []
            ,optWriteIface = False
            ,optGhcSession = liftIO $ newSession ghcOptions
            ,optExtensions = ["hs"]
            ,optPkgLocationOpts = error "optPkgLocationOpts not implemented yet"
            ,optThreads = 0
            ,optShakeProfiling = Nothing -- Just "output.html"
            }
    setFilesOfInterest ide $ Set.fromList files
    _ <- runAction ide $ uses_ TypeCheck files
    -- shake now writes an async message that it is completed with timing info,
    -- so we sleep briefly to wait for it to have been written
    sleep 0.01
    putStrLn "Done"


-- | Print an LSP event.
showEvent :: Lock -> Event -> IO ()
showEvent _ (EventFileDiagnostics (_, [])) = return ()
showEvent lock (EventFileDiagnostics (file, diags)) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,) diags
showEvent lock e = withLock lock $ print e


-- | Create a GHC session that will be subsequently reused.
newSession :: [String] -> IO HscEnv
newSession flags = runGhc (Just libdir) $ do
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
getCmdLine :: IO ([String], [FilePath])
getCmdLine = do
    args <- getArgs
    args <- return $ if null args then [".ghci"] else args
    let (flags, files) = partition ("-" `isPrefixOf`) args
    let (ghci, hs) = partition ((==) ".ghci" . takeExtension) files
    (flags, files) <- both concat . unzip . ((flags,hs):) <$> mapM readGhci ghci
    when (null files) $
        fail "Expected some files to load, but didn't find any"
    return (flags, files)

readGhci :: FilePath -> IO ([String], [FilePath])
readGhci file = do
    xs <- lines <$> readFileUTF8' file
    let flags = concatMap words $ mapMaybe (stripPrefix ":set ") xs
    let files = concatMap words $ mapMaybe (stripPrefix ":load ") xs
    return (flags, files)
