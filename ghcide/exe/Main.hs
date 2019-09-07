-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above

module Main(main) where

import Arguments
import Data.Maybe
import Data.List.Extra
import System.FilePath
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Default
import System.Time.Extra
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.Service
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.Core.RuleTypes
import Development.IDE.LSP.Protocol
import Development.IDE.Types.Location
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Types.Logger
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Linker
import System.Info
import Data.Version
import Development.IDE.LSP.LanguageServer
import System.Directory.Extra as IO
import System.Environment
import System.IO
import Development.Shake hiding (Env)
import qualified Data.Set as Set

-- import CmdLineParser
-- import DynFlags
-- import Panic
import GHC hiding (def)
import qualified GHC.Paths

import HIE.Bios

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    hPutStrLn stderr $ "Starting hie-core (GHC v" ++ showVersion compilerVersion ++ ")"
    Arguments{..} <- getArguments

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger = Logger $ \pri msg -> withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd setCurrentDirectory

    dir <- getCurrentDirectory
    hPutStrLn stderr dir

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        runLanguageServer def def $ \event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            let options = (defaultIdeOptions $ liftIO $ newSession' =<< findCradle (dir <> "/"))
                    { optReportProgress = clientSupportsProgress caps }
            initialise (mainRule >> action kick) event logger options vfs
    else do
        putStrLn "[1/6] Finding hie-bios cradle"
        cradle <- findCradle (dir <> "/")
        print cradle

        putStrLn "\n[2/6] Converting Cradle to GHC session"
        env <- newSession' cradle

        putStrLn "\n[3/6] Initialising IDE session"
        vfs <- makeVFSHandle
        ide <- initialise mainRule (showEvent lock) logger (defaultIdeOptions $ return env) vfs

        putStrLn "\n[4/6] Finding interesting files"
        files <- nubOrd <$> expandFiles (argFiles ++ ["." | null argFiles])
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\n[5/6] Setting interesting files"
        setFilesOfInterest ide $ Set.fromList $ map toNormalizedFilePath files

        putStrLn "\n[6/6] Loading interesting files"
        results <- runActionSync ide $ uses TypeCheck $ map toNormalizedFilePath files
        let (worked, failed) = partition fst $ zip (map isJust results) files
        putStrLn $ "Files that worked: " ++ show (length worked)
        putStrLn $ "Files that failed: " ++ show (length failed)
        putStr $ unlines $ map ((++) " * " . snd) failed

        putStrLn "Done"


expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b then return [x] else do
        let recurse "." = True
            recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
            recurse x = takeFileName x `notElem` ["dist","dist-newstyle"] -- cabal directories
        files <- filter (\x -> takeExtension x `elem` [".hs",".lhs"]) <$> listFilesInside (return . recurse) x
        when (null files) $
            fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
        return files


kick :: Action ()
kick = do
    files <- getFilesOfInterest
    void $ uses TypeCheck $ Set.toList files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,) diags
showEvent lock e = withLock lock $ print e

newSession' :: Cradle -> IO HscEnv
newSession' cradle = getLibdir >>= \libdir -> do
    env <- runGhc (Just libdir) $ do
        initializeFlagsWithCradle "" cradle
        getSession
    initDynLinker env
    pure env
