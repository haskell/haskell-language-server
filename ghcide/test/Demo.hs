-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Demo(main) where

import Data.Maybe
import Control.Concurrent.Extra
import Control.Monad
import System.Time.Extra
import Development.IDE.Core.FileStore
import Development.IDE.Core.Service
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.Core.RuleTypes
import Development.IDE.LSP.Protocol
import Development.IDE.Types.Location
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Options
import Development.IDE.Types.Logger
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Development.IDE.LSP.LanguageServer
import System.Directory
import System.Environment
import System.IO
import Development.Shake hiding (Env)
import qualified Data.Set as Set

-- import CmdLineParser
-- import DynFlags
-- import Panic
import GHC
import qualified GHC.Paths

import HIE.Bios

-- Set the GHC libdir to the nix libdir if it's present.
getLibdir :: IO FilePath
getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    hPutStrLn stderr "Starting hie-core Demo"
    args <- getArgs
    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger = makeOneLogger $ withLock lock . T.putStrLn

    dir <- getCurrentDirectory
    hPutStrLn stderr dir

    cradle <- findCradle (dir <> "/")

    let options = defaultIdeOptions $ liftIO $ newSession' cradle

    if "--lsp" `elem` args then do
        hPutStrLn stderr "Starting IDE server"
        runLanguageServer logger $ \event vfs -> do
            hPutStrLn stderr "Server started"
            initialise (mainRule >> action kick) event logger options vfs
    else do
        let files = map toNormalizedFilePath $ filter (/= "--lsp") args
        vfs <- makeVFSHandle
        ide <- initialise mainRule (showEvent lock) logger options vfs
        setFilesOfInterest ide $ Set.fromList files
        runAction ide kick
        -- shake now writes an async message that it is completed with timing info,
        -- so we sleep briefly to wait for it to have been written
        sleep 0.01
        putStrLn "Done"


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
newSession' cradle = getLibdir >>= \libdir -> runGhc (Just libdir) $ do
    initializeFlagsWithCradle "" cradle
    getSession
