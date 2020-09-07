-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE TemplateHaskell #-}

module Main(main) where

import Arguments
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Lens ( (^.) )
import Data.Default
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version
import Development.IDE.Core.Debouncer
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
import Development.IDE.Plugin
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.Plugin.CodeAction as CodeAction
import Development.IDE.Plugin.Test as Test
import Development.IDE.Session
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens (params, initializationOptions)
import Development.IDE.LSP.LanguageServer
import qualified System.Directory.Extra as IO
import System.Environment
import System.IO
import System.Info
import System.Exit
import System.FilePath
import System.Time.Extra
import Paths_ghcide
import Development.GitRev
import qualified Data.HashSet as HashSet
import qualified Data.Aeson as J

import HIE.Bios.Cradle

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> showVersion compilerVersion
             <> ") (PATH: " <> path <> ")"
             <> gitHashSection

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd IO.setCurrentDirectory

    dir <- IO.getCurrentDirectory
    command <- makeLspCommandId "typesignature.add"

    let plugins = Completions.plugin <> CodeAction.plugin
            <> if argsTesting then Test.plugin else mempty
        onInitialConfiguration :: InitializeRequest -> Either T.Text LspConfig
        onInitialConfiguration x = case x ^. params . initializationOptions of
          Nothing -> Right defaultLspConfig
          Just v -> case J.fromJSON v of
            J.Error err -> Left $ T.pack err
            J.Success a -> Right a
        onConfigurationChange = const $ Left "Updating Not supported"
        options = def { LSP.executeCommandCommands = Just [command]
                      , LSP.completionTriggerCharacters = Just "."
                      }

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer options (pluginHandler plugins) onInitialConfiguration onConfigurationChange $ \getLspId event vfs caps wProg wIndefProg getConfig rootPath -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            sessionLoader <- loadSession $ fromMaybe dir rootPath
            config <- fromMaybe defaultLspConfig <$> getConfig
            let options = (defaultIdeOptions sessionLoader)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    , optTesting        = IdeTesting argsTesting
                    , optThreads        = argsThreads
                    , optCheckParents   = checkParents config
                    , optCheckProject   = checkProject config
                    }
                logLevel = if argsVerbose then minBound else Info
            debouncer <- newAsyncDebouncer
            initialise caps (mainRule >> pluginRules plugins)
                getLspId event wProg wIndefProg (logger logLevel) debouncer options vfs
    else do
        -- GHC produces messages with UTF8 in them, so make sure the terminal doesn't error
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        putStrLn $ "Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/digital-asset/ghcide/issues"

        putStrLn $ "\nStep 1/4: Finding files to test in " ++ dir
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        files <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/4: Looking for hie.yaml files that control setup"
        cradles <- mapM findCradle files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        when (n > 0) $ putStrLn $ "  (" ++ intercalate ", " (catMaybes ucradles) ++ ")"
        putStrLn "\nStep 3/4: Initializing the IDE"
        vfs <- makeVFSHandle
        debouncer <- newAsyncDebouncer
        let logLevel = if argsVerbose then minBound else Info
            dummyWithProg _ _ f = f (const (pure ()))
        sessionLoader <- loadSession dir
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) dummyWithProg (const (const id)) (logger logLevel) debouncer (defaultIdeOptions sessionLoader)  vfs

        putStrLn "\nStep 4/4: Type checking the files"
        setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath' files
        results <- runAction "User TypeCheck" ide $ uses TypeCheck (map toNormalizedFilePath' files)
        let (worked, failed) = partition fst $ zip (map isJust results) files
        when (failed /= []) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        let files xs = let n = length xs in if n == 1 then "1 file" else show n ++ " files"
        putStrLn $ "\nCompleted (" ++ files worked ++ " worked, " ++ files failed ++ " failed)"
        unless (null failed) (exitWith $ ExitFailure (length failed))

expandFiles :: [FilePath] -> IO [FilePath]
expandFiles = concatMapM $ \x -> do
    b <- IO.doesFileExist x
    if b then return [x] else do
        let recurse "." = True
            recurse x | "." `isPrefixOf` takeFileName x = False -- skip .git etc
            recurse x = takeFileName x `notElem` ["dist","dist-newstyle"] -- cabal directories
        files <- filter (\x -> takeExtension x `elem` [".hs",".lhs"]) <$> IO.listFilesInside (return . recurse) x
        when (null files) $
            fail $ "Couldn't find any .hs/.lhs files inside directory: " ++ x
        return files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath' -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e
