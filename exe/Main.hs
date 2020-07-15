-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-} -- To get precise GHC version and check if distributed binary
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Arguments
import Control.Concurrent.Extra
import Control.Monad.Extra
import Data.Default
import qualified Data.HashSet as HashSet
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Development.IDE.Core.Debouncer
import Development.IDE.Core.FileStore
import Development.IDE.Core.OfInterest
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Rules
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.LSP.LanguageServer
import Development.IDE.LSP.Protocol
import Development.IDE.Plugin
import Development.IDE.Session
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.IDE.Types.Options
import HIE.Bios.Cradle
import qualified Language.Haskell.LSP.Core as LSP
import Ide.Logger
import Ide.Plugin
import Ide.Plugin.Config
import Ide.Types                                (IdePlugins, ipMap)
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified System.Directory.Extra as IO
import System.Exit
import System.FilePath
import System.IO
import qualified System.Log.Logger as L
import System.Time.Extra

-- ---------------------------------------------------------------------
-- ghcide partialhandlers
import Development.IDE.Plugin.CodeAction  as CodeAction
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.LSP.HoverDefinition as HoverDefinition

 -- haskell-language-server plugins
import Ide.Plugin.Example                 as Example
import Ide.Plugin.Example2                as Example2
import Ide.Plugin.GhcIde                  as GhcIde
import Ide.Plugin.Floskell                as Floskell
import Ide.Plugin.Ormolu                  as Ormolu
import Ide.Plugin.StylishHaskell          as StylishHaskell
#if AGPL
import Ide.Plugin.Brittany                as Brittany
#endif
import Ide.Plugin.Pragmas                 as Pragmas
-- import Data.Typeable (Typeable)


-- ---------------------------------------------------------------------



-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Bool -> IdePlugins
idePlugins includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins = if includeExamples
                   then basePlugins ++ examplePlugins
                   else basePlugins
    basePlugins =
      [
      --   applyRefactDescriptor "applyrefact"
      -- , haddockDescriptor     "haddock"
      -- , hareDescriptor        "hare"
      -- , hsimportDescriptor    "hsimport"
      -- , liquidDescriptor      "liquid"
      -- , packageDescriptor     "package"
      GhcIde.descriptor  "ghcide"
      , Pragmas.descriptor  "pragmas"
      , Floskell.descriptor "floskell"
      -- , genericDescriptor     "generic"
      -- , ghcmodDescriptor      "ghcmod"
      , Ormolu.descriptor   "ormolu"
      , StylishHaskell.descriptor "stylish-haskell"
#if AGPL
      , Brittany.descriptor    "brittany"
#endif
      ]
    examplePlugins =
      [Example.descriptor  "eg"
      ,Example2.descriptor "eg2"
      -- ,hfaAlignDescriptor "hfaa"
      ]

ghcIdePlugins :: T.Text -> IdePlugins -> (Plugin Config, [T.Text])
ghcIdePlugins pid ps = (asGhcIdePlugin ps, allLspCmdIds' pid ps)

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    args@Arguments{..} <- getArguments "haskell-language-server"

    hlsVer <- haskellLanguageServerVersion
    if argsVersion then putStrLn hlsVer
    else hPutStrLn stderr hlsVer {- see WARNING above -}

    LSP.setupLogger argsLogFile ["hls", "hie-bios"]
      $ if argsDebugOn then L.DEBUG else L.INFO

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd IO.setCurrentDirectory

    dir <- IO.getCurrentDirectory

    pid <- getPid
    let
        idePlugins' = idePlugins argsExamplePlugin
        (ps, commandIds) = ghcIdePlugins pid idePlugins'
        plugins = Completions.plugin <> CodeAction.plugin <>
                  Plugin mempty HoverDefinition.setHandlersDefinition <>
                  ps
        options = def { LSP.executeCommandCommands = Just commandIds
                      , LSP.completionTriggerCharacters = Just "."
                      }

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting (haskell-language-server)LSP server..."
        hPutStrLn stderr $ "  with arguments: " <> show args
        hPutStrLn stderr $ "  with plugins: " <> show (Map.keys $ ipMap idePlugins')
        hPutStrLn stderr $ "  in directory: " <> dir
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer options (pluginHandler plugins) getInitialConfig getConfigFromNotification $ \getLspId event vfs caps wProg wIndefProg -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            sessionLoader <- loadSession dir
            let options = (defaultIdeOptions sessionLoader)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    , optTesting        = IdeTesting argsTesting
                    , optThreads        = argsThreads
                    }
            debouncer <- newAsyncDebouncer
            initialise caps (mainRule >> pluginRules plugins)
                getLspId event wProg wIndefProg hlsLogger debouncer options vfs
    else do
        -- GHC produces messages with UTF8 in them, so make sure the terminal doesn't error
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

        putStrLn $ "(haskell-language-server)Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/haskell/haskell-language-server/issues"

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
        putStrLn "\nStep 3/4: Initializing the IDE"
        vfs <- makeVFSHandle
        debouncer <- newAsyncDebouncer
        let dummyWithProg _ _ f = f (const (pure ()))
        sessionLoader <- loadSession dir
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) dummyWithProg (const (const id)) (logger Info)     debouncer (defaultIdeOptions sessionLoader) vfs

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
