-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main(main) where

import Arguments
import Control.Concurrent.Extra
import Control.DeepSeq (NFData)
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Binary (Binary)
import Data.Default
import Data.Dynamic (Typeable)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable)
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
import Development.IDE.GHC.Util
import Development.IDE.LSP.LanguageServer
import Development.IDE.LSP.Protocol
import Development.IDE.Plugin
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.IDE.Types.Options
import Development.Shake (Action, RuleResult, Rules, action, doesFileExist, need)
import GHC hiding (def)
import GHC.Generics (Generic)
-- import qualified GHC.Paths
import HIE.Bios
import HIE.Bios.Cradle
import HIE.Bios.Types
import Ide.Plugin
import Ide.Plugin.Config
-- import Ide.Plugin.Formatter
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types (LspId(IdInt))
import Linker
import qualified System.Directory.Extra as IO
-- import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Time.Extra

-- ---------------------------------------------------------------------

import Development.IDE.Plugin.CodeAction  as CodeAction
import Development.IDE.Plugin.Completions as Completions
import Ide.Plugin.Example                 as Example
import Ide.Plugin.Example2                as Example2
import Ide.Plugin.Floskell                as Floskell
import Ide.Plugin.Ormolu                  as Ormolu

-- ---------------------------------------------------------------------

-- The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.
idePlugins :: Bool -> Plugin Config
idePlugins includeExample
  = Completions.plugin <>
    CodeAction.plugin <>
    formatterPlugins [("ormolu",   Ormolu.provider)
                     ,("floskell", Floskell.provider)] <>
    codeActionPlugins [("eg",  Example.codeAction)
                      ,("eg2", Example2.codeAction)] <>
    hoverPlugins [Example.hover, Example2.hover] <>
    if includeExample then Example.plugin <> Example2.plugin
                      else mempty

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    Arguments{..} <- getArguments "haskell-language-server"

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

    -- lock to avoid overlapping output on stdout
    lock <- newLock
    let logger p = Logger $ \pri msg -> when (pri >= p) $ withLock lock $
            T.putStrLn $ T.pack ("[" ++ upper (show pri) ++ "] ") <> msg

    whenJust argsCwd IO.setCurrentDirectory

    dir <- IO.getCurrentDirectory

    let plugins = idePlugins argsExamplePlugin

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting (haskell-language-server)LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer def (pluginHandler plugins) getInitialConfig getConfigFromNotification $ \getLspId event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            let options = (defaultIdeOptions $ loadSession dir)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    }
            debouncer <- newAsyncDebouncer
            initialise caps (loadGhcSessionIO >> mainRule >> pluginRules plugins >> action kick)
                getLspId event (logger minBound) debouncer options vfs
    else do
        putStrLn $ "(haskell-language-server)Ghcide setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/haskell/haskell-language-server/issues"

        putStrLn $ "\nStep 1/6: Finding files to test in " ++ dir
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        files <- nubOrd <$> mapM IO.canonicalizePath files
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/6: Looking for hie.yaml files that control setup"
        cradles <- mapM findCradle files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        sessions <- forM (zipFrom (1 :: Int) ucradles) $ \(i, x) -> do
            let msg = maybe ("Implicit cradle for " ++ dir) ("Loading " ++) x
            putStrLn $ "\nStep 3/6, Cradle " ++ show i ++ "/" ++ show n ++ ": " ++ msg
            cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle x
            when (isNothing x) $ print cradle
            putStrLn $ "\nStep 4/6, Cradle " ++ show i ++ "/" ++ show n ++ ": Loading GHC Session"
            opts <- getComponentOptions cradle
            createSession opts

        putStrLn "\nStep 5/6: Initializing the IDE"
        vfs <- makeVFSHandle
        let cradlesToSessions = Map.fromList $ zip ucradles sessions
        let filesToCradles = Map.fromList $ zip files cradles
        let grab file = fromMaybe (head sessions) $ do
                cradle <- Map.lookup file filesToCradles
                Map.lookup cradle cradlesToSessions

        let options =
              (defaultIdeOptions $ return $ return . grab)
                    { optShakeProfiling = argsShakeProfiling }
        ide <- initialise def (loadGhcSessionIO >> mainRule) (pure $ IdInt 0) (showEvent lock) (logger Info) noopDebouncer options vfs

        putStrLn "\nStep 6/6: Type checking the files"
        setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath files
        results <- runActionSync ide $ uses TypeCheck $ map toNormalizedFilePath files
        let (worked, failed) = partition fst $ zip (map isJust results) files
        when (failed /= []) $
            putStr $ unlines $ "Files that failed:" : map ((++) " * " . snd) failed

        let files xs = let n = length xs in if n == 1 then "1 file" else show n ++ " files"
        putStrLn $ "\nCompleted (" ++ files worked ++ " worked, " ++ files failed ++ " failed)"

        unless (null failed) exitFailure


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


kick :: Action ()
kick = do
    files <- getFilesOfInterest
    void $ uses TypeCheck $ HashSet.toList files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e


-- Rule type for caching GHC sessions.
type instance RuleResult GetHscEnv = HscEnvEq

data GetHscEnv = GetHscEnv
    { hscenvOptions :: [String]        -- componentOptions from hie-bios
    , hscenvDependencies :: [FilePath] -- componentDependencies from hie-bios
    }
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetHscEnv
instance NFData   GetHscEnv
instance Binary   GetHscEnv


loadGhcSessionIO :: Rules ()
loadGhcSessionIO =
    -- This rule is for caching the GHC session. E.g., even when the cabal file
    -- changed, if the resulting flags did not change, we would continue to use
    -- the existing session.
    defineNoFile $ \(GetHscEnv opts deps) ->
        liftIO $ createSession $ ComponentOptions opts deps


getComponentOptions :: Cradle a -> IO ComponentOptions
getComponentOptions cradle = do
    let showLine s = putStrLn ("> " ++ s)
    cradleRes <- runCradle (cradleOptsProg cradle) showLine ""
    case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err -> throwIO err
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone -> fail "'none' cradle is not yet supported"


createSession :: ComponentOptions -> IO HscEnvEq
createSession opts = do
    libdir <- getLibdir
    env <- runGhc (Just libdir) $ do
        _targets <- initSession opts
        getSession
    initDynLinker env
    newHscEnvEq env


cradleToSession :: Maybe FilePath -> Cradle a -> Action HscEnvEq
cradleToSession mbYaml cradle = do
    cmpOpts <- liftIO $ getComponentOptions cradle
    let opts = componentOptions cmpOpts
        deps = componentDependencies cmpOpts
        deps' = case mbYaml of
                  -- For direct cradles, the hie.yaml file itself must be watched.
                  Just yaml | isDirectCradle cradle -> yaml : deps
                  _ -> deps
    existingDeps <- filterM doesFileExist deps'
    need existingDeps
    useNoFile_ $ GetHscEnv opts deps


loadSession :: FilePath -> Action (FilePath -> Action HscEnvEq)
loadSession dir = liftIO $ do
    cradleLoc <- memoIO $ \v -> do
        res <- findCradle v
        -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
        -- try and normalise that
        -- e.g. see https://github.com/digital-asset/ghcide/issues/126
        res' <- traverse IO.makeAbsolute res
        return $ normalise <$> res'
    let session :: Maybe FilePath -> Action HscEnvEq
        session file = do
          c <- liftIO $ maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle file
          cradleToSession file c
    return $ \file -> session =<< liftIO (cradleLoc file)


-- | Memoize an IO function, with the characteristics:
--
--   * If multiple people ask for a result simultaneously, make sure you only compute it once.
--
--   * If there are exceptions, repeatedly reraise them.
--
--   * If the caller is aborted (async exception) finish computing it anyway.
memoIO :: Ord a => (a -> IO b) -> IO (a -> IO b)
memoIO op = do
    ref <- newVar Map.empty
    return $ \k -> join $ mask_ $ modifyVar ref $ \mp ->
        case Map.lookup k mp of
            Nothing -> do
                res <- onceFork $ op k
                return (Map.insert k res mp, res)
            Just res -> return (mp, res)
