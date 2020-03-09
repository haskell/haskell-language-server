-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.IORef
import NameCache
import Packages
import Module
import Arguments
import Data.Maybe
import Data.List.Extra
import System.FilePath
import Control.Concurrent.Extra
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Default
import System.Time.Extra
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
import Development.IDE.GHC.Util
import Development.IDE.Plugin
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.Plugin.CodeAction as CodeAction
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types (LspId(IdInt))
import Linker
import Development.IDE.LSP.LanguageServer
import System.Directory.Extra as IO
import System.IO
import System.Exit
import HIE.Bios.Environment
import Development.Shake (Action, action)
import qualified Data.HashSet as HashSet
import qualified Data.Map.Strict as Map
import Data.Either

import GhcMonad
import HscTypes (HscEnv(..), ic_dflags)
import DynFlags (PackageFlag(..), PackageArg(..), PackageDBFlag(..), gopt_unset)
import GHC hiding (def)

import HIE.Bios
import HIE.Bios.Types

-- ---------------------------------------------------------------------

import Ide.Plugin.Example                 as Example
import Ide.Plugin.Floskell                as Floskell
import Ide.Plugin.Ormolu                  as Ormolu
import Ide.Plugin.Formatter
import Ide.Plugin.Config
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
    if includeExample then Example.plugin else mempty

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

    whenJust argsCwd setCurrentDirectory

    dir <- getCurrentDirectory

    let plugins = idePlugins argsExamplePlugin

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting haskell-language-server LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer def (pluginHandler plugins) getInitialConfig getConfigFromNotification $ \getLspId event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            -- very important we only call loadSession once, and it's fast, so just do it before starting
            session <- loadSession dir
            let options = (defaultIdeOptions $ return session)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    }
            debouncer <- newAsyncDebouncer
            initialise caps (mainRule >> pluginRules plugins >> action kick) getLspId event (logger minBound) debouncer options vfs
    else do
        putStrLn $ "haskell-language-server setup tester in " ++ dir ++ "."
        putStrLn "Report bugs at https://github.com/haskell/haskell-language-server/issues"

        putStrLn $ "\nStep 1/6: Finding files to test in " ++ dir
        files <- expandFiles (argFiles ++ ["." | null argFiles])
        -- LSP works with absolute file paths, so try and behave similarly
        files <- nubOrd <$> mapM canonicalizePath files
        putStrLn $ "Found " ++ show (length files) ++ " files"

        putStrLn "\nStep 2/6: Looking for hie.yaml files that control setup"
        cradles <- mapM findCradle files
        let ucradles = nubOrd cradles
        let n = length ucradles
        putStrLn $ "Found " ++ show n ++ " cradle" ++ ['s' | n /= 1]
        putStrLn "\nStep 3/6: Initializing the IDE"
        vfs <- makeVFSHandle
        grab <- loadSession dir
        debouncer <- newAsyncDebouncer
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) (logger minBound) debouncer (defaultIdeOptions $ return grab) vfs

        putStrLn "\nStep 4/6: Type checking the files"
        setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath files
        _ <- runActionSync ide $ uses TypeCheck (map toNormalizedFilePath files)
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath "src/Development/IDE/Core/Rules.hs"
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath "exe/Main.hs"
        return ()


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
    void $ uses TypeCheck $ HashSet.toList files

-- | Print an LSP event.
showEvent :: Lock -> FromServerMessage -> IO ()
showEvent _ (EventFileDiagnostics _ []) = return ()
showEvent lock (EventFileDiagnostics (toNormalizedFilePath -> file) diags) =
    withLock lock $ T.putStrLn $ showDiagnosticsColored $ map (file,ShowDiag,) diags
showEvent lock e = withLock lock $ print e


cradleToSessionOpts :: Cradle a -> FilePath -> IO ComponentOptions
cradleToSessionOpts cradle file = do
    let showLine s = putStrLn ("> " ++ s)
    cradleRes <- runCradle (cradleOptsProg cradle) showLine file
    opts <- case cradleRes of
        CradleSuccess r -> pure r
        CradleFail err -> throwIO err
        -- TODO Rather than failing here, we should ignore any files that use this cradle.
        -- That will require some more changes.
        CradleNone -> fail "'none' cradle is not yet supported"
    pure opts

emptyHscEnv :: IO HscEnv
emptyHscEnv = do
    libdir <- getLibdir
    env <- runGhc (Just libdir) getSession
    initDynLinker env
    pure env

-- Convert a target to a list of potential absolute paths.
-- A TargetModule can be anywhere listed by the supplied include
-- directories
-- A target file is a relative path but with a specific prefix so just need
-- to canonicalise it.
targetToFile :: [FilePath] -> TargetId -> IO [(NormalizedFilePath, Bool)]
targetToFile is (TargetModule mod) = forM is $ \i -> do
    let fp = i </> (moduleNameSlashes mod) -<.> "hs"
    cfp <- canonicalizePath fp
    return (toNormalizedFilePath cfp, False)
targetToFile _ (TargetFile f _) = do
  f' <- canonicalizePath f
  return [(toNormalizedFilePath f', True)]

setNameCache :: IORef NameCache -> HscEnv -> HscEnv
setNameCache nc hsc = hsc { hsc_NC = nc }

loadSession :: FilePath -> IO (FilePath -> Action HscEnvEq)
loadSession dir = do
    -- Mapping from hie.yaml file to HscEnv, one per hie.yaml file
    hscEnvs <- newVar Map.empty
    -- Mapping from a filepath to HscEnv
    fileToFlags <- newVar Map.empty
    -- This caches the mapping from Mod.hs -> hie.yaml
    cradleLoc <- memoIO $ \v -> do
        res <- findCradle v
        -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
        -- try and normalise that
        -- e.g. see https://github.com/digital-asset/ghcide/issues/126
        res' <- traverse makeAbsolute res
        return $ normalise <$> res'

    -- Create a new HscEnv from a set of options
    packageSetup <- return $ \(hieYaml, opts) -> do
        -- Parse DynFlags for the newly discovered component
        hscEnv <- emptyHscEnv
        (df, targets) <- runGhcEnv hscEnv $ do
                          (df, target) <- addCmdOpts (componentOptions opts) (hsc_dflags hscEnv)
                          -- initPackages parses the -package flags and
                          -- sets up the visibility for each component.
                          (df', _) <- liftIO $ initPackages df
                          let df'' = gopt_unset df' Opt_WarnIsError
                          return (df'' , target)
        -- Now lookup to see whether we are adding to an exisiting HscEnv
        -- or making a new one. The lookup returns the HscEnv and a list of
        -- information about other components loaded into the HscEnv
        -- (unitId, DynFlag, Targets)
        modifyVar hscEnvs $ \m -> do
            -- Just deps if there's already an HscEnv
            -- Nothing is it's the first time we are making an HscEnv
            let oldDeps = Map.lookup hieYaml m
            let -- Add the raw information about this component to the list
                -- We will modify the unitId and DynFlags used for
                -- compilation but these are the true source of information
                new_deps = (thisInstalledUnitId df, df, targets) : maybe [] snd oldDeps
                -- Get all the unit-ids for things in this component
                inplace = map (\(a, _, _) -> a) new_deps
                -- Remove all inplace dependencies from package flags for
                -- components in this HscEnv
                do_one (uid,df, ts) = (uid, removeInplacePackages inplace df, ts)
                -- All deps, but without any packages which are also loaded
                -- into memory
                new_deps' = map do_one new_deps
            -- Make a new HscEnv, we have to recompile everything from
            -- scratch again (for now)
            -- It's important to keep the same NameCache though for reasons
            -- that I do not fully understand
            print ("Making new HscEnv" ++ (show inplace))
            hscEnv <- case oldDeps of
                        Nothing -> emptyHscEnv
                        Just (old_hsc, _) -> setNameCache (hsc_NC old_hsc) <$> emptyHscEnv
            newHscEnv <-
              -- Add the options for the current component to the HscEnv
              runGhcEnv hscEnv $ do
                _ <- setSessionDynFlags df
                getSession
            -- Now overwrite the other dflags options but with an
            -- initialised package state to get a proper HscEnv for each
            -- component
            let do_one' (uid, (df, uids), ts) = (uid, (df, uids, ts))
                new_deps'' = map do_one' new_deps'

            pure (Map.insert hieYaml (newHscEnv, new_deps) m, (newHscEnv, head new_deps'', tail new_deps''))


    session <- return $ \(hieYaml, opts) -> do
        (hscEnv, new, old_deps) <- packageSetup (hieYaml, opts)
        -- TODO Handle the case where there is no hie.yaml
        let uids = map (\(iuid, (df, _uis, _targets)) -> (iuid, df)) (new : old_deps)

        -- For each component, now make a new HscEnvEq which contains the
        -- HscEnv for the hie.yaml file but the DynFlags for that component
        --
        -- Then look at the targets for each component and create a map
        -- from FilePath to the HscEnv
        let new_cache (_iuid, (df, _uis, targets)) =  do
              let hscEnv' = hscEnv { hsc_dflags = df
                                   , hsc_IC = (hsc_IC hscEnv) { ic_dflags = df } }

              res <- newHscEnvEq hscEnv' uids

              let is = importPaths df
              ctargets <- concatMapM (targetToFile is  . targetId) targets
              --pprTraceM "TARGETS" (ppr (map (text . show) ctargets))
              let xs = map (,res) ctargets
              return (xs, res)

        -- New HscEnv for the component in question
        (cs, res) <- new_cache new
        -- Modified cache targets for everything else in the hie.yaml file
        -- which now uses the same EPS and so on
        cached_targets <- concatMapM (fmap fst . new_cache) old_deps
        modifyVar_ fileToFlags $ \var -> do
            pure $ Map.insert hieYaml (cs ++ cached_targets) var
        return res

    lock <- newLock

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    sessionOpts <- return $ \(hieYaml, file) -> do
        fm <- readVar fileToFlags
        let mv = Map.lookup hieYaml fm
        let v = fromMaybe [] mv
        cfp <- liftIO $ canonicalizePath file
        -- We sort so exact matches come first.
        case find (\((f', exact), _) -> fromNormalizedFilePath f' == cfp || not exact && fromNormalizedFilePath f' `isSuffixOf` cfp) v of
            Just (_, opts) -> do
                putStrLn $ "Cached component of " <> show file
                pure opts
            Nothing-> do
                putStrLn $ "Shelling out to cabal " <> show file
                cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle hieYaml
                opts <- cradleToSessionOpts cradle file
                print opts
                session (hieYaml, opts)
    return $ \file -> liftIO $ withLock lock $ do
        hieYaml <- cradleLoc file
        sessionOpts (hieYaml, file)

-- This function removes all the -package flags which refer to packages we
-- are going to deal with ourselves. For example, if a executable depends
-- on a library component, then this function will remove the library flag
-- from the package flags for the executable
--
-- There are several places in GHC (for example the call to hptInstances in
-- tcRnImports) which assume that all modules in the HPT have the same unit
-- ID. Therefore we create a fake one and give them all the same unit id.
removeInplacePackages :: [InstalledUnitId] -> DynFlags -> (DynFlags, [InstalledUnitId])
removeInplacePackages us df = (df { packageFlags = ps
                                  , thisInstalledUnitId = fake_uid }, uids)
  where
    (uids, ps) = partitionEithers (map go (packageFlags df))
    fake_uid = toInstalledUnitId (stringToUnitId "fake_uid")
    go p@(ExposePackage _ (UnitIdArg u) _) = if (toInstalledUnitId u `elem` us) then Left (toInstalledUnitId u) else Right p
    go p = Right p

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

instance Show PackageDBFlag where
  show (PackageDB _) = "pkdb"
  show (NoUserPackageDB) = "no-user"
  show (NoGlobalPackageDB) = "no-global"
  show (ClearPackageDBs)  = "clear"
