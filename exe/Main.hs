-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
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
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1               as H
import           Data.ByteString.Base16         (encode)
import qualified Data.ByteString.Char8          as B
import Data.Default
import Data.Either
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
-- import Data.Version
-- import Development.GitRev
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
import Development.Shake                        (Action,  action)
import DynFlags                                 (gopt_set, gopt_unset,
                                                 updOptLevel)
import DynFlags                                 (PackageFlag(..), PackageArg(..))
import GHC hiding                               (def)
import GHC.Check                                (runTimeVersion, compileTimeVersionFromLibdir)
-- import GhcMonad
import HIE.Bios.Cradle
import HIE.Bios.Environment                     (addCmdOpts)
import HIE.Bios.Types
import HscTypes                                 (HscEnv(..), ic_dflags)
import qualified Language.Haskell.LSP.Core as LSP
import Ide.Logger
import Ide.Plugin
import Ide.Plugin.Config
import Ide.Types                                (IdePlugins, ipMap)
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types               (LspId(IdInt))
import Linker                                   (initDynLinker)
import Module
import NameCache
import Packages
-- import Paths_ghcide
import System.Directory
import qualified System.Directory.Extra as IO
-- import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Log.Logger as L
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
#if AGPL
import Ide.Plugin.Brittany                as Brittany
#endif
import Ide.Plugin.Pragmas                 as Pragmas


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

-- -- Set the GHC libdir to the nix libdir if it's present.
-- getLibdir :: IO FilePath
-- getLibdir = fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"

main :: IO ()
main = do
    -- WARNING: If you write to stdout before runLanguageServer
    --          then the language server will not work
    args@Arguments{..} <- getArguments "haskell-language-server"

    if argsVersion then ghcideVersion >>= putStrLn >> exitSuccess
    else hPutStrLn stderr {- see WARNING above -} =<< ghcideVersion

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
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer options (pluginHandler plugins) getInitialConfig getConfigFromNotification $ \getLspId event vfs caps -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            let options = (defaultIdeOptions $ loadSession dir)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    , optTesting        = argsTesting
                    , optInterfaceLoadingDiagnostics = argsTesting
                    , optThreads = argsThread
                    }
            debouncer <- newAsyncDebouncer
            initialise caps (mainRule >> pluginRules plugins >> action kick)
                getLspId event hlsLogger debouncer options vfs
    else do
        -- GHC produces messages with UTF8 in them, so make sure the terminal doesn't error
        hSetEncoding stdout utf8
        hSetEncoding stderr utf8

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
        putStrLn "\nStep 3/6: Initializing the IDE"
        vfs <- makeVFSHandle

        debouncer <- newAsyncDebouncer
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) (logger Info) debouncer (defaultIdeOptions $ loadSession dir) vfs

        putStrLn "\nStep 4/6: Type checking the files"
        setFilesOfInterest ide $ HashSet.fromList $ map toNormalizedFilePath' files
        _ <- runActionSync ide $ uses TypeCheck (map toNormalizedFilePath' files)
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath' "src/Development/IDE/Core/Rules.hs"
--        results <- runActionSync ide $ use TypeCheck $ toNormalizedFilePath' "exe/Main.hs"
        return ()

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
showEvent lock (EventFileDiagnostics (toNormalizedFilePath' -> file) diags) =
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
targetToFile :: [FilePath] -> TargetId -> IO [NormalizedFilePath]
targetToFile is (TargetModule mod) = do
    let fps = [i </> (moduleNameSlashes mod) -<.> ext | ext <- exts, i <- is ]
        exts = ["hs", "hs-boot", "lhs"]
    mapM (fmap toNormalizedFilePath' . canonicalizePath) fps
targetToFile _ (TargetFile f _) = do
  f' <- canonicalizePath f
  return [(toNormalizedFilePath' f')]

setNameCache :: IORef NameCache -> HscEnv -> HscEnv
setNameCache nc hsc = hsc { hsc_NC = nc }

-- This is the key function which implements multi-component support. All
-- components mapping to the same hie,yaml file are mapped to the same
-- HscEnv which is updated as new components are discovered.
loadSession :: FilePath -> Action (FilePath -> Action HscEnvEq)
loadSession dir = liftIO $ do
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
        res' <- traverse IO.makeAbsolute res
        return $ normalise <$> res'

    -- Create a new HscEnv from a hieYaml root and a set of options
    -- If the hieYaml file already has an HscEnv, the new component is
    -- combined with the components in the old HscEnv into a new HscEnv
    -- which contains both.
    packageSetup <- return $ \(hieYaml, cfp, opts) -> do
        -- Parse DynFlags for the newly discovered component
        hscEnv <- emptyHscEnv
        (df, targets) <- evalGhcEnv hscEnv $ do
                          setOptions opts (hsc_dflags hscEnv)
        dep_info <- getDependencyInfo (componentDependencies opts)
        -- Now lookup to see whether we are combining with an exisiting HscEnv
        -- or making a new one. The lookup returns the HscEnv and a list of
        -- information about other components loaded into the HscEnv
        -- (unitId, DynFlag, Targets)
        modifyVar hscEnvs $ \m -> do
            -- Just deps if there's already an HscEnv
            -- Nothing is it's the first time we are making an HscEnv
            let oldDeps = Map.lookup hieYaml m
            let -- Add the raw information about this component to the list
                -- We will modify the unitId and DynFlags used for
                -- compilation but these are the true source of
                -- information.
                new_deps = (thisInstalledUnitId df, df, targets, cfp, dep_info) : maybe [] snd oldDeps
                -- Get all the unit-ids for things in this component
                inplace = map (\(a, _, _, _, _) -> a) new_deps
                -- Remove all inplace dependencies from package flags for
                -- components in this HscEnv
                rearrange (uid, (df, uids), ts, cfp, di) = (uid, (df, uids, ts, cfp, di))
                do_one (uid,df, ts, cfp, di) = rearrange (uid, removeInplacePackages inplace df, ts, cfp, di)
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
              evalGhcEnv hscEnv $ do
                _ <- setSessionDynFlags df
                getSession
            -- Modify the map so the hieYaml now maps to the newly created
            -- HscEnv
            -- Returns
            -- * the new HscEnv so it can be used to modify the
            --   FilePath -> HscEnv map
            -- * The information for the new component which caused this cache miss
            -- * The modified information (without -inplace flags) for
            --   existing packages
            pure (Map.insert hieYaml (newHscEnv, new_deps) m, (newHscEnv, head new_deps', tail new_deps'))


    session <- return $ \(hieYaml, cfp, opts) -> do
        (hscEnv, new, old_deps) <- packageSetup (hieYaml, cfp, opts)
        -- TODO Handle the case where there is no hie.yaml
        -- Make a map from unit-id to DynFlags, this is used when trying to
        -- resolve imports.
        let uids = map (\(iuid, (df, _uis, _targets, _cfp, _di)) -> (iuid, df)) (new : old_deps)

        -- For each component, now make a new HscEnvEq which contains the
        -- HscEnv for the hie.yaml file but the DynFlags for that component
        --
        -- Then look at the targets for each component and create a map
        -- from FilePath to the HscEnv
        let new_cache (_iuid, (df, _uis, targets, cfp, di)) =  do
              let hscEnv' = hscEnv { hsc_dflags = df
                                   , hsc_IC = (hsc_IC hscEnv) { ic_dflags = df } }

              versionMismatch <- evalGhcEnv hscEnv' checkGhcVersion
              henv <- case versionMismatch of
                        Just mismatch -> return mismatch
                        Nothing -> newHscEnvEq hscEnv' uids
              let res = (henv, di)
              print res

              let is = importPaths df
              ctargets <- concatMapM (targetToFile is  . targetId) targets
              -- A special target for the file which caused this wonderful
              -- component to be created.
              let special_target = (cfp, res)
              --pprTraceM "TARGETS" (ppr (map (text . show) ctargets))
              let xs = map (,res) ctargets
              return (special_target:xs, res)

        -- New HscEnv for the component in question
        (cs, res) <- new_cache new
        -- Modified cache targets for everything else in the hie.yaml file
        -- which now uses the same EPS and so on
        cached_targets <- concatMapM (fmap fst . new_cache) old_deps
        modifyVar_ fileToFlags $ \var -> do
            pure $ Map.insert hieYaml (HM.fromList (cs ++ cached_targets)) var

        return res

    lock <- newLock
    cradle_lock <- newLock

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    sessionOpts <- return $ \(hieYaml, file) -> do


        fm <- readVar fileToFlags
        let mv = Map.lookup hieYaml fm
        let v = fromMaybe HM.empty mv
        cfp <- liftIO $ canonicalizePath file
        case HM.lookup (toNormalizedFilePath' cfp) v of
          Just (_, old_di) -> do
            deps_ok <- checkDependencyInfo old_di
            unless deps_ok $ do
              modifyVar_ fileToFlags (const (return Map.empty))
              -- Keep the same name cache
              modifyVar_ hscEnvs (return . Map.adjust (\(h, _) -> (h, [])) hieYaml )
          Nothing -> return ()
        -- We sort so exact matches come first.
        case HM.lookup (toNormalizedFilePath' cfp) v of
            Just opts -> do
                --putStrLn $ "Cached component of " <> show file
                pure (fst opts)
            Nothing-> do
                finished_barrier <- newBarrier
                -- fork a new thread here which won't be killed by shake
                -- throwing an async exception
                void $ forkIO $ withLock cradle_lock $ do
                  putStrLn $ "Shelling out to cabal " <> show file
                  cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle hieYaml
                  opts <- cradleToSessionOpts cradle cfp
                  print opts
                  res <- fst <$> session (hieYaml, toNormalizedFilePath' cfp, opts)
                  signalBarrier finished_barrier res
                waitBarrier finished_barrier
    return $ \file -> liftIO $ mask_ $ withLock lock $ do
              hieYaml <- cradleLoc file
              sessionOpts (hieYaml, file)

checkDependencyInfo :: Map.Map FilePath (Maybe UTCTime) -> IO Bool
checkDependencyInfo old_di = do
  di <- getDependencyInfo (Map.keys old_di)
  return (di == old_di)



getDependencyInfo :: [FilePath] -> IO (Map.Map FilePath (Maybe UTCTime))
getDependencyInfo fs = Map.fromList <$> mapM do_one fs

  where
    do_one fp = do
      exists <- IO.doesFileExist fp
      if exists
        then do
          mtime <- getModificationTime fp
          return (fp, Just mtime)
        else return (fp, Nothing)

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

setOptions :: GhcMonad m => ComponentOptions -> DynFlags -> m (DynFlags, [Target])
setOptions (ComponentOptions theOpts compRoot _) dflags = do
    cacheDir <- liftIO $ getCacheDir theOpts
    (dflags', targets) <- addCmdOpts compRoot theOpts dflags
    let dflags'' =
          -- disabled, generated directly by ghcide instead
          flip gopt_unset Opt_WriteInterface $
          -- disabled, generated directly by ghcide instead
          -- also, it can confuse the interface stale check
          dontWriteHieFiles $
          setHiDir cacheDir $
          setDefaultHieDir cacheDir $
          setIgnoreInterfacePragmas $
          setLinkerOptions $
          disableOptimisation dflags'
    -- initPackages parses the -package flags and
    -- sets up the visibility for each component.
    (final_df, _) <- liftIO $ initPackages dflags''
--    let df'' = gopt_unset df' Opt_WarnIsError
    return (final_df, targets)


-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscNothing
  , ghcMode = CompManager
  }

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

getCacheDir :: [String] -> IO FilePath
getCacheDir opts = IO.getXdgDirectory IO.XdgCache (cacheDir </> opts_hash)
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- Prefix for the cache path
cacheDir :: String
cacheDir = "ghcide"

compileTimeGhcVersion :: Version
compileTimeGhcVersion = $$(compileTimeVersionFromLibdir getLibdir)

checkGhcVersion :: Ghc (Maybe HscEnvEq)
checkGhcVersion = do
    v <- runTimeVersion
    return $ if v == Just compileTimeGhcVersion
        then Nothing
        else Just GhcVersionMismatch {compileTime = compileTimeGhcVersion, runTime = v}
