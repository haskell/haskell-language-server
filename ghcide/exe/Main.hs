-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -Wno-dodgy-imports #-} -- GHC no longer exports def in GHC 8.6 and above
{-# LANGUAGE CPP #-} -- To get precise GHC version
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main(main) where

import Linker (initDynLinker)
import Data.IORef
import NameCache
import Packages
import Module
import Arguments
import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Exception.Safe
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor(second))
import Data.Default
import Data.Either
import Data.Function
import Data.List.Extra
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock (UTCTime)
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
import Development.IDE.GHC.Util
import Development.IDE.Plugin
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.Plugin.CodeAction as CodeAction
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Data.Aeson (ToJSON(toJSON))
import Development.IDE.LSP.LanguageServer
import qualified System.Directory.Extra as IO
import System.Environment
import System.Info
import System.IO
import System.Exit
import System.FilePath
import System.Directory
import System.Time.Extra
import HIE.Bios.Environment (addCmdOpts, makeDynFlagsAbsolute, getRuntimeGhcLibDir)
import Paths_ghcide
import Development.GitRev
import Development.Shake (Action)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Crypto.Hash.SHA1 as H
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Base16 (encode)
import DynFlags (gopt_set, gopt_unset, updOptLevel, PackageFlag(..), PackageArg(..))
import GhcMonad
import HscTypes (HscEnv(..), ic_dflags)
import GHC hiding (def)
import GHC.Check
-- Only use this for checking against the compile time GHC libDir!
-- Use getRuntimeGhcLibDir from hie-bios instead for everything else
-- otherwise binaries will not be distributable since paths will be baked into them
import qualified GHC.Paths
import Data.Either.Extra

import HIE.Bios.Cradle
import HIE.Bios.Types

ghcideVersion :: IO String
ghcideVersion = do
  path <- getExecutablePath
  let gitHashSection = case $(gitHash) of
        x | x == "UNKNOWN" -> ""
        x -> " (GIT hash: " <> x <> ")"
  return $ "ghcide version: " <> showVersion version
             <> " (GHC: " <> VERSION_ghc
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
        onInitialConfiguration = const $ Right ()
        onConfigurationChange  = const $ Right ()
        options = def { LSP.executeCommandCommands = Just [command]
                      , LSP.completionTriggerCharacters = Just "."
                      }

    if argLSP then do
        t <- offsetTime
        hPutStrLn stderr "Starting LSP server..."
        hPutStrLn stderr "If you are seeing this in a terminal, you probably should have run ghcide WITHOUT the --lsp option!"
        runLanguageServer options (pluginHandler plugins) onInitialConfiguration onConfigurationChange $ \getLspId event vfs caps wProg wIndefProg -> do
            t <- t
            hPutStrLn stderr $ "Started LSP server in " ++ showDuration t
            sessionLoader <- loadSession dir
            let options = (defaultIdeOptions sessionLoader)
                    { optReportProgress = clientSupportsProgress caps
                    , optShakeProfiling = argsShakeProfiling
                    , optTesting        = IdeTesting argsTesting
                    , optThreads        = argsThreads
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
        putStrLn "\nStep 3/4: Initializing the IDE"
        vfs <- makeVFSHandle
        debouncer <- newAsyncDebouncer
        let dummyWithProg _ _ f = f (const (pure ()))
        sessionLoader <- loadSession dir
        ide <- initialise def mainRule (pure $ IdInt 0) (showEvent lock) dummyWithProg (const (const id)) (logger minBound) debouncer (defaultIdeOptions sessionLoader)  vfs

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


-- | Run the specific cradle on a specific FilePath via hie-bios.
-- This then builds dependencies or whatever based on the cradle, gets the
-- GHC options/dynflags needed for the session and the GHC library directory
cradleToOptsAndLibDir :: Show a => Cradle a -> FilePath
                      -> IO (Either [CradleError] (ComponentOptions, FilePath))
cradleToOptsAndLibDir cradle file = do
    -- Start off by getting the session options
    let showLine s = hPutStrLn stderr ("> " ++ s)
    hPutStrLn stderr $ "Output from setting up the cradle " <> show cradle
    cradleRes <- runCradle (cradleOptsProg cradle) showLine file
    case cradleRes of
        CradleSuccess r -> do
            -- Now get the GHC lib dir
            libDirRes <- getRuntimeGhcLibDir cradle
            case libDirRes of
                -- This is the successful path
                CradleSuccess libDir -> pure (Right (r, libDir))
                CradleFail err -> return (Left [err])
                -- For the None cradle perhaps we still want to report an Info
                -- message about the fact that the file is being ignored.
                CradleNone -> return (Left [])

        CradleFail err -> return (Left [err])
        -- Same here
        CradleNone -> return (Left [])

emptyHscEnv :: IORef NameCache -> FilePath -> IO HscEnv
emptyHscEnv nc libDir = do
    env <- runGhc (Just libDir) getSession
    initDynLinker env
    pure $ setNameCache nc env

-- | Convert a target to a list of potential absolute paths.
-- A TargetModule can be anywhere listed by the supplied include
-- directories
-- A target file is a relative path but with a specific prefix so just need
-- to canonicalise it.
targetToFile :: [FilePath] -> TargetId -> IO [NormalizedFilePath]
targetToFile is (TargetModule mod) = do
    let fps = [i </> moduleNameSlashes mod -<.> ext | ext <- exts, i <- is ]
        exts = ["hs", "hs-boot", "lhs"]
    mapM (fmap toNormalizedFilePath' . canonicalizePath) fps
targetToFile _ (TargetFile f _) = do
  f' <- canonicalizePath f
  return [toNormalizedFilePath' f']

setNameCache :: IORef NameCache -> HscEnv -> HscEnv
setNameCache nc hsc = hsc { hsc_NC = nc }

-- | This is the key function which implements multi-component support. All
-- components mapping to the same hie.yaml file are mapped to the same
-- HscEnv which is updated as new components are discovered.
loadSession :: FilePath -> IO (Action IdeGhcSession)
loadSession dir = do
  -- Mapping from hie.yaml file to HscEnv, one per hie.yaml file
  hscEnvs <- newVar Map.empty :: IO (Var HieMap)
  -- Mapping from a Filepath to HscEnv
  fileToFlags <- newVar Map.empty :: IO (Var FlagsMap)
  -- Version of the mappings above
  version <- newVar 0
  let returnWithVersion fun = IdeGhcSession fun <$> liftIO (readVar version)
  let invalidateShakeCache = do
        modifyVar_ version (return . succ)
  -- This caches the mapping from Mod.hs -> hie.yaml
  cradleLoc <- liftIO $ memoIO $ \v -> do
      res <- findCradle v
      -- Sometimes we get C:, sometimes we get c:, and sometimes we get a relative path
      -- try and normalise that
      -- e.g. see https://github.com/digital-asset/ghcide/issues/126
      res' <- traverse IO.makeAbsolute res
      return $ normalise <$> res'

  dummyAs <- async $ return (error "Uninitialised")
  runningCradle <- newVar dummyAs :: IO (Var (Async (IdeResult HscEnvEq,[FilePath])))

  return $ do
    ShakeExtras{logger, eventer, restartShakeSession, withIndefiniteProgress, ideNc} <- getShakeExtras
    IdeOptions{optTesting = IdeTesting optTesting} <- getIdeOptions

    -- Create a new HscEnv from a hieYaml root and a set of options
    -- If the hieYaml file already has an HscEnv, the new component is
    -- combined with the components in the old HscEnv into a new HscEnv
    -- which contains the union.
    let packageSetup :: (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath)
                     -> IO (HscEnv, ComponentInfo, [ComponentInfo])
        packageSetup (hieYaml, cfp, opts, libDir) = do
          -- Parse DynFlags for the newly discovered component
          hscEnv <- emptyHscEnv ideNc libDir
          (df, targets) <- evalGhcEnv hscEnv $
              setOptions opts (hsc_dflags hscEnv)
          let deps = componentDependencies opts ++ maybeToList hieYaml
          dep_info <- getDependencyInfo deps
          -- Now lookup to see whether we are combining with an existing HscEnv
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
                  new_deps = RawComponentInfo (thisInstalledUnitId df) df targets cfp opts dep_info
                                : maybe [] snd oldDeps
                  -- Get all the unit-ids for things in this component
                  inplace = map rawComponentUnitId new_deps

              new_deps' <- forM new_deps $ \RawComponentInfo{..} -> do
                  -- Remove all inplace dependencies from package flags for
                  -- components in this HscEnv
                  let (df2, uids) = removeInplacePackages inplace rawComponentDynFlags
                  let prefix = show rawComponentUnitId
                  -- See Note [Avoiding bad interface files]
                  processed_df <- setCacheDir logger prefix (sort $ map show uids) opts df2
                  -- The final component information, mostly the same but the DynFlags don't
                  -- contain any packages which are also loaded
                  -- into the same component.
                  pure $ ComponentInfo rawComponentUnitId
                                       processed_df
                                       uids
                                       rawComponentTargets
                                       rawComponentFP
                                       rawComponentCOptions
                                       rawComponentDependencyInfo
              -- Make a new HscEnv, we have to recompile everything from
              -- scratch again (for now)
              -- It's important to keep the same NameCache though for reasons
              -- that I do not fully understand
              logInfo logger (T.pack ("Making new HscEnv" ++ show inplace))
              hscEnv <- emptyHscEnv ideNc libDir
              newHscEnv <-
                -- Add the options for the current component to the HscEnv
                evalGhcEnv hscEnv $ do
                  _ <- setSessionDynFlags df
                  getSession

              -- Modify the map so the hieYaml now maps to the newly created
              -- HscEnv
              -- Returns
              -- . the new HscEnv so it can be used to modify the
              --   FilePath -> HscEnv map (fileToFlags)
              -- . The information for the new component which caused this cache miss
              -- . The modified information (without -inplace flags) for
              --   existing packages
              pure (Map.insert hieYaml (newHscEnv, new_deps) m, (newHscEnv, head new_deps', tail new_deps'))


    let session :: (Maybe FilePath, NormalizedFilePath, ComponentOptions, FilePath)
                -> IO (IdeResult HscEnvEq,[FilePath])
        session args@(hieYaml, _cfp, _opts, _libDir) = do
          (hscEnv, new, old_deps) <- packageSetup args
          -- Make a map from unit-id to DynFlags, this is used when trying to
          -- resolve imports. (especially PackageImports)
          let uids = map (\ci -> (componentUnitId ci, componentDynFlags ci)) (new : old_deps)

          -- For each component, now make a new HscEnvEq which contains the
          -- HscEnv for the hie.yaml file but the DynFlags for that component

          -- New HscEnv for the component in question, returns the new HscEnvEq and
          -- a mapping from FilePath to the newly created HscEnvEq.
          let new_cache = newComponentCache logger hscEnv uids
          (cs, res) <- new_cache new
          -- Modified cache targets for everything else in the hie.yaml file
          -- which now uses the same EPS and so on
          cached_targets <- concatMapM (fmap fst . new_cache) old_deps
          modifyVar_ fileToFlags $ \var -> do
              pure $ Map.insert hieYaml (HM.fromList (cs ++ cached_targets)) var

          -- Invalidate all the existing GhcSession build nodes by restarting the Shake session
          invalidateShakeCache
          restartShakeSession [kick]

          return (second Map.keys res)

    let consultCradle :: Maybe FilePath -> FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        consultCradle hieYaml cfp = do
           when optTesting $ eventer $ notifyCradleLoaded cfp
           logInfo logger $ T.pack ("Consulting the cradle for " <> show cfp)

           cradle <- maybe (loadImplicitCradle $ addTrailingPathSeparator dir) loadCradle hieYaml
           -- Display a user friendly progress message here: They probably don't know what a
           -- cradle is
           let progMsg = "Setting up project " <> T.pack (takeBaseName (cradleRootDir cradle))



           eopts <- withIndefiniteProgress progMsg LSP.NotCancellable $
             cradleToOptsAndLibDir cradle cfp

           logDebug logger $ T.pack ("Session loading result: " <> show eopts)
           case eopts of
             -- The cradle gave us some options so get to work turning them
             -- into and HscEnv.
             Right (opts, libDir) -> do
               installationCheck <- ghcVersionChecker libDir
               case installationCheck of
                 InstallationNotFound{..} ->
                     error $ "GHC installation not found in libdir: " <> libdir
                 InstallationMismatch{..} ->
                     return (([renderPackageSetupException cfp GhcVersionMismatch{..}], Nothing),[])
                 InstallationChecked _compileTime _ghcLibCheck ->
                   session (hieYaml, toNormalizedFilePath' cfp, opts, libDir)
             -- Failure case, either a cradle error or the none cradle
             Left err -> do
               dep_info <- getDependencyInfo (maybeToList hieYaml)
               let ncfp = toNormalizedFilePath' cfp
               let res = (map (renderCradleError ncfp) err, Nothing)
               modifyVar_ fileToFlags $ \var -> do
                 pure $ Map.insertWith HM.union hieYaml (HM.singleton ncfp (res, dep_info)) var
               return (res,[])

    -- This caches the mapping from hie.yaml + Mod.hs -> [String]
    -- Returns the Ghc session and the cradle dependencies
    let sessionOpts :: (Maybe FilePath, FilePath) -> IO (IdeResult HscEnvEq, [FilePath])
        sessionOpts (hieYaml, file) = do
          v <- fromMaybe HM.empty . Map.lookup hieYaml <$> readVar fileToFlags
          cfp <- canonicalizePath file
          case HM.lookup (toNormalizedFilePath' cfp) v of
            Just (opts, old_di) -> do
              deps_ok <- checkDependencyInfo old_di
              if not deps_ok
                then do
                  -- If the dependencies are out of date then clear both caches and start
                  -- again.
                  modifyVar_ fileToFlags (const (return Map.empty))
                  -- Keep the same name cache
                  modifyVar_ hscEnvs (return . Map.adjust (\(h, _) -> (h, [])) hieYaml )
                  consultCradle hieYaml cfp
                else return (opts, Map.keys old_di)
            Nothing -> consultCradle hieYaml cfp

    -- The main function which gets options for a file. We only want one of these running
    -- at a time. Therefore the IORef contains the currently running cradle, if we try
    -- to get some more options then we wait for the currently running action to finish
    -- before attempting to do so.
    let getOptions :: FilePath -> IO (IdeResult HscEnvEq, [FilePath])
        getOptions file = do
            hieYaml <- cradleLoc file
            sessionOpts (hieYaml, file) `catch` \e ->
                return (([renderPackageSetupException file e], Nothing),[])

    returnWithVersion $ \file -> do
      liftIO $ join $ mask_ $ modifyVar runningCradle $ \as -> do
        -- If the cradle is not finished, then wait for it to finish.
        void $ wait as
        as <- async $ getOptions file
        return (as, wait as)

-- | Create a mapping from FilePaths to HscEnvEqs
newComponentCache
         :: Logger
         -> HscEnv
         -> [(InstalledUnitId, DynFlags)]
         -> ComponentInfo
         -> IO ([(NormalizedFilePath, (IdeResult HscEnvEq, DependencyInfo))], (IdeResult HscEnvEq, DependencyInfo))
newComponentCache logger hsc_env uids ci = do
    let df = componentDynFlags ci
    let hscEnv' = hsc_env { hsc_dflags = df
                          , hsc_IC = (hsc_IC hsc_env) { ic_dflags = df } }

    henv <- newHscEnvEq hscEnv' uids
    let res = (([], Just henv), componentDependencyInfo ci)
    logDebug logger ("New Component Cache HscEnvEq: " <> T.pack (show res))

    let is = importPaths df
    ctargets <- concatMapM (targetToFile is  . targetId) (componentTargets ci)
    -- A special target for the file which caused this wonderful
    -- component to be created. In case the cradle doesn't list all the targets for
    -- the component, in which case things will be horribly broken anyway.
    -- Otherwise, we will immediately attempt to reload this module which
    -- causes an infinite loop and high CPU usage.
    let special_target = (componentFP ci, res)
    let xs = map (,res) ctargets
    return (special_target:xs, res)

{- Note [Avoiding bad interface files]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Originally, we set the cache directory for the various components once
on the first occurrence of the component.
This works fine if these components have no references to each other,
but you have components that depend on each other, the interface files are
updated for each component.
After restarting the session and only opening the component that depended
on the other, suddenly the interface files of this component are stale.
However, from the point of view of `ghcide`, they do not look stale,
thus, not regenerated and the IDE shows weird errors such as:
```
typecheckIface
Declaration for Rep_ClientRunFlags
Axiom branches Rep_ClientRunFlags:
  Failed to load interface for ‘Distribution.Simple.Flag’
  Use -v to see a list of the files searched for.
```
and
```
expectJust checkFamInstConsistency
CallStack (from HasCallStack):
  error, called at compiler\\utils\\Maybes.hs:55:27 in ghc:Maybes
  expectJust, called at compiler\\typecheck\\FamInst.hs:461:30 in ghc:FamInst
```

To mitigate this, we set the cache directory for each component dependent
on the components of the current `HscEnv`, additionally to the component options
of the respective components.
Assume two components, c1, c2, where c2 depends on c1, and the options of the
respective components are co1, co2.
If we want to load component c2, followed by c1, we set the cache directory for
each component in this way:

  * Load component c2
    * (Cache Directory State)
        - name of c2 + co2
  * Load component c1
    * (Cache Directory State)
        - name of c2 + name of c1 + co2
        - name of c2 + name of c1 + co1

Overall, we created three cache directories. If we opened c1 first, then we
create a fourth cache directory.
This makes sure that interface files are always correctly updated.

Since this causes a lot of recompilation, we only update the cache-directory,
if the dependencies of a component have really changed.
E.g. when you load two executables, they can not depend on each other. They
should be filtered out, such that we dont have to re-compile everything.
-}

-- | Set the cache-directory based on the ComponentOptions and a list of
-- internal packages.
-- For the exact reason, see Note [Avoiding bad interface files].
setCacheDir :: MonadIO m => Logger -> String -> [String] -> ComponentOptions -> DynFlags -> m DynFlags
setCacheDir logger prefix hscComponents comps dflags = do
    cacheDir <- liftIO $ getCacheDir prefix (hscComponents ++ componentOptions comps)
    liftIO $ logInfo logger $ "Using interface files cache dir: " <> T.pack cacheDir
    pure $ dflags
          & setHiDir cacheDir
          & setHieDir cacheDir


renderCradleError :: NormalizedFilePath -> CradleError -> FileDiagnostic
renderCradleError nfp (CradleError _ _ec t) =
  ideErrorWithSource (Just "cradle") (Just DsError) nfp (T.unlines (map T.pack t))

-- See Note [Multi Cradle Dependency Info]
type DependencyInfo = Map.Map FilePath (Maybe UTCTime)
type HieMap = Map.Map (Maybe FilePath) (HscEnv, [RawComponentInfo])
type FlagsMap = Map.Map (Maybe FilePath) (HM.HashMap NormalizedFilePath (IdeResult HscEnvEq, DependencyInfo))

-- This is pristine information about a component
data RawComponentInfo = RawComponentInfo
  { rawComponentUnitId :: InstalledUnitId
  -- | Unprocessed DynFlags. Contains inplace packages such as libraries.
  -- We do not want to use them unprocessed.
  , rawComponentDynFlags :: DynFlags
  -- | All targets of this components.
  , rawComponentTargets :: [Target]
  -- | Filepath which caused the creation of this component
  , rawComponentFP :: NormalizedFilePath
  -- | Component Options used to load the component.
  , rawComponentCOptions :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info].
  , rawComponentDependencyInfo :: DependencyInfo
  }

-- This is processed information about the component, in particular the dynflags will be modified.
data ComponentInfo = ComponentInfo
  { componentUnitId :: InstalledUnitId
  -- | Processed DynFlags. Does not contain inplace packages such as local
  -- libraries. Can be used to actually load this Component.
  , componentDynFlags :: DynFlags
  -- | Internal units, such as local libraries, that this component
  -- is loaded with. These have been extracted from the original
  -- ComponentOptions.
  , componentInternalUnits :: [InstalledUnitId]
  -- | All targets of this components.
  , componentTargets :: [Target]
  -- | Filepath which caused the creation of this component
  , componentFP :: NormalizedFilePath
  -- | Component Options used to load the component.
  , componentCOptions :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info]
  , componentDependencyInfo :: DependencyInfo
  }

-- | Check if any dependency has been modified lately.
checkDependencyInfo :: DependencyInfo -> IO Bool
checkDependencyInfo old_di = do
  di <- getDependencyInfo (Map.keys old_di)
  return (di == old_di)

-- Note [Multi Cradle Dependency Info]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why do we implement our own file modification tracking here?
-- The primary reason is that the custom caching logic is quite complicated and going into shake
-- adds even more complexity and more indirection. I did try for about 5 hours to work out how to
-- use shake rules rather than IO but eventually gave up.

-- | Computes a mapping from a filepath to its latest modification date.
-- See Note [Multi Cradle Dependency Info] why we do this ourselves instead
-- of letting shake take care of it.
getDependencyInfo :: [FilePath] -> IO DependencyInfo
getDependencyInfo fs = Map.fromList <$> mapM do_one fs

  where
    tryIO :: IO a -> IO (Either IOException a)
    tryIO = try

    do_one :: FilePath -> IO (FilePath, Maybe UTCTime)
    do_one fp = (fp,) . eitherToMaybe <$> tryIO (getModificationTime fp)

-- | This function removes all the -package flags which refer to packages we
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
    go p@(ExposePackage _ (UnitIdArg u) _) = if toInstalledUnitId u `elem` us
                                                  then Left (toInstalledUnitId u)
                                                  else Right p
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

-- | Throws if package flags are unsatisfiable
setOptions :: GhcMonad m => ComponentOptions -> DynFlags -> m (DynFlags, [Target])
setOptions (ComponentOptions theOpts compRoot _) dflags = do
    (dflags', targets) <- addCmdOpts theOpts dflags
    let dflags'' =
          -- disabled, generated directly by ghcide instead
          flip gopt_unset Opt_WriteInterface $
          -- disabled, generated directly by ghcide instead
          -- also, it can confuse the interface stale check
          dontWriteHieFiles $
          setIgnoreInterfacePragmas $
          setLinkerOptions $
          disableOptimisation $
          makeDynFlagsAbsolute compRoot dflags'
    -- initPackages parses the -package flags and
    -- sets up the visibility for each component.
    -- Throws if a -package flag cannot be satisfied.
    (final_df, _) <- liftIO $ wrapPackageSetupException $ initPackages dflags''
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

getCacheDir :: String -> [String] -> IO FilePath
getCacheDir prefix opts = IO.getXdgDirectory IO.XdgCache (cacheDir </> prefix ++ "-" ++ opts_hash)
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ encode $ H.finalize $ H.updates H.init (map B.pack opts)

-- | Sub directory for the cache path
cacheDir :: String
cacheDir = "ghcide"

notifyCradleLoaded :: FilePath -> FromServerMessage
notifyCradleLoaded fp =
    NotCustomServer $
    NotificationMessage "2.0" (CustomServerMethod cradleLoadedMethod) $
    toJSON fp

cradleLoadedMethod :: T.Text
cradleLoadedMethod = "ghcide/cradle/loaded"

----------------------------------------------------------------------------------------------------

ghcVersionChecker :: GhcVersionChecker
ghcVersionChecker = $$(makeGhcVersionChecker (fromMaybe GHC.Paths.libdir <$> lookupEnv "NIX_GHC_LIBDIR"))

data PackageSetupException
    = PackageSetupException
        { message     :: !String
        }
    | GhcVersionMismatch
        { compileTime :: !Version
        , runTime     :: !Version
        }
    | PackageCheckFailed !NotCompatibleReason
    deriving (Eq, Show, Typeable)

instance Exception PackageSetupException

-- | Wrap any exception as a 'PackageSetupException'
wrapPackageSetupException :: IO a -> IO a
wrapPackageSetupException = handleAny $ \case
  e | Just (pkgE :: PackageSetupException) <- fromException e -> throwIO pkgE
  e -> (throwIO . PackageSetupException . show) e

showPackageSetupException :: PackageSetupException -> String
showPackageSetupException GhcVersionMismatch{..} = unwords
    ["ghcide compiled against GHC"
    ,showVersion compileTime
    ,"but currently using"
    ,showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC version as the project."
    ]
showPackageSetupException PackageSetupException{..} = unwords
    [ "ghcide compiled by GHC", showVersion compilerVersion
    , "failed to load packages:", message <> "."
    , "\nPlease ensure that ghcide is compiled with the same GHC installation as the project."]
showPackageSetupException (PackageCheckFailed PackageVersionMismatch{..}) = unwords
    ["ghcide compiled with package "
    , packageName <> "-" <> showVersion compileTime
    ,"but project uses package"
    , packageName <> "-" <> showVersion runTime
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC installation as the project."
    ]
showPackageSetupException (PackageCheckFailed BasePackageAbiMismatch{..}) = unwords
    ["ghcide compiled with base-" <> showVersion compileTime <> "-" <> compileTimeAbi
    ,"but project uses base-" <> showVersion compileTime <> "-" <> runTimeAbi
    ,"\nThis is unsupported, ghcide must be compiled with the same GHC installation as the project."
    ]

renderPackageSetupException :: FilePath -> PackageSetupException -> (NormalizedFilePath, ShowDiagnostic, Diagnostic)
renderPackageSetupException fp e =
    ideErrorWithSource (Just "cradle") (Just DsError) (toNormalizedFilePath' fp) (T.pack $ showPackageSetupException e)
