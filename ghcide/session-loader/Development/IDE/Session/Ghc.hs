{-# LANGUAGE CPP #-}
module Development.IDE.Session.Ghc where

import           Control.Monad
import           Control.Monad.Extra                 as Extra
import           Control.Monad.IO.Class
import qualified Crypto.Hash.SHA1                    as H
import qualified Data.ByteString.Base16              as B16
import qualified Data.ByteString.Char8               as B
import           Data.Function
import           Data.List
import           Data.List.Extra                     as L
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as NE
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import qualified Data.Text                           as T
import           Development.IDE.Core.Shake          hiding (Log, knownTargets,
                                                      withHieDb)
import qualified Development.IDE.GHC.Compat          as Compat
import           Development.IDE.GHC.Compat.CmdLine
import           Development.IDE.GHC.Compat.Core     hiding (Target, TargetFile,
                                                      TargetModule, Var,
                                                      Warning, getOptions)
import qualified Development.IDE.GHC.Compat.Core     as GHC
import           Development.IDE.GHC.Compat.Env      hiding (Logger)
import           Development.IDE.GHC.Compat.Units    (UnitId)
import           Development.IDE.GHC.Util
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.HscEnvEq      (HscEnvEq, newHscEnvEq)
import           Development.IDE.Types.Location
import           GHC.ResponseFile
import           HIE.Bios.Environment                hiding (getCacheDir)
import           HIE.Bios.Types                      hiding (Log)
import           Ide.Logger                          (Pretty (pretty),
                                                      Priority (Debug, Error, Info),
                                                      Recorder, WithPriority, logWith, viaShow, (<+>))
import           System.Directory
import           System.FilePath
import           System.Info


import           Control.DeepSeq
import           Control.Exception                   (evaluate)
import           Control.Monad.IO.Unlift             (MonadUnliftIO)
import qualified Data.Set                            as OS
import qualified Development.IDE.GHC.Compat.Util     as Compat
import           Development.IDE.Session.Dependency
import           GHC.Data.Graph.Directed
import           Ide.PluginUtils                     (toAbsolute)

import           GHC.Driver.Env                      (hsc_all_home_unit_ids)
import           GHC.Driver.Errors.Types
import           GHC.Types.Error                     (errMsgDiagnostic,
                                                      singleMessage)
import           GHC.Unit.State

data Log
  = LogInterfaceFilesCacheDir !FilePath
  | LogMakingNewHscEnv ![UnitId]
  | LogNewComponentCache !(([FileDiagnostic], Maybe HscEnvEq), DependencyInfo)
  | LogDLLLoadError !String
deriving instance Show Log

instance Pretty Log where
  pretty = \case
    LogInterfaceFilesCacheDir path ->
      "Interface files cache directory:" <+> pretty path
    LogMakingNewHscEnv inPlaceUnitIds ->
      "Making new HscEnv. In-place unit ids:" <+> pretty (map show inPlaceUnitIds)
    LogNewComponentCache componentCache ->
      "New component cache HscEnvEq:" <+> viaShow componentCache
    LogDLLLoadError errorString ->
      "Error dynamically loading libm.so.6:" <+> pretty errorString

-- This is pristine information about a component
data RawComponentInfo = RawComponentInfo
  { rawComponentUnitId         :: UnitId
  -- | Unprocessed DynFlags. Contains inplace packages such as libraries.
  -- We do not want to use them unprocessed.
  , rawComponentDynFlags       :: DynFlags
  -- | All targets of this components.
  , rawComponentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , rawComponentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , rawComponentCOptions       :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info].
  , rawComponentDependencyInfo :: DependencyInfo
  }

-- This is processed information about the component, in particular the dynflags will be modified.
data ComponentInfo = ComponentInfo
  { componentUnitId         :: UnitId
  -- | Processed DynFlags. Does not contain inplace packages such as local
  -- libraries. Can be used to actually load this Component.
  , componentDynFlags       :: DynFlags
  -- | All targets of this components.
  , componentTargets        :: [GHC.Target]
  -- | Filepath which caused the creation of this component
  , componentFP             :: NormalizedFilePath
  -- | Component Options used to load the component.
  , componentCOptions       :: ComponentOptions
  -- | Maps cradle dependencies, such as `stack.yaml`, or `.cabal` file
  -- to last modification time. See Note [Multi Cradle Dependency Info]
  , componentDependencyInfo :: DependencyInfo
  }


unit_flags :: [Flag (CmdLineP [String])]
unit_flags = [defFlag "unit"  (SepArg addUnit)]

addUnit :: String -> EwM (CmdLineP [String]) ()
addUnit unit_str = liftEwM $ do
  units <- getCmdLineState
  putCmdLineState (unit_str : units)


-- | Create a mapping from FilePaths to HscEnvEqs
-- This combines all the components we know about into
-- an appropriate session, which is a multi component
-- session on GHC 9.4+
newComponentCache
         :: Recorder (WithPriority Log)
         -> [String]           -- ^ File extensions to consider
         -> NormalizedFilePath -- ^ Path to file that caused the creation of this component
         -> HscEnv             -- ^ An empty HscEnv
         -> [ComponentInfo]    -- ^ New components to be loaded
         -> [ComponentInfo]    -- ^ old, already existing components
         -> IO [ [TargetDetails] ]
newComponentCache recorder exts _cfp hsc_env old_cis new_cis = do
    let cis = Map.unionWith unionCIs (mkMap new_cis) (mkMap old_cis)
        -- When we have multiple components with the same uid,
        -- prefer the new one over the old.
        -- However, we might have added some targets to the old unit
        -- (see special target), so preserve those
        unionCIs new_ci old_ci = new_ci { componentTargets = componentTargets new_ci ++ componentTargets old_ci }
        mkMap = Map.fromListWith unionCIs . map (\ci -> (componentUnitId ci, ci))
    let dfs = map componentDynFlags $ Map.elems cis
        uids = Map.keys cis
    logWith recorder Info $ LogMakingNewHscEnv uids
    hscEnv' <- -- Set up a multi component session with the other units on GHC 9.4
              Compat.initUnits dfs hsc_env

    let closure_errs = maybeToList $ checkHomeUnitsClosed' (hsc_unit_env hscEnv') (hsc_all_home_unit_ids hscEnv')
        closure_err_to_multi_err err =
            ideErrorWithSource
                (Just "cradle") (Just DiagnosticSeverity_Warning) _cfp
                (T.pack (Compat.printWithoutUniques (singleMessage err)))
                (Just (fmap GhcDriverMessage err))
        multi_errs = map closure_err_to_multi_err closure_errs
        bad_units = OS.fromList $ concat $ do
            x <- map errMsgDiagnostic closure_errs
            DriverHomePackagesNotClosed us <- pure x
            pure us
        isBad ci = (homeUnitId_ (componentDynFlags ci)) `OS.member` bad_units
    -- Whenever we spin up a session on Linux, dynamically load libm.so.6
    -- in. We need this in case the binary is statically linked, in which
    -- case the interactive session will fail when trying to load
    -- ghc-prim, which happens whenever Template Haskell is being
    -- evaluated or haskell-language-server's eval plugin tries to run
    -- some code. If the binary is dynamically linked, then this will have
    -- no effect.
    -- See https://github.com/haskell/haskell-language-server/issues/221
    -- We need to do this after the call to setSessionDynFlags initialises
    -- the loader
    when (os == "linux") $ do
      initObjLinker hscEnv'
      res <- loadDLL hscEnv' "libm.so.6"
      case res of
        Nothing  -> pure ()
        Just err -> logWith recorder Error $ LogDLLLoadError err

    forM (Map.elems cis) $ \ci -> do
      let df = componentDynFlags ci
      thisEnv <- do
            -- In GHC 9.4 we have multi component support, and we have initialised all the units
            -- above.
            -- We just need to set the current unit here
            pure $ hscSetActiveUnitId (homeUnitId_ df) hscEnv'
      henv <- newHscEnvEq thisEnv
      let targetEnv = (if isBad ci then multi_errs else [], Just henv)
          targetDepends = componentDependencyInfo ci
      logWith recorder Debug $ LogNewComponentCache (targetEnv, targetDepends)
      evaluate $ liftRnf rwhnf $ componentTargets ci

      let mk t = fromTargetId (importPaths df) exts (targetId t) targetEnv targetDepends
      ctargets <- concatMapM mk (componentTargets ci)

      return (L.nubOrdOn targetTarget ctargets)

-- | Throws if package flags are unsatisfiable
setOptions :: GhcMonad m
    => NormalizedFilePath
    -> ComponentOptions
    -> DynFlags
    -> FilePath -- ^ root dir, see Note [Root Directory]
    -> m (NonEmpty (DynFlags, [GHC.Target]))
setOptions cfp (ComponentOptions theOpts compRoot _) dflags rootDir = do
    ((theOpts',_errs,_warns),units) <- processCmdLineP unit_flags [] (map noLoc theOpts)
    case NE.nonEmpty units of
      Just us -> initMulti us
      Nothing -> do
        (df, targets) <- initOne (map unLoc theOpts')
        -- A special target for the file which caused this wonderful
        -- component to be created. In case the cradle doesn't list all the targets for
        -- the component, in which case things will be horribly broken anyway.
        --
        -- When we have a singleComponent that is caused to be loaded due to a
        -- file, we assume the file is part of that component. This is useful
        -- for bare GHC sessions, such as many of the ones used in the testsuite
        --
        -- We don't do this when we have multiple components, because each
        -- component better list all targets or there will be anarchy.
        -- It is difficult to know which component to add our file to in
        -- that case.
        -- Multi unit arguments are likely to come from cabal, which
        -- does list all targets.
        --
        -- If we don't end up with a target for the current file in the end, then
        -- we will report it as an error for that file
        let abs_fp = toAbsolute rootDir (fromNormalizedFilePath cfp)
        let special_target = Compat.mkSimpleTarget df abs_fp
        pure $ (df, special_target : targets) :| []
    where
      initMulti unitArgFiles =
        forM unitArgFiles $ \f -> do
          args <- liftIO $ expandResponse [f]
          initOne args
      initOne this_opts = do
        (dflags', targets') <- addCmdOpts this_opts dflags
        let dflags'' =
                case unitIdString (homeUnitId_ dflags') of
                     -- cabal uses main for the unit id of all executable packages
                     -- This makes multi-component sessions confused about what
                     -- options to use for that component.
                     -- Solution: hash the options and use that as part of the unit id
                     -- This works because there won't be any dependencies on the
                     -- executable unit.
                     "main" ->
                       let hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack this_opts)
                           hashed_uid = Compat.toUnitId (Compat.stringToUnit ("main-"++hash))
                       in setHomeUnitId_ hashed_uid dflags'
                     _ -> dflags'

        let targets = makeTargetsAbsolute root targets'
            root = case workingDirectory dflags'' of
              Nothing   -> compRoot
              Just wdir -> compRoot </> wdir
        let dflags''' =
              setWorkingDirectory root $
              disableWarningsAsErrors $
              -- disabled, generated directly by ghcide instead
              flip gopt_unset Opt_WriteInterface $
              -- disabled, generated directly by ghcide instead
              -- also, it can confuse the interface stale check
              dontWriteHieFiles $
              setIgnoreInterfacePragmas $
              setBytecodeLinkerOptions $
              disableOptimisation $
              Compat.setUpTypedHoles $
              makeDynFlagsAbsolute compRoot -- makeDynFlagsAbsolute already accounts for workingDirectory
              dflags''
        return (dflags''', targets)

addComponentInfo ::
  MonadUnliftIO m =>
  Recorder (WithPriority Log) ->
  (String -> [String] -> IO CacheDirs) ->
  DependencyInfo ->
  NonEmpty (DynFlags, [GHC.Target]) ->
  (Maybe FilePath, NormalizedFilePath, ComponentOptions) ->
  Map.Map (Maybe FilePath) [RawComponentInfo] ->
  m (Map.Map (Maybe FilePath) [RawComponentInfo], ([ComponentInfo], [ComponentInfo]))
addComponentInfo recorder getCacheDirs dep_info newDynFlags (hieYaml, cfp, opts) m = do
  -- Just deps if there's already an HscEnv
  -- Nothing is it's the first time we are making an HscEnv
  let oldDeps = Map.lookup hieYaml m
  let -- Add the raw information about this component to the list
      -- We will modify the unitId and DynFlags used for
      -- compilation but these are the true source of
      -- information.
      new_deps = fmap (\(df, targets) -> RawComponentInfo (homeUnitId_ df) df targets cfp opts dep_info) newDynFlags
      all_deps = new_deps `NE.appendList` fromMaybe [] oldDeps
      -- Get all the unit-ids for things in this component

  all_deps' <- forM all_deps $ \RawComponentInfo{..} -> do
    let prefix = show rawComponentUnitId
    -- See Note [Avoiding bad interface files]
    let cacheDirOpts = componentOptions opts
    cacheDirs <- liftIO $ getCacheDirs prefix cacheDirOpts
    processed_df <- setCacheDirs recorder cacheDirs rawComponentDynFlags
    -- The final component information, mostly the same but the DynFlags don't
    -- contain any packages which are also loaded
    -- into the same component.
    pure $ ComponentInfo
      { componentUnitId = rawComponentUnitId
      , componentDynFlags = processed_df
      , componentTargets = rawComponentTargets
      , componentFP = rawComponentFP
      , componentCOptions = rawComponentCOptions
      , componentDependencyInfo = rawComponentDependencyInfo
      }
  -- Modify the map so the hieYaml now maps to the newly updated
  -- ComponentInfos
  -- Returns
  -- . The information for the new component which caused this cache miss
  -- . The modified information (without -inplace flags) for
  --   existing packages
  let (new,old) = NE.splitAt (NE.length new_deps) all_deps'
  pure (Map.insert hieYaml (NE.toList all_deps) m, (new,old))

setIgnoreInterfacePragmas :: DynFlags -> DynFlags
setIgnoreInterfacePragmas df =
    gopt_set (gopt_set df Opt_IgnoreInterfacePragmas) Opt_IgnoreOptimChanges

disableOptimisation :: DynFlags -> DynFlags
disableOptimisation df = updOptLevel 0 df

setHiDir :: FilePath -> DynFlags -> DynFlags
setHiDir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { hiDir      = Just f}

setODir :: FilePath -> DynFlags -> DynFlags
setODir f d =
    -- override user settings to avoid conflicts leading to recompilation
    d { objectDir = Just f}

data CacheDirs = CacheDirs
  { hiCacheDir, hieCacheDir, oCacheDir :: Maybe FilePath}

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

and many more.

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
setCacheDirs :: MonadUnliftIO m => Recorder (WithPriority Log) -> CacheDirs -> DynFlags -> m DynFlags
setCacheDirs recorder CacheDirs{..} dflags = do
    logWith recorder Info $ LogInterfaceFilesCacheDir (fromMaybe cacheDir hiCacheDir)
    pure $ dflags
          & maybe id setHiDir hiCacheDir
          & maybe id setHieDir hieCacheDir
          & maybe id setODir oCacheDir

getCacheDirsDefault :: String -> [String] -> IO CacheDirs
getCacheDirsDefault prefix opts = do
    dir <- Just <$> getXdgDirectory XdgCache (cacheDir </> prefix ++ "-" ++ opts_hash)
    return $ CacheDirs dir dir dir
    where
        -- Create a unique folder per set of different GHC options, assuming that each different set of
        -- GHC options will create incompatible interface files.
        opts_hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init (map B.pack opts)

setNameCache :: NameCache -> HscEnv -> HscEnv
setNameCache nc hsc = hsc { hsc_NC = nc }

-- | Sub directory for the cache path
cacheDir :: String
cacheDir = "ghcide"

emptyHscEnv :: NameCache -> FilePath -> IO HscEnv
emptyHscEnv nc libDir = do
    -- We call setSessionDynFlags so that the loader is initialised
    -- We need to do this before we call initUnits.
    env <- liftIO $ runGhc (Just libDir) $
      getSessionDynFlags >>= setSessionDynFlags >> getSession
    pure $ setNameCache nc (hscSetFlags ((hsc_dflags env){useUnicode = True }) env)

-- ----------------------------------------------------------------------------
-- Target Details
-- ----------------------------------------------------------------------------

data TargetDetails = TargetDetails
  {
      targetTarget    :: !Target,
      targetEnv       :: !(IdeResult HscEnvEq),
      targetDepends   :: !DependencyInfo,
      targetLocations :: ![NormalizedFilePath]
  }

fromTargetId :: [FilePath]          -- ^ import paths
             -> [String]            -- ^ extensions to consider
             -> TargetId
             -> IdeResult HscEnvEq
             -> DependencyInfo
             -> IO [TargetDetails]
-- For a target module we consider all the import paths
fromTargetId is exts (GHC.TargetModule modName) env dep = do
    let fps = [i </> moduleNameSlashes modName -<.> ext <> boot
              | ext <- exts
              , i <- is
              , boot <- ["", "-boot"]
              ]
    let locs = fmap toNormalizedFilePath' fps
    return [TargetDetails (TargetModule modName) env dep locs]
-- For a 'TargetFile' we consider all the possible module names
fromTargetId _ _ (GHC.TargetFile f _) env deps = do
    let nf = toNormalizedFilePath' f
    let other
          | "-boot" `isSuffixOf` f = toNormalizedFilePath' (L.dropEnd 5 $ fromNormalizedFilePath nf)
          | otherwise = toNormalizedFilePath' (fromNormalizedFilePath nf ++ "-boot")
    return [TargetDetails (TargetFile nf) env deps [nf, other]]

-- ----------------------------------------------------------------------------
-- Backwards compatibility
-- ----------------------------------------------------------------------------

#if MIN_VERSION_ghc(9,13,0)
-- Moved back to implementation in GHC.
checkHomeUnitsClosed' ::  UnitEnv -> OS.Set UnitId -> [DriverMessages]
checkHomeUnitsClosed' ue _ = checkHomeUnitsClosed ue
#else
-- This function checks the important property that if both p and q are home units
-- then any dependency of p, which transitively depends on q is also a home unit.
-- GHC had an implementation of this function, but it was horribly inefficient
-- We should move back to the GHC implementation on compilers where
-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12162 is included
checkHomeUnitsClosed' ::  UnitEnv -> OS.Set UnitId -> Maybe (Compat.MsgEnvelope DriverMessage)
checkHomeUnitsClosed' ue home_id_set
    | OS.null bad_unit_ids = Nothing
    | otherwise = Just (GHC.mkPlainErrorMsgEnvelope rootLoc $ DriverHomePackagesNotClosed (OS.toList bad_unit_ids))
  where
    bad_unit_ids = upwards_closure OS.\\ home_id_set
    rootLoc = mkGeneralSrcSpan (Compat.fsLit "<command line>")

    graph :: Graph (Node UnitId UnitId)
    graph = graphFromEdgedVerticesUniq graphNodes

    -- downwards closure of graph
    downwards_closure
      = graphFromEdgedVerticesUniq [ DigraphNode uid uid (OS.toList deps)
                                   | (uid, deps) <- Map.toList (allReachable graph node_key)]

    inverse_closure = transposeG downwards_closure

    upwards_closure = OS.fromList $ map node_key $ reachablesG inverse_closure [DigraphNode uid uid [] | uid <- OS.toList home_id_set]

    all_unit_direct_deps :: UniqMap UnitId (OS.Set UnitId)
    all_unit_direct_deps
      = unitEnv_foldWithKey go emptyUniqMap $ ue_home_unit_graph ue
      where
        go rest this this_uis =
           plusUniqMap_C OS.union
             (addToUniqMap_C OS.union external_depends this (OS.fromList this_deps))
             rest
           where
             external_depends = mapUniqMap (OS.fromList . unitDepends)
#if !MIN_VERSION_ghc(9,7,0)
                              $ listToUniqMap $ Map.toList
#endif

                              $ unitInfoMap this_units
             this_units = homeUnitEnv_units this_uis
             this_deps = [ Compat.toUnitId unit | (unit,Just _) <- explicitUnits this_units]

    graphNodes :: [Node UnitId UnitId]
    graphNodes = go OS.empty home_id_set
      where
        go done todo
          = case OS.minView todo of
              Nothing -> []
              Just (uid, todo')
                | OS.member uid done -> go done todo'
                | otherwise -> case lookupUniqMap all_unit_direct_deps uid of
                    Nothing -> pprPanic "uid not found" (Compat.ppr (uid, all_unit_direct_deps))
                    Just depends ->
                      let todo'' = (depends OS.\\ done) `OS.union` todo'
                      in DigraphNode uid uid (OS.toList depends) : go (OS.insert uid done) todo''
#endif
