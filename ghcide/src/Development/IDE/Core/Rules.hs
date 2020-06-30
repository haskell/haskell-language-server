-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DuplicateRecordFields #-}
#include "ghc-api-version.h"

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    IdeState, GetDependencies(..), GetParsedModule(..), TransitiveDependencies(..),
    Priority(..), GhcSessionIO(..),
    priorityTypeCheck,
    priorityGenerateCore,
    priorityFilesOfInterest,
    runAction, useE, useNoFileE, usesE,
    toIdeResult,
    defineNoFile,
    defineEarlyCutOffNoFile,
    mainRule,
    getAtPoint,
    getDefinition,
    getTypeDefinition,
    getDependencies,
    getParsedModule,
    generateCore,
    ) where

import Fingerprint

import Data.Binary hiding (get, put)
import Data.Tuple.Extra
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Development.IDE.Core.Compile
import Development.IDE.Core.OfInterest
import Development.IDE.Types.Options
import Development.IDE.Spans.Calculate
import Development.IDE.Import.DependencyInformation
import Development.IDE.Import.FindImports
import           Development.IDE.Core.FileExists
import           Development.IDE.Core.FileStore        (getFileContents)
import           Development.IDE.Types.Diagnostics as Diag
import Development.IDE.Types.Location
import Development.IDE.GHC.Compat hiding (parseModule, typecheckModule)
import Development.IDE.GHC.Util
import Development.IDE.GHC.WithDynFlags
import Data.Either.Extra
import qualified Development.IDE.Types.Logger as L
import Data.Maybe
import           Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.List
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Development.IDE.GHC.Error
import           Development.Shake                        hiding (Diagnostic)
import Development.IDE.Core.RuleTypes
import Development.IDE.Spans.Type
import qualified Data.ByteString.Char8 as BS
import Development.IDE.Core.PositionMapping

import qualified GHC.LanguageExtensions as LangExt
import HscTypes
import PackageConfig
import DynFlags (gopt_set, xopt)
import GHC.Generics(Generic)

import qualified Development.IDE.Spans.AtPoint as AtPoint
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.Shake.Classes hiding (get, put)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString (ByteString)
import Control.Concurrent.Async (concurrently)
import System.Time.Extra
import Control.Monad.Reader
import System.Directory ( getModificationTime )
import Control.Exception

import Control.Monad.State
import FastString (FastString(uniq))
import qualified HeaderInfo as Hdr

-- | This is useful for rules to convert rules that can only produce errors or
-- a result into the more general IdeResult type that supports producing
-- warnings while also producing a result.
toIdeResult :: Either [FileDiagnostic] v -> IdeResult v
toIdeResult = either (, Nothing) (([],) . Just)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> MaybeT IdeAction (v, PositionMapping)
useE k = MaybeT . useWithStaleFast k

useNoFileE :: IdeRule k v => IdeState -> k -> MaybeT IdeAction v
useNoFileE _ide k = fst <$> useE k emptyFilePath

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT IdeAction [(v,PositionMapping)]
usesE k = MaybeT . fmap sequence . mapM (useWithStaleFast k)

defineNoFile :: IdeRule k v => (k -> Action v) -> Rules ()
defineNoFile f = define $ \k file -> do
    if file == emptyFilePath then do res <- f k; return ([], Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutOffNoFile :: IdeRule k v => (k -> Action (ByteString, v)) -> Rules ()
defineEarlyCutOffNoFile f = defineEarlyCutoff $ \k file -> do
    if file == emptyFilePath then do (hash, res) <- f k; return (Just hash, ([], Just res)) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"


------------------------------------------------------------
-- Exposed API

-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: NormalizedFilePath -> Action (Maybe [NormalizedFilePath])
getDependencies file = fmap transitiveModuleDeps <$> use GetDependencies file

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> IdeAction (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = fmap join $ runMaybeT $ do
  ide <- ask
  opts <- liftIO $ getIdeOptionsIO ide
  (spans, mapping) <- useE  GetSpanInfo file
  !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
  return $ AtPoint.atPoint opts spans pos'

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe Location)
getDefinition file pos = runMaybeT $ do
    ide <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (spans,mapping) <- useE GetSpanInfo file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    AtPoint.gotoDefinition (getHieFile ide file) opts (spansExprs spans) pos'

getTypeDefinition :: NormalizedFilePath -> Position -> IdeAction (Maybe [Location])
getTypeDefinition file pos = runMaybeT $ do
    ide <- ask
    opts <- liftIO $ getIdeOptionsIO ide
    (spans,mapping) <- useE GetSpanInfo file
    !pos' <- MaybeT (return $ fromCurrentPosition mapping pos)
    AtPoint.gotoTypeDefinition (getHieFile ide file) opts (spansExprs spans) pos'

getHieFile
  :: ShakeExtras
  -> NormalizedFilePath -- ^ file we're editing
  -> Module -- ^ module dep we want info for
  -> MaybeT IdeAction (HieFile, FilePath) -- ^ hie stuff for the module
getHieFile ide file mod = do
  TransitiveDependencies {transitiveNamedModuleDeps} <- fst <$> useE GetDependencies file
  case find (\x -> nmdModuleName x == moduleName mod) transitiveNamedModuleDeps of
    Just NamedModuleDep{nmdFilePath=nfp} -> do
        let modPath = fromNormalizedFilePath nfp
        hieFile <- getHomeHieFile nfp
        return (hieFile, modPath)
    _ -> getPackageHieFile ide mod file

getHomeHieFile :: NormalizedFilePath -> MaybeT IdeAction HieFile
getHomeHieFile f = do
  ms <- fst <$> useE GetModSummary f
  let normal_hie_f = toNormalizedFilePath' hie_f
      hie_f = ml_hie_file $ ms_location ms

  mbHieTimestamp <- either (\(_ :: IOException) -> Nothing) Just <$> (liftIO $ try $ getModificationTime hie_f)
  srcTimestamp   <- MaybeT (either (\(_ :: IOException) -> Nothing) Just <$> (liftIO $ try $ getModificationTime $ fromNormalizedFilePath f))
  liftIO $ print (mbHieTimestamp, srcTimestamp, hie_f, normal_hie_f)
  let isUpToDate
        | Just d <- mbHieTimestamp = d > srcTimestamp
        | otherwise = False

  if isUpToDate
    then do
      hf <- liftIO $ whenMaybe isUpToDate (loadHieFile hie_f)
      MaybeT $ return hf
    else do
      wait <- lift $ delayedAction $ mkDelayedAction "OutOfDateHie" L.Info $ do
        hsc <- hscEnv <$> use_ GhcSession f
        pm <- use_ GetParsedModule f
        typeCheckRuleDefinition hsc pm DoGenerateInterfaceFiles
      _ <- MaybeT $ liftIO $ timeout 1 wait
      liftIO $ loadHieFile hie_f


getPackageHieFile :: ShakeExtras
                  -> Module             -- ^ Package Module to load .hie file for
                  -> NormalizedFilePath -- ^ Path of home module importing the package module
                  -> MaybeT IdeAction (HieFile, FilePath)
getPackageHieFile ide mod file = do
    pkgState  <- hscEnv . fst <$> useE GhcSession file
    IdeOptions {..} <- liftIO $ getIdeOptionsIO ide
    let unitId = moduleUnitId mod
    case lookupPackageConfig unitId pkgState of
        Just pkgConfig -> do
            -- 'optLocateHieFile' returns Nothing if the file does not exist
            hieFile <- liftIO $ optLocateHieFile optPkgLocationOpts pkgConfig mod
            path    <- liftIO $ optLocateSrcFile optPkgLocationOpts pkgConfig mod
            case (hieFile, path) of
                (Just hiePath, Just modPath) -> MaybeT $
                    -- deliberately loaded outside the Shake graph
                    -- to avoid dependencies on non-workspace files
                        liftIO $ Just . (, modPath) <$> loadHieFile hiePath
                _ -> MaybeT $ return Nothing
        _ -> MaybeT $ return Nothing

-- | Parse the contents of a daml file.
getParsedModule :: NormalizedFilePath -> Action (Maybe ParsedModule)
getParsedModule file = use GetParsedModule file

------------------------------------------------------------
-- Rules
-- These typically go from key to value and are oracles.

priorityTypeCheck :: Priority
priorityTypeCheck = Priority 0

priorityGenerateCore :: Priority
priorityGenerateCore = Priority (-1)

priorityFilesOfInterest :: Priority
priorityFilesOfInterest = Priority (-2)

getParsedModuleRule :: Rules ()
getParsedModuleRule = defineEarlyCutoff $ \GetParsedModule file -> do
    sess <- use_ GhcSession file
    let hsc = hscEnv sess
        -- These packages are used when removing PackageImports from a
        -- parsed module
        comp_pkgs = mapMaybe (fmap fst . mkImportDirs (hsc_dflags hsc)) (deps sess)
    opt <- getIdeOptions
    (_, contents) <- getFileContents file

    let dflags    = hsc_dflags hsc
        mainParse = getParsedModuleDefinition hsc opt comp_pkgs file contents

    -- Parse again (if necessary) to capture Haddock parse errors
    if gopt Opt_Haddock dflags
        then
            liftIO mainParse
        else do
            let haddockParse = do
                    (_, (!diagsHaddock, _)) <-
                        getParsedModuleDefinition (withOptHaddock hsc) opt comp_pkgs file contents
                    return diagsHaddock

            ((fingerPrint, (diags, res)), diagsHaddock) <-
                -- parse twice, with and without Haddocks, concurrently
                -- we want warnings if parsing with Haddock fails
                -- but if we parse with Haddock we lose annotations
                liftIO $ concurrently mainParse haddockParse

            return (fingerPrint, (mergeParseErrorsHaddock diags diagsHaddock, res))


withOptHaddock :: HscEnv -> HscEnv
withOptHaddock hsc = hsc{hsc_dflags = gopt_set (hsc_dflags hsc) Opt_Haddock}


-- | Given some normal parse errors (first) and some from Haddock (second), merge them.
--   Ignore Haddock errors that are in both. Demote Haddock-only errors to warnings.
mergeParseErrorsHaddock :: [FileDiagnostic] -> [FileDiagnostic] -> [FileDiagnostic]
mergeParseErrorsHaddock normal haddock = normal ++
    [ (a,b,c{_severity = Just DsWarning, _message = fixMessage $ _message c})
    | (a,b,c) <- haddock, Diag._range c `Set.notMember` locations]
  where
    locations = Set.fromList $ map (Diag._range . thd3) normal

    fixMessage x | "parse error " `T.isPrefixOf` x = "Haddock " <> x
                 | otherwise = "Haddock: " <> x


getParsedModuleDefinition :: HscEnv -> IdeOptions -> [PackageName] -> NormalizedFilePath -> Maybe T.Text -> IO (Maybe ByteString, ([FileDiagnostic], Maybe ParsedModule))
getParsedModuleDefinition packageState opt comp_pkgs file contents = do
    (diag, res) <- parseModule opt packageState comp_pkgs (fromNormalizedFilePath file) (fmap textToStringBuffer contents)
    case res of
        Nothing -> pure (Nothing, (diag, Nothing))
        Just (contents, modu) -> do
            mbFingerprint <- if isNothing $ optShakeFiles opt
                then pure Nothing
                else Just . fingerprintToBS <$> fingerprintFromStringBuffer contents
            pure (mbFingerprint, (diag, Just modu))

getLocatedImportsRule :: Rules ()
getLocatedImportsRule =
    define $ \GetLocatedImports file -> do
        ms <- use_ GetModSummary file
        let imports = [(False, imp) | imp <- ms_textual_imps ms] ++ [(True, imp) | imp <- ms_srcimps ms]
        env_eq <- use_ GhcSession file
        let env = hscEnv env_eq
        let import_dirs = deps env_eq
        let dflags = addRelativeImport file (moduleName $ ms_mod ms) $ hsc_dflags env
        opt <- getIdeOptions
        (diags, imports') <- fmap unzip $ forM imports $ \(isSource, (mbPkgName, modName)) -> do
            diagOrImp <- locateModule dflags import_dirs (optExtensions opt) getFileExists modName mbPkgName isSource
            case diagOrImp of
                Left diags -> pure (diags, Left (modName, Nothing))
                Right (FileImport path) -> pure ([], Left (modName, Just path))
                Right (PackageImport pkgId) -> liftIO $ do
                    diagsOrPkgDeps <- computePackageDeps env pkgId
                    case diagsOrPkgDeps of
                        Left diags -> pure (diags, Right Nothing)
                        Right pkgIds -> pure ([], Right $ Just $ pkgId : pkgIds)
        let (moduleImports, pkgImports) = partitionEithers imports'
        case sequence pkgImports of
            Nothing -> pure (concat diags, Nothing)
            Just pkgImports -> pure (concat diags, Just (moduleImports, Set.fromList $ concat pkgImports))

type RawDepM a = StateT (RawDependencyInformation, IntMap ArtifactsLocation) Action a

execRawDepM :: Monad m => StateT (RawDependencyInformation, IntMap a1) m a2 -> m (RawDependencyInformation, IntMap a1)
execRawDepM act =
    execStateT act
        ( RawDependencyInformation IntMap.empty emptyPathIdMap IntMap.empty
        , IntMap.empty
        )

-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: [NormalizedFilePath] -> Action RawDependencyInformation
rawDependencyInformation fs = do
    (rdi, ss) <- execRawDepM (mapM_ go fs)
    let bm = IntMap.foldrWithKey (updateBootMap rdi) IntMap.empty ss
    return (rdi { rawBootMap = bm })
  where
    go :: NormalizedFilePath -- ^ Current module being processed
       -> StateT (RawDependencyInformation, IntMap ArtifactsLocation) Action FilePathId
    go f = do
      -- First check to see if we have already processed the FilePath
      -- If we have, just return its Id but don't update any of the state.
      -- Otherwise, we need to process its imports.
      checkAlreadyProcessed f $ do
          al <- lift $ modSummaryToArtifactsLocation f <$> use_ GetModSummary f
          -- Get a fresh FilePathId for the new file
          fId <- getFreshFid al
          -- Adding an edge to the bootmap so we can make sure to
          -- insert boot nodes before the real files.
          addBootMap al fId
          -- Try to parse the imports of the file
          importsOrErr <- lift $ use GetLocatedImports f
          case importsOrErr of
            Nothing -> do
            -- File doesn't parse so add the module as a failure into the
            -- dependency information, continue processing the other
            -- elements in the queue
              modifyRawDepInfo (insertImport fId (Left ModuleParseError))
              return fId
            Just (modImports, pkgImports) -> do
              -- Get NFPs of the imports which have corresponding files
              -- Imports either come locally from a file or from a package.
              let (no_file, with_file) = splitImports modImports
                  (mns, ls) = unzip with_file
              -- Recursively process all the imports we just learnt about
              -- and get back a list of their FilePathIds
              fids <- mapM (go . artifactFilePath) ls
              -- Associate together the ModuleName with the FilePathId
              let moduleImports' = map (,Nothing) no_file ++ zip mns (map Just fids)
              -- Insert into the map the information about this modules
              -- imports.
              modifyRawDepInfo $ insertImport fId (Right $ ModuleImports moduleImports' pkgImports)
              return fId


    checkAlreadyProcessed :: NormalizedFilePath -> RawDepM FilePathId -> RawDepM FilePathId
    checkAlreadyProcessed nfp k = do
      (rawDepInfo, _) <- get
      maybe k return (lookupPathToId (rawPathIdMap rawDepInfo) nfp)

    modifyRawDepInfo :: (RawDependencyInformation -> RawDependencyInformation) -> RawDepM ()
    modifyRawDepInfo f = modify (first f)

    addBootMap ::  ArtifactsLocation -> FilePathId -> RawDepM ()
    addBootMap al fId =
      modify (\(rd, ss) -> (rd, if isBootLocation al
                                  then IntMap.insert (getFilePathId fId) al ss
                                  else ss))

    getFreshFid :: ArtifactsLocation -> RawDepM FilePathId
    getFreshFid al = do
      (rawDepInfo, ss) <- get
      let (fId, path_map) = getPathId al (rawPathIdMap rawDepInfo)
      -- Insert the File into the bootmap if it's a boot module
      let rawDepInfo' = rawDepInfo { rawPathIdMap = path_map }
      put (rawDepInfo', ss)
      return fId

    -- Split in (package imports, local imports)
    splitImports :: [(Located ModuleName, Maybe ArtifactsLocation)]
                 -> ([Located ModuleName], [(Located ModuleName, ArtifactsLocation)])
    splitImports = foldr splitImportsLoop ([],[])

    splitImportsLoop (imp, Nothing) (ns, ls) = (imp:ns, ls)
    splitImportsLoop (imp, Just artifact) (ns, ls) = (ns, (imp,artifact) : ls)

    updateBootMap pm boot_mod_id ArtifactsLocation{..} bm =
      if not artifactIsSource
        then
          let msource_mod_id = lookupPathToId (rawPathIdMap pm) (toNormalizedFilePath' $ dropBootSuffix artifactModLocation)
          in case msource_mod_id of
               Just source_mod_id -> insertBootId source_mod_id (FilePathId boot_mod_id) bm
               Nothing -> bm
        else bm

    dropBootSuffix :: ModLocation -> FilePath
    dropBootSuffix (ModLocation (Just hs_src) _ _) = reverse . drop (length @[] "-boot") . reverse $ hs_src
    dropBootSuffix _ = error "dropBootSuffix"

getDependencyInformationRule :: Rules ()
getDependencyInformationRule =
    define $ \GetDependencyInformation file -> do
       rawDepInfo <- rawDependencyInformation [file]
       pure ([], Just $ processDependencyInformation rawDepInfo)

reportImportCyclesRule :: Rules ()
reportImportCyclesRule =
    define $ \ReportImportCycles file -> fmap (\errs -> if null errs then ([], Just ()) else (errs, Nothing)) $ do
        DependencyInformation{..} <- use_ GetDependencyInformation file
        let fileId = pathToId depPathIdMap file
        case IntMap.lookup (getFilePathId fileId) depErrorNodes of
            Nothing -> pure []
            Just errs -> do
                let cycles = mapMaybe (cycleErrorInFile fileId) (toList errs)
                -- Convert cycles of files into cycles of module names
                forM cycles $ \(imp, files) -> do
                    modNames <- forM files $ \fileId -> do
                        let file = idToPath depPathIdMap fileId
                        getModuleName file
                    pure $ toDiag imp $ sort modNames
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods = (fp , ShowDiag , ) $ Diagnostic
            { _range = (_range :: Location -> Range) loc
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            , _tags = Nothing
            }
            where loc = srcSpanToLocation (getLoc imp)
                  fp = toNormalizedFilePath' $ srcSpanToFilename (getLoc imp)
          getModuleName file = do
           ms <- use_ GetModSummary file
           pure (moduleNameString . moduleName . ms_mod $ ms)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: Rules ()
getDependenciesRule =
    defineEarlyCutoff $ \GetDependencies file -> do
        depInfo <- use_ GetDependencyInformation file
        let allFiles = reachableModules depInfo
        _ <- uses_ ReportImportCycles allFiles
        opts <- getIdeOptions
        let mbFingerprints = map (fingerprintString . fromNormalizedFilePath) allFiles <$ optShakeFiles opts
        return (fingerprintToBS . fingerprintFingerprints <$> mbFingerprints, ([], transitiveDeps depInfo file))

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: Rules ()
getSpanInfoRule =
    define $ \GetSpanInfo file -> do
        tc <- use_ TypeCheck file
        packageState <- hscEnv <$> use_ GhcSession file

-- When possible, rely on the haddocks embedded in our interface files
-- This creates problems on ghc-lib, see comment on 'getDocumentationTryGhc'
#if MIN_GHC_API_VERSION(8,6,0) && !defined(GHC_LIB)
        let parsedDeps = []
#else
        deps <- maybe (TransitiveDependencies [] [] []) fst <$> useWithStale GetDependencies file
        let tdeps = transitiveModuleDeps deps
        parsedDeps <- mapMaybe (fmap fst) <$> usesWithStale GetParsedModule tdeps
#endif

        (fileImports, _) <- use_ GetLocatedImports file
        let imports = second (fmap artifactFilePath) <$> fileImports
        x <- liftIO $ getSrcSpanInfos packageState imports tc parsedDeps
        return ([], Just x)

-- Typechecks a module.
typeCheckRule :: Rules ()
typeCheckRule = define $ \TypeCheck file -> do
    pm <- use_ GetParsedModule file
    hsc  <- hscEnv <$> use_ GhcSessionDeps file
    -- do not generate interface files as this rule is called
    -- for files of interest on every keystroke
    typeCheckRuleDefinition hsc pm SkipGenerationOfInterfaceFiles

data GenerateInterfaceFiles
    = DoGenerateInterfaceFiles
    | SkipGenerationOfInterfaceFiles
    deriving (Show)

-- This is factored out so it can be directly called from the GetModIface
-- rule. Directly calling this rule means that on the initial load we can
-- garbage collect all the intermediate typechecked modules rather than
-- retain the information forever in the shake graph.
typeCheckRuleDefinition
    :: HscEnv
    -> ParsedModule
    -> GenerateInterfaceFiles -- ^ Should generate .hi and .hie files ?
    -> Action (IdeResult TcModuleResult)
typeCheckRuleDefinition hsc pm generateArtifacts = do
  setPriority priorityTypeCheck
  IdeOptions { optDefer = defer } <- getIdeOptions

  addUsageDependencies $ liftIO $ do
    res <- typecheckModule defer hsc pm
    case res of
      (diags, Just (hsc,tcm)) | DoGenerateInterfaceFiles <- generateArtifacts -> do
        diagsHie <- generateAndWriteHieFile hsc (tmrModule tcm)
        diagsHi  <- generateAndWriteHiFile hsc tcm
        return (diags <> diagsHi <> diagsHie, Just tcm)
      (diags, res) ->
        return (diags, snd <$> res)
 where
  addUsageDependencies :: Action (a, Maybe TcModuleResult) -> Action (a, Maybe TcModuleResult)
  addUsageDependencies a = do
    r@(_, mtc) <- a
    forM_ mtc $ \tc -> do
      let used_files = mapMaybe udep (mi_usages (hm_iface (tmrModInfo tc)))
          udep (UsageFile fp _h) = Just fp
          udep _ = Nothing
      -- Add a dependency on these files which are added by things like
      -- qAddDependentFile
      void $ uses_ GetModificationTime (map toNormalizedFilePath' used_files)
    return r


generateCore :: RunSimplifier -> NormalizedFilePath -> Action (IdeResult (SafeHaskellMode, CgGuts, ModDetails))
generateCore runSimplifier file = do
    deps <- use_ GetDependencies file
    (tm:tms) <- uses_ TypeCheck (file:transitiveModuleDeps deps)
    setPriority priorityGenerateCore
    packageState <- hscEnv <$> use_ GhcSession file
    liftIO $ compileModule runSimplifier packageState [(tmrModSummary x, tmrModInfo x) | x <- tms] tm

generateCoreRule :: Rules ()
generateCoreRule =
    define $ \GenerateCore -> generateCore (RunSimplifier True)

generateByteCodeRule :: Rules ()
generateByteCodeRule =
    define $ \GenerateByteCode file -> do
      deps <- use_ GetDependencies file
      (tm : tms) <- uses_ TypeCheck (file: transitiveModuleDeps deps)
      session <- hscEnv <$> use_ GhcSession file
      (_, guts, _) <- use_ GenerateCore file
      liftIO $ generateByteCode session [(tmrModSummary x, tmrModInfo x) | x <- tms] tm guts

-- A local rule type to get caching. We want to use newCache, but it has
-- thread killed exception issues, so we lift it to a full rule.
-- https://github.com/digital-asset/daml/pull/2808#issuecomment-529639547
type instance RuleResult GhcSessionIO = IdeGhcSession

data GhcSessionIO = GhcSessionIO deriving (Eq, Show, Typeable, Generic)
instance Hashable GhcSessionIO
instance NFData   GhcSessionIO
instance Binary   GhcSessionIO

loadGhcSession :: Rules ()
loadGhcSession = do
    -- This function should always be rerun because it tracks changes
    -- to the version of the collection of HscEnv's.
    defineEarlyCutOffNoFile $ \GhcSessionIO -> do
        alwaysRerun
        opts <- getIdeOptions
        res <- optGhcSession opts

        let fingerprint = hash (sessionVersion res)
        return (BS.pack (show fingerprint), res)

    defineEarlyCutoff $ \GhcSession file -> do
        IdeGhcSession{loadSessionFun} <- useNoFile_ GhcSessionIO
        (val,deps) <- liftIO $ loadSessionFun $ fromNormalizedFilePath file

        -- add the deps to the Shake graph
        let addDependency fp = do
                let nfp = toNormalizedFilePath' fp
                itExists <- getFileExists nfp
                when itExists $ void $ use_ GetModificationTime nfp
        mapM_ addDependency deps

        opts <- getIdeOptions
        let cutoffHash =
              case optShakeFiles opts of
                -- optShakeFiles is only set in the DAML case.
                -- https://github.com/digital-asset/ghcide/pull/522#discussion_r428622915
                Just {} -> ""
                -- Hash the HscEnvEq returned so cutoff if it didn't change
                -- from last time
                Nothing -> BS.pack (show (hash (snd val)))
        return (Just cutoffHash, val)

    define $ \GhcSessionDeps file -> ghcSessionDepsDefinition file

ghcSessionDepsDefinition :: NormalizedFilePath -> Action (IdeResult HscEnvEq)
ghcSessionDepsDefinition file = do
        hsc <- hscEnv <$> use_ GhcSession file
        (ms,_) <- useWithStale_ GetModSummary file
        (deps,_) <- useWithStale_ GetDependencies file
        let tdeps = transitiveModuleDeps deps
        ifaces <- uses_ GetModIface tdeps

        -- Figure out whether we need TemplateHaskell or QuasiQuotes support
        let graph_needs_th_qq = needsTemplateHaskellOrQQ $ hsc_mod_graph hsc
            file_uses_th_qq   = uses_th_qq $ ms_hspp_opts ms
            any_uses_th_qq    = graph_needs_th_qq || file_uses_th_qq

        bytecodes <- if any_uses_th_qq
            then -- If we use TH or QQ, we must obtain the bytecode
            fmap Just <$> uses_ GenerateByteCode (transitiveModuleDeps deps)
            else
            pure $ repeat Nothing

        -- Currently GetDependencies returns things in topological order so A comes before B if A imports B.
        -- We need to reverse this as GHC gets very unhappy otherwise and complains about broken interfaces.
        -- Long-term we might just want to change the order returned by GetDependencies
        let inLoadOrder = reverse (zipWith unpack ifaces bytecodes)

        (session',_) <- liftIO $ runGhcEnv hsc $ do
            setupFinderCache (map hirModSummary ifaces)
            mapM_ (uncurry loadDepModule) inLoadOrder

        res <- liftIO $ newHscEnvEq session' []
        return ([], Just res)
 where
  unpack HiFileResult{..} bc = (hirModIface, bc)
  uses_th_qq dflags =
    xopt LangExt.TemplateHaskell dflags || xopt LangExt.QuasiQuotes dflags

getModIfaceFromDiskRule :: Rules ()
getModIfaceFromDiskRule = defineEarlyCutoff $ \GetModIfaceFromDisk f -> do
  ms <- use_ GetModSummary f
  (diags_session, mb_session) <- ghcSessionDepsDefinition f
  case mb_session of
      Nothing -> return (Nothing, (diags_session, Nothing))
      Just session -> do
        let hiFile = toNormalizedFilePath'
                    $ case ms_hsc_src ms of
                        HsBootFile -> addBootSuffix (ml_hi_file $ ms_location ms)
                        _ -> ml_hi_file $ ms_location ms
        mbHiVersion <- use  GetModificationTime_{missingFileDiagnostics=False} hiFile
        modVersion  <- use_ GetModificationTime f
        let sourceModified = case mbHiVersion of
                Nothing -> SourceModified
                Just x -> if modificationTime x >= modificationTime modVersion
                            then SourceUnmodified else SourceModified
        r <- loadInterface (hscEnv session) ms sourceModified (regenerateHiFile session f)
        case r of
            (diags, Just x) -> do
                let fp = fingerprintToBS (getModuleHash (hirModIface x))
                return (Just fp, (diags <> diags_session, Just x))
            (diags, Nothing) -> return (Nothing, (diags ++ diags_session, Nothing))

getModSummaryRule :: Rules ()
getModSummaryRule = defineEarlyCutoff $ \GetModSummary f -> do
    dflags <- hsc_dflags . hscEnv <$> use_ GhcSession f
    (_, mFileContent) <- getFileContents f
    modS <- liftIO $ evalWithDynFlags dflags $ runExceptT $
        getModSummaryFromImports (fromNormalizedFilePath f) (textToStringBuffer <$> mFileContent)
    case modS of
        Right ms -> do
            -- Clear the contents as no longer needed
            let !ms' = ms{ms_hspp_buf=Nothing}
            return ( Just (computeFingerprint f dflags ms), ([], Just ms'))
        Left diags -> return (Nothing, (diags, Nothing))
    where
        -- Compute a fingerprint from the contents of `ModSummary`,
        -- eliding the timestamps and other non relevant fields.
        computeFingerprint f dflags ModSummary{..} =
            let fingerPrint =
                    ( moduleNameString (moduleName ms_mod)
                    , ms_hspp_file
                    , map unLoc opts
                    , ml_hs_file ms_location
                    , fingerPrintImports ms_srcimps
                    , fingerPrintImports ms_textual_imps
                    )
                fingerPrintImports = map (fmap uniq *** (moduleNameString . unLoc))
                opts = Hdr.getOptions dflags (fromJust ms_hspp_buf) (fromNormalizedFilePath f)
                fp = hash fingerPrint
            in BS.pack (show fp)

getModIfaceRule :: Rules ()
getModIfaceRule = define $ \GetModIface f -> do
#if MIN_GHC_API_VERSION(8,6,0) && !defined(GHC_LIB)
    fileOfInterest <- use_ IsFileOfInterest f
    if fileOfInterest
        then do
            -- Never load from disk for files of interest
            tmr <- use TypeCheck f
            return ([], extractHiFileResult tmr)
        else
            ([],) <$> use GetModIfaceFromDisk f
#else
    tm <- use TypeCheck f
    let modIface = hm_iface . tmrModInfo <$> tm
        modSummary = tmrModSummary <$> tm
    return ([], HiFileResult <$> modSummary <*> modIface)
#endif

regenerateHiFile :: HscEnvEq -> NormalizedFilePath -> Action ([FileDiagnostic], Maybe HiFileResult)
regenerateHiFile sess f = do
    let hsc = hscEnv sess
        -- After parsing the module remove all package imports referring to
        -- these packages as we have already dealt with what they map to.
        comp_pkgs = mapMaybe (fmap fst . mkImportDirs (hsc_dflags hsc)) (deps sess)
    opt <- getIdeOptions
    (_, contents) <- getFileContents f
    -- Embed --haddocks in the interface file
    (_, (diags, mb_pm)) <- liftIO $ getParsedModuleDefinition (withOptHaddock hsc) opt comp_pkgs f contents
    (diags, mb_pm) <- case mb_pm of
        Just _ -> return (diags, mb_pm)
        Nothing -> do
            -- if parsing fails, try parsing again with Haddock turned off
            (_, (diagsNoHaddock, mb_pm)) <- liftIO $ getParsedModuleDefinition hsc opt comp_pkgs f contents
            return (mergeParseErrorsHaddock diagsNoHaddock diags, mb_pm)
    case mb_pm of
        Nothing -> return (diags, Nothing)
        Just pm -> do
            -- Invoke typechecking directly to update it without incurring a dependency
            -- on the parsed module and the typecheck rules
            (diags', tmr) <- typeCheckRuleDefinition hsc pm DoGenerateInterfaceFiles
            -- Bang pattern is important to avoid leaking 'tmr'
            let !res = extractHiFileResult tmr
            return (diags <> diags', res)

extractHiFileResult :: Maybe TcModuleResult -> Maybe HiFileResult
extractHiFileResult Nothing = Nothing
extractHiFileResult (Just tmr) =
    -- Bang patterns are important to force the inner fields
    Just $! HiFileResult (tmrModSummary tmr) (hm_iface $ tmrModInfo tmr)

isFileOfInterestRule :: Rules ()
isFileOfInterestRule = defineEarlyCutoff $ \IsFileOfInterest f -> do
    filesOfInterest <- getFilesOfInterest
    let res = f `elem` filesOfInterest
    return (Just (if res then "1" else ""), ([], Just res))

-- | A rule that wires per-file rules together
mainRule :: Rules ()
mainRule = do
    getParsedModuleRule
    getLocatedImportsRule
    getDependencyInformationRule
    reportImportCyclesRule
    getDependenciesRule
    typeCheckRule
    getSpanInfoRule
    generateCoreRule
    generateByteCodeRule
    loadGhcSession
    getModIfaceFromDiskRule
    getModIfaceRule
    isFileOfInterestRule
    getModSummaryRule
