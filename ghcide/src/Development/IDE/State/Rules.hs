-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.State.Rules(
    IdeState, GetDependencies(..), GetParsedModule(..), TransitiveDependencies(..),
    Priority(..),
    runAction, runActions, useE, usesE,
    toIdeResultNew, defineNoFile,
    mainRule,
    getGhcCore,
    getAtPoint,
    getDefinition,
    getDependencies,
    getDalfDependencies,
    fileFromParsedModule
    ) where

import           Control.Concurrent.Extra
import Control.Exception (evaluate)
import Control.Lens (set)
import           Control.Monad.Except
import Control.Monad.Extra (whenJust)
import qualified Development.IDE.Functions.Compile             as Compile
import Development.IDE.Functions.DependencyInformation
import Development.IDE.Functions.FindImports
import           Development.IDE.State.FileStore
import           Development.IDE.Types.Diagnostics as Base
import Development.IDE.UtilGHC
import Data.Bifunctor
import Data.Either.Extra
import Data.Maybe
import           Data.Foldable
import qualified Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Development.IDE.Functions.GHCError
import           Development.Shake                        hiding (Diagnostic, Env, newCache)
import           Development.IDE.Types.LSP as Compiler
import Development.IDE.State.RuleTypes

import           "ghc-lib" GHC
import           "ghc-lib-parser" UniqSupply
import           "ghc-lib-parser" Module                         as M

import qualified Development.IDE.Functions.AtPoint as AtPoint
import Development.IDE.State.Service
import Development.IDE.State.Shake

-- LEGACY STUFF ON THE OLD STYLE

toIdeResultNew :: Either [Diagnostic] v -> IdeResult v
toIdeResultNew = either (, Nothing) (([],) . Just)

-- Convert to a legacy Ide result but dropping dependencies
toIdeResultSilent :: Maybe v -> Either [Diagnostic] v
toIdeResultSilent val = maybe (Left []) Right val


defineNoFile :: IdeRule k v => (k -> Action v) -> Rules ()
defineNoFile f = define $ \k file -> do
    if file == "" then do res <- f k; return ([], Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"


-- | Return a distinct supply of uniques.
getUniqSupply :: Action UniqSupply
getUniqSupply =
    getServiceEnv >>= liftIO . getUniqSupplyFrom

getUniqSupplyFrom :: Env -> IO UniqSupply
getUniqSupplyFrom Env{..} =
    modifyVar envUniqSupplyVar $ evaluate . splitUniqSupply


------------------------------------------------------------
-- Exposed API


-- | Get GHC Core for the supplied file.
getGhcCore :: FilePath -> Action (Maybe [CoreModule])
getGhcCore file = eitherToMaybe <$> runExceptT (coresForFile file)

-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: FilePath -> Action (Maybe [FilePath])
getDependencies file =
  eitherToMaybe <$>
  (runExceptT $ transitiveModuleDeps <$> useE GetDependencies file)

getDalfDependencies :: FilePath -> Action (Maybe [InstalledUnitId])
getDalfDependencies file =
  eitherToMaybe <$>
  (runExceptT $ transitivePkgDeps <$> useE GetDependencies file)

-- | Documentation at point.
getAtPoint :: FilePath -> Position -> Action (Maybe (Maybe Range, [HoverText]))
getAtPoint file pos = do
    fmap (either (const Nothing) id) . runExceptT $ getAtPointForFile file pos

-- | Goto Definition.
getDefinition :: FilePath -> Position -> Action (Maybe Location)
getDefinition file pos = do
    fmap (either (const Nothing) id) . runExceptT $ getDefinitionForFile file pos


------------------------------------------------------------
-- Internal Actions

useE
    :: IdeRule k v
    => k -> FilePath -> ExceptT [Diagnostic] Action v
useE k = ExceptT . fmap toIdeResultSilent . use k

-- picks the first error
usesE
    :: IdeRule k v
    => k -> [FilePath] -> ExceptT [Diagnostic] Action [v]
usesE k = ExceptT . fmap (mapM toIdeResultSilent) . uses k

-- | Generate the GHC Core for the supplied file and its dependencies.
coresForFile :: FilePath -> ExceptT [Diagnostic] Action [CoreModule]
coresForFile file = do
    files <- transitiveModuleDeps <$> useE  GetDependencies file
    pms   <- usesE GetParsedModule $ files ++ [file]
    fs <- liftIO
          . mapM fileFromParsedModule
          . filter (not . modIsInternal . ms_mod . pm_mod_summary)
          $ pms
    cores <- usesE GenerateCore fs
    pure (map Compile.gmCore cores)

-- | Try to get hover text for the name under point.
getAtPointForFile
  :: FilePath
  -> Position
  -> ExceptT [Diagnostic] Action (Maybe (Maybe Range, [HoverText]))
getAtPointForFile file pos = do
  files <- transitiveModuleDeps <$> useE GetDependencies file
  tms   <- usesE TypeCheck (file : files)
  spans <- useE  GetSpanInfo file
  return $ AtPoint.atPoint (map Compile.tmrModule tms) spans pos

getDefinitionForFile :: FilePath -> Position -> ExceptT [Diagnostic] Action (Maybe Location)
getDefinitionForFile file pos = do
    spans <- useE GetSpanInfo file
    return $ AtPoint.gotoDefinition spans pos

getOpts :: Action Compile.CompileOpts
getOpts = envOptions <$> getServiceEnv

------------------------------------------------------------
-- Rules
-- These typically go from key to value and are oracles.

-- TODO (MK) This should be independent of DAML or move out of haskell-ide-core.
-- | We build artefacts based on the following high-to-low priority order.
data Priority
    = PriorityTypeCheck
    | PriorityGenerateDalf
    | PriorityFilesOfInterest
  deriving (Eq, Ord, Show, Enum)


getParsedModuleRule :: Rules ()
getParsedModuleRule =
    define $ \GetParsedModule file -> do
        contents <- getFileContents file
        packageState <- getPackageState
        opt <- getOpts
        liftIO $ Compile.parseModule opt packageState file contents

getLocatedImportsRule :: Rules ()
getLocatedImportsRule =
    define $ \GetLocatedImports file -> do
        pm <- use_ GetParsedModule file
        let ms = pm_mod_summary pm
        let imports = ms_textual_imps ms
        packageState <- getPackageState
        opt <- getOpts
        dflags <- liftIO $ Compile.getGhcDynFlags opt pm packageState
        xs <- forM imports $ \(mbPkgName, modName) ->
            (modName, ) <$> locateModule dflags getFileExists modName mbPkgName
        return (concat $ lefts $ map snd xs, Just $ map (second eitherToMaybe) xs)


-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: FilePath -> ExceptT [Diagnostic] Action RawDependencyInformation
rawDependencyInformation f = go (Set.singleton f) Map.empty Map.empty
  where go fs !modGraph !pkgs =
          case Set.minView fs of
            Nothing -> pure (RawDependencyInformation modGraph pkgs)
            Just (f, fs) -> do
              importsOrErr <- lift $ use GetLocatedImports f
              case importsOrErr of
                Nothing ->
                  let modGraph' = Map.insert f (Left ModuleParseError) modGraph
                  in go fs modGraph' pkgs
                Just imports -> do
                  packageState <- lift getPackageState
                  opt <- lift getOpts
                  modOrPkgImports <- forM imports $ \imp -> do
                    case imp of
                      (_modName, Just (PackageImport pkg)) -> do
                          pkgs <- ExceptT $ liftIO $ Compile.computePackageDeps opt packageState pkg
                          pure $ Right $ pkg:pkgs
                      (modName, Just (FileImport absFile)) -> pure $ Left (modName, Just absFile)
                      (modName, Nothing) -> pure $ Left (modName, Nothing)
                  let (modImports, pkgImports) = partitionEithers modOrPkgImports
                  let newFiles = Set.fromList (mapMaybe snd modImports) Set.\\ Map.keysSet modGraph
                      modGraph' = Map.insert f (Right modImports) modGraph
                      pkgs' = Map.insert f (Set.fromList $ concat pkgImports) pkgs
                  go (fs `Set.union` newFiles) modGraph' pkgs'

getDependencyInformationRule :: Rules ()
getDependencyInformationRule =
    define $ \GetDependencyInformation file -> fmap toIdeResultNew $ runExceptT $ do
       rawDepInfo <- rawDependencyInformation file
       pure $ processDependencyInformation rawDepInfo

reportImportCyclesRule :: Rules ()
reportImportCyclesRule =
    define $ \ReportImportCycles file -> fmap toIdeResultNew $ runExceptT $ do
        DependencyInformation{..} <- useE GetDependencyInformation file
        whenJust (Map.lookup file depErrorNodes) $ \errs -> do
            let cycles = mapMaybe (cycleErrorInFile file) (toList errs)
            when (not $ null cycles) $ do
              -- Convert cycles of files into cycles of module names
              diags <- forM cycles $ \(imp, files) -> do
                modNames <- mapM getModuleName files
                pure $ toDiag imp modNames
              throwError diags
    where cycleErrorInFile f (PartOfCycle imp fs)
            | f `elem` fs = Just (imp, fs)
          cycleErrorInFile _ _ = Nothing
          toDiag imp mods = set dLocation (Just loc) $ Diagnostic
            { _range = (_range :: Location -> Range) loc
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            }
            where loc = srcSpanToLocation (getLoc imp)
          getModuleName file = do
           pm <- useE GetParsedModule file
           pure (moduleNameString . moduleName . ms_mod $ pm_mod_summary pm)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: Rules ()
getDependenciesRule =
    define $ \GetDependencies file -> do
        depInfo@DependencyInformation{..} <- use_ GetDependencyInformation file
        let allFiles = Map.keys depModuleDeps <> Map.keys depErrorNodes
        _ <- uses_ ReportImportCycles allFiles
        return ([], transitiveDeps depInfo file)

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: Rules ()
getSpanInfoRule =
    define $ \GetSpanInfo file -> do
        pm <- use_ GetParsedModule file
        tc <- use_ TypeCheck file
        imports <- use_ GetLocatedImports file
        packageState <- getPackageState
        opt <- getOpts
        x <- liftIO $ Compile.getSrcSpanInfos opt pm packageState (fileImports imports) tc
        return ([], Just x)

-- Typechecks a module.
typeCheckRule :: Rules ()
typeCheckRule =
    define $ \TypeCheck file -> do
        pm <- use_ GetParsedModule file
        deps <- use_ GetDependencies file
        lps <- mapM (flip use_ "" . LoadPackage) (transitivePkgDeps deps)
        tms <- uses_ TypeCheck (transitiveModuleDeps deps)
        setPriority PriorityTypeCheck
        us <- getUniqSupply
        packageState <- getPackageState
        opt <- getOpts
        liftIO $ Compile.typecheckModule opt pm packageState us tms lps pm


loadPackageRule :: Rules ()
loadPackageRule =
  defineNoFile $ \(LoadPackage pkg) -> do
      packageState <- getPackageState
      opt <- getOpts
      pkgs <- liftIO $ Compile.computePackageDeps opt packageState pkg
      case pkgs of
        Left e -> do
            reportSeriousErrorDie $ "LoadPackage " ++ show pkg ++ " computePackageDeps failed, " ++ show e
        Right v -> do
            lps <- mapM (flip use_ "" . LoadPackage) v
            us <- getUniqSupply
            res <- liftIO $ Compile.loadPackage opt packageState us lps pkg
            case res of
                Left e -> reportSeriousErrorDie $ "LoadPackage " ++ show pkg ++ " loadPackage failed, " ++ show e
                Right v -> return v


generateCoreRule :: Rules ()
generateCoreRule =
    define $ \GenerateCore file -> do
        deps <- use_ GetDependencies file
        lps <- mapM (flip use_ "" . LoadPackage) (transitivePkgDeps deps)
        (tm:tms) <- uses_ TypeCheck (file:transitiveModuleDeps deps)
        let pm = tm_parsed_module . Compile.tmrModule $ tm
        setPriority PriorityGenerateDalf
        us <- getUniqSupply
        packageState <- getPackageState
        opt <- getOpts
        liftIO $ Compile.compileModule opt pm packageState us tms lps tm

generatePackageStateRule :: Rules ()
generatePackageStateRule =
    defineNoFile $ \(GeneratePackageState paths hideAllPkgs pkgImports) -> do
        liftIO $ Compile.generatePackageState paths hideAllPkgs pkgImports

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
    generatePackageStateRule
    loadPackageRule

------------------------------------------------------------

fileFromParsedModule :: ParsedModule -> IO FilePath
fileFromParsedModule = pure . ms_hspp_file . pm_mod_summary

getPackageState :: Action PackageState
getPackageState = do
  opts <- envOptions <$> getServiceEnv
  use_ (GeneratePackageState (Compile.optPackageDbs opts) (Compile.optHideAllPkgs opts) (Compile.optPackageImports opts)) ""

fileImports ::
     [(Located ModuleName, Maybe Import)]
  -> [(Located ModuleName, Maybe FilePath)]
fileImports = mapMaybe $ \case
    (modName, Nothing) -> Just (modName, Nothing)
    (modName, Just (FileImport absFile)) -> Just (modName, Just absFile)
    (_modName, Just (PackageImport _pkg)) -> Nothing
