-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | A Shake implementation of the compiler service, built
--   using the "Shaker" abstraction layer for in-memory use.
--
module Development.IDE.Core.Rules(
    IdeState, GetDependencies(..), GetParsedModule(..), TransitiveDependencies(..),
    Priority(..),
    priorityTypeCheck,
    priorityGenerateCore,
    priorityFilesOfInterest,
    runAction, useE, useNoFileE, usesE,
    toIdeResult, defineNoFile,
    mainRule,
    getGhcCore,
    getAtPoint,
    getDefinition,
    getDependencies,
    getParsedModule,
    fileFromParsedModule,
    writeIfacesAndHie,
    ) where

import           Control.Monad.Except
import Control.Monad.Trans.Maybe
import Development.IDE.Core.Compile
import Development.IDE.Types.Options
import Development.IDE.Spans.Calculate
import Development.IDE.Import.DependencyInformation
import Development.IDE.Import.FindImports
import           Development.IDE.Core.FileStore
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Data.Coerce
import Data.Either.Extra
import Data.Maybe
import           Data.Foldable
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Development.IDE.GHC.Error
import           Development.Shake                        hiding (Diagnostic, Env, newCache)
import Development.IDE.Core.RuleTypes

import           GHC hiding (parseModule, typecheckModule)
import Development.IDE.GHC.Compat
import           UniqSupply
import NameCache
import HscTypes

import qualified Development.IDE.Spans.AtPoint as AtPoint
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import System.Directory
import System.FilePath
import MkIface

-- | This is useful for rules to convert rules that can only produce errors or
-- a result into the more general IdeResult type that supports producing
-- warnings while also producing a result.
toIdeResult :: Either [FileDiagnostic] v -> IdeResult v
toIdeResult = either (, Nothing) (([],) . Just)

-- | useE is useful to implement functions that aren’t rules but need shortcircuiting
-- e.g. getDefinition.
useE :: IdeRule k v => k -> NormalizedFilePath -> MaybeT Action v
useE k = MaybeT . use k

useNoFileE :: IdeRule k v => k -> MaybeT Action v
useNoFileE k = useE k ""

usesE :: IdeRule k v => k -> [NormalizedFilePath] -> MaybeT Action [v]
usesE k = MaybeT . fmap sequence . uses k

defineNoFile :: IdeRule k v => (k -> Action v) -> Rules ()
defineNoFile f = define $ \k file -> do
    if file == "" then do res <- f k; return ([], Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"


------------------------------------------------------------
-- Exposed API


-- | Generate the GHC Core for the supplied file and its dependencies.
getGhcCore :: NormalizedFilePath -> Action (Maybe [CoreModule])
getGhcCore file = runMaybeT $ do
    files <- transitiveModuleDeps <$> useE GetDependencies file
    pms   <- usesE GetParsedModule $ files ++ [file]
    usesE GenerateCore $ map fileFromParsedModule pms



-- | Get all transitive file dependencies of a given module.
-- Does not include the file itself.
getDependencies :: NormalizedFilePath -> Action (Maybe [NormalizedFilePath])
getDependencies file = fmap transitiveModuleDeps <$> use GetDependencies file

-- | Try to get hover text for the name under point.
getAtPoint :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
getAtPoint file pos = fmap join $ runMaybeT $ do
  opts <- lift getIdeOptions
  files <- transitiveModuleDeps <$> useE GetDependencies file
  tms   <- usesE TypeCheck (file : files)
  spans <- useE GetSpanInfo file
  return $ AtPoint.atPoint opts (map tmrModule tms) spans pos

-- | Goto Definition.
getDefinition :: NormalizedFilePath -> Position -> Action (Maybe Location)
getDefinition file pos = fmap join $ runMaybeT $ do
    spans <- useE GetSpanInfo file
    pkgState <- useNoFileE GhcSession
    opts <- lift getIdeOptions
    let getHieFile x = useNoFile (GetHieFile x)
    lift $ AtPoint.gotoDefinition getHieFile opts pkgState spans pos

-- | Parse the contents of a daml file.
getParsedModule :: NormalizedFilePath -> Action (Maybe ParsedModule)
getParsedModule file = use GetParsedModule file

-- | Write interface files and hie files to the location specified by the given options.
writeIfacesAndHie ::
       NormalizedFilePath -> [NormalizedFilePath] -> Action (Maybe [NormalizedFilePath])
writeIfacesAndHie ifDir files =
    runMaybeT $ do
        tcms <- usesE TypeCheck files
        session <- lift $ useNoFile_ GhcSession
        liftIO $ concat <$> mapM (writeTcm session) tcms
  where
    writeTcm session tcm =
        do
            let fp =
                    fromNormalizedFilePath ifDir </>
                    (ms_hspp_file $
                     pm_mod_summary $ tm_parsed_module $ tmrModule tcm)
            createDirectoryIfMissing True (takeDirectory fp)
            let ifaceFp = replaceExtension fp ".hi"
            let hieFp = replaceExtension fp ".hie"
            writeIfaceFile
                (hsc_dflags session)
                ifaceFp
                (hm_iface $ tmrModInfo tcm)
            hieFile <-
                liftIO $
                runHsc session $
                mkHieFile
                    (pm_mod_summary $ tm_parsed_module $ tmrModule tcm)
                    (fst $ tm_internals_ $ tmrModule tcm)
                    (fromJust $ tm_renamed_source $ tmrModule tcm)
            writeHieFile hieFp hieFile
            pure [toNormalizedFilePath ifaceFp, toNormalizedFilePath hieFp]

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
getParsedModuleRule =
    define $ \GetParsedModule file -> do
        (_, contents) <- getFileContents file
        packageState <- useNoFile_ GhcSession
        opt <- getIdeOptions
        liftIO $ parseModule opt packageState (fromNormalizedFilePath file) contents

getLocatedImportsRule :: Rules ()
getLocatedImportsRule =
    define $ \GetLocatedImports file -> do
        pm <- use_ GetParsedModule file
        let ms = pm_mod_summary pm
        let imports = ms_textual_imps ms
        env <- useNoFile_ GhcSession
        let dflags = addRelativeImport pm $ hsc_dflags env
        opt <- getIdeOptions
        (diags, imports') <- fmap unzip $ forM imports $ \(mbPkgName, modName) -> do
            diagOrImp <- locateModule dflags (optExtensions opt) getFileExists modName mbPkgName
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


-- | Given a target file path, construct the raw dependency results by following
-- imports recursively.
rawDependencyInformation :: NormalizedFilePath -> Action RawDependencyInformation
rawDependencyInformation f = do
    let (initialId, initialMap) = getPathId f emptyPathIdMap
    go (IntSet.singleton $ getFilePathId initialId)
       (RawDependencyInformation IntMap.empty initialMap)
  where
    go fs rawDepInfo =
        case IntSet.minView fs of
            -- Queue is empty
            Nothing -> pure rawDepInfo
            -- Pop f from the queue and process it
            Just (f, fs) -> do
                let fId = FilePathId f
                importsOrErr <- use GetLocatedImports $ idToPath (rawPathIdMap rawDepInfo) fId
                case importsOrErr of
                  Nothing ->
                    -- File doesn’t parse
                    let rawDepInfo' = insertImport fId (Left ModuleParseError) rawDepInfo
                    in go fs rawDepInfo'
                  Just (modImports, pkgImports) -> do
                    let f :: PathIdMap -> (a, Maybe NormalizedFilePath) -> (PathIdMap, (a, Maybe FilePathId))
                        f pathMap (imp, mbPath) = case mbPath of
                            Nothing -> (pathMap, (imp, Nothing))
                            Just path ->
                                let (pathId, pathMap') = getPathId path pathMap
                                in (pathMap', (imp, Just pathId))
                    -- Convert paths in imports to ids and update the path map
                    let (pathIdMap, modImports') = mapAccumL f (rawPathIdMap rawDepInfo) modImports
                    -- Files that we haven’t seen before are added to the queue.
                    let newFiles =
                            IntSet.fromList (coerce $ mapMaybe snd modImports')
                            IntSet.\\ IntMap.keysSet (rawImports rawDepInfo)
                    let rawDepInfo' = insertImport fId (Right $ ModuleImports modImports' pkgImports) rawDepInfo
                    go (newFiles `IntSet.union` fs) (rawDepInfo' { rawPathIdMap = pathIdMap })

getDependencyInformationRule :: Rules ()
getDependencyInformationRule =
    define $ \GetDependencyInformation file -> do
       rawDepInfo <- rawDependencyInformation file
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
          toDiag imp mods = (fp ,) $ Diagnostic
            { _range = (_range :: Location -> Range) loc
            , _severity = Just DsError
            , _source = Just "Import cycle detection"
            , _message = "Cyclic module dependency between " <> showCycle mods
            , _code = Nothing
            , _relatedInformation = Nothing
            }
            where loc = srcSpanToLocation (getLoc imp)
                  fp = toNormalizedFilePath $ srcSpanToFilename (getLoc imp)
          getModuleName file = do
           pm <- use_ GetParsedModule file
           pure (moduleNameString . moduleName . ms_mod $ pm_mod_summary pm)
          showCycle mods  = T.intercalate ", " (map T.pack mods)

-- returns all transitive dependencies in topological order.
-- NOTE: result does not include the argument file.
getDependenciesRule :: Rules ()
getDependenciesRule =
    define $ \GetDependencies file -> do
        depInfo@DependencyInformation{..} <- use_ GetDependencyInformation file
        let allFiles = reachableModules depInfo
        _ <- uses_ ReportImportCycles allFiles
        return ([], transitiveDeps depInfo file)

-- Source SpanInfo is used by AtPoint and Goto Definition.
getSpanInfoRule :: Rules ()
getSpanInfoRule =
    define $ \GetSpanInfo file -> do
        tc <- use_ TypeCheck file
        (fileImports, _) <- use_ GetLocatedImports file
        packageState <- useNoFile_ GhcSession
        x <- liftIO $ getSrcSpanInfos packageState fileImports tc
        return ([], Just x)

-- Typechecks a module.
typeCheckRule :: Rules ()
typeCheckRule =
    define $ \TypeCheck file -> do
        pm <- use_ GetParsedModule file
        deps <- use_ GetDependencies file
        tms <- uses_ TypeCheck (transitiveModuleDeps deps)
        setPriority priorityTypeCheck
        packageState <- useNoFile_ GhcSession
        liftIO $ typecheckModule packageState tms pm


generateCoreRule :: Rules ()
generateCoreRule =
    define $ \GenerateCore file -> do
        deps <- use_ GetDependencies file
        (tm:tms) <- uses_ TypeCheck (file:transitiveModuleDeps deps)
        setPriority priorityGenerateCore
        packageState <- useNoFile_ GhcSession
        liftIO $ compileModule packageState tms tm

loadGhcSession :: Rules ()
loadGhcSession =
    defineNoFile $ \GhcSession -> do
        opts <- getIdeOptions
        optGhcSession opts


getHieFileRule :: Rules ()
getHieFileRule =
    defineNoFile $ \(GetHieFile f) -> do
        u <- liftIO $ mkSplitUniqSupply 'a'
        let nameCache = initNameCache u []
        liftIO $ fmap (hie_file_result . fst) $ readHieFile nameCache f

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
    loadGhcSession
    getHieFileRule

------------------------------------------------------------

fileFromParsedModule :: ParsedModule -> NormalizedFilePath
fileFromParsedModule = toNormalizedFilePath . ms_hspp_file . pm_mod_summary
