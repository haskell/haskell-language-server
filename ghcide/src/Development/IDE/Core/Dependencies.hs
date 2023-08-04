module Development.IDE.Core.Dependencies
  ( indexDependencyHieFiles
  ) where

import           Control.Concurrent.STM          (atomically)
import           Control.Concurrent.STM.TQueue   (writeTQueue)
import           Control.Monad                   (unless, void)
import           Data.Foldable                   (traverse_)
import qualified Data.Map                        as Map
import           Data.Maybe                      (isNothing)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Development.IDE.Core.Compile    (indexHieFile)
import           Development.IDE.Core.Rules      (HieFileCheck(..), Log, checkHieFile)
import           Development.IDE.Core.Shake      (HieDbWriter(indexQueue), ShakeExtras(hiedbWriter, lspEnv, withHieDb))
import qualified Development.IDE.GHC.Compat      as GHC
import           Development.IDE.Types.Location  (NormalizedFilePath, toNormalizedFilePath')
import           Development.IDE.Types.Logger    (Recorder, WithPriority)
import           HieDb                           (SourceFile(FakeFile), lookupPackage, removeDependencySrcFiles)
import           Language.LSP.Server             (resRootPath)
import           System.Directory                (doesDirectoryExist)
import           System.FilePath                 ((</>), (<.>))

newtype Package = Package GHC.UnitInfo deriving Eq
instance Ord Package where
  compare (Package u1) (Package u2) = compare (GHC.unitId u1) (GHC.unitId u2)

indexDependencyHieFiles :: Recorder (WithPriority Log) -> ShakeExtras -> GHC.HscEnv -> IO ()
indexDependencyHieFiles recorder se hscEnv = do
    dotHlsDirExists <- maybe (pure False) doesDirectoryExist mHlsDir
    unless dotHlsDirExists deleteMissingDependencySources
    void $ Map.traverseWithKey indexPackageHieFiles packagesWithModules
    where
        mHlsDir :: Maybe FilePath
        mHlsDir = do
            projectDir <- resRootPath =<< lspEnv se
            pure $ projectDir </> ".hls"
        deleteMissingDependencySources :: IO ()
        deleteMissingDependencySources =
            atomically $ writeTQueue (indexQueue $ hiedbWriter se) $
                \withHieDb ->
                    withHieDb $ \db ->
                        removeDependencySrcFiles db
        indexPackageHieFiles :: Package -> [GHC.Module] -> IO ()
        indexPackageHieFiles (Package package) modules = do
            let pkgLibDir :: FilePath
                pkgLibDir = case GHC.unitLibraryDirs package of
                  [] -> ""
                  (libraryDir : _) -> libraryDir
                hieDir :: FilePath
                hieDir = pkgLibDir </> "extra-compilation-artifacts"
                unit :: GHC.Unit
                unit = GHC.RealUnit $ GHC.Definite $ GHC.unitId package
            moduleRows <- withHieDb se $ \db ->
                lookupPackage db unit
            case moduleRows of
                [] -> traverse_ (indexModuleHieFile hieDir) modules
                _  -> return ()
        indexModuleHieFile :: FilePath -> GHC.Module -> IO ()
        indexModuleHieFile hieDir m = do
            let hiePath :: NormalizedFilePath
                hiePath = toNormalizedFilePath' $
                  hieDir </> GHC.moduleNameSlashes (GHC.moduleName m) <.> "hie"
            hieCheck <- checkHieFile recorder se "newHscEnvEqWithImportPaths" hiePath
            case hieCheck of
                HieFileMissing -> return ()
                HieAlreadyIndexed -> return ()
                CouldNotLoadHie _e -> return ()
                DoIndexing hash hie ->
                    indexHieFile se hiePath (FakeFile Nothing) hash hie
        packagesWithModules :: Map.Map Package [GHC.Module]
        packagesWithModules = Map.fromSet getModulesForPackage packages
        packages :: Set Package
        packages = Set.fromList
            $ map Package
            $ Map.elems
            $ Map.filterWithKey (\uid _ -> uid `Set.member` dependencyIds) unitInfoMap
            where
                unitInfoMap :: GHC.UnitInfoMap
                unitInfoMap = GHC.getUnitInfoMap hscEnv
                dependencyIds :: Set GHC.UnitId
                dependencyIds =
                    calculateTransitiveDependencies unitInfoMap directDependencyIds directDependencyIds
                directDependencyIds :: Set GHC.UnitId
                directDependencyIds = Set.fromList
                    $ map GHC.toUnitId
                    $ GHC.explicitUnits
                    $ GHC.unitState hscEnv

calculateTransitiveDependencies :: GHC.UnitInfoMap -> Set GHC.UnitId -> Set GHC.UnitId -> Set GHC.UnitId
calculateTransitiveDependencies unitInfoMap allDependencies newDepencencies
    | Set.null newDepencencies = allDependencies
    | otherwise = calculateTransitiveDependencies unitInfoMap nextAll nextNew
    where
        nextAll :: Set GHC.UnitId
        nextAll = Set.union allDependencies nextNew
        nextNew :: Set GHC.UnitId
        nextNew = flip Set.difference allDependencies
            $ Set.unions
            $ map (Set.fromList . GHC.unitDepends)
            $ Map.elems
            $ Map.filterWithKey (\uid _ -> uid `Set.member` newDepencencies) unitInfoMap

getModulesForPackage :: Package -> [GHC.Module]
getModulesForPackage (Package package) =
    map makeModule allModules
    where
        allModules :: [GHC.ModuleName]
        allModules = map fst
            ( filter (isNothing . snd)
            $ GHC.unitExposedModules package
            )
            ++ GHC.unitHiddenModules package
        makeModule :: GHC.ModuleName
                   -> GHC.Module
        makeModule = GHC.mkModule (GHC.unitInfoId package)
