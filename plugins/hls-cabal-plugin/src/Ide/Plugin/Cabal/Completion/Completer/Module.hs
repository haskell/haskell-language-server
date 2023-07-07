{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Completion.Completer.Module where

import           Control.Monad                                  (filterM)
import           Control.Monad.Extra                            (concatForM,
                                                                 forM)
import           Data.List                                      (stripPrefix)
import qualified Data.List                                      as List
import           Data.Maybe                                     (fromMaybe)
import qualified Data.Text                                      as T
import           Distribution.PackageDescription                (Benchmark (..),
                                                                 BuildInfo (..),
                                                                 CondTree (condTreeData),
                                                                 Executable (..),
                                                                 GenericPackageDescription (..),
                                                                 Library (..),
                                                                 UnqualComponentName,
                                                                 mkUnqualComponentName,
                                                                 testBuildInfo)
import           Distribution.Utils.Path                        (getSymbolicPath)
import           Ide.Logger                                     (Priority (..),
                                                                 Recorder,
                                                                 WithPriority,
                                                                 logWith)
import           Ide.Plugin.Cabal.Completion.Completer.FilePath (PathCompletionInfo (..),
                                                                 listFileCompletions,
                                                                 mkCompletionDirectory)
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types
import           Ide.Plugin.Cabal.Completion.Types
import           System.Directory                               (doesFileExist)
import qualified System.FilePath                                as FP
import qualified Text.Fuzzy.Parallel                            as Fuzzy

-- | Completer to be used when module paths can be completed for the field.
--
-- Takes an extraction function which extracts the source directories
-- to be used by the completer.
modulesCompleter :: (Maybe StanzaName -> GenericPackageDescription -> [FilePath]) -> Completer
modulesCompleter extractionFunction recorder cData = do
  mGPD <- getLatestGPD cData
  case mGPD of
    Just gpd -> do
      let sourceDirs = extractionFunction sName gpd
      filePathCompletions <-
        filePathsForExposedModules recorder sourceDirs prefInfo
      pure $ map (\compl -> mkSimpleCompletionItem (completionRange prefInfo) compl) filePathCompletions
    Nothing -> do
      logWith recorder Debug LogUseWithStaleFastNoResult
      pure []
  where
    sName = stanzaName cData
    prefInfo = cabalPrefixInfo cData

-- | Extracts the source directories of the library stanza.
sourceDirsExtractionLibrary :: Maybe StanzaName -> GenericPackageDescription -> [FilePath]
sourceDirsExtractionLibrary Nothing gpd =
  -- we use condLibrary to get the information contained in the library stanza
  -- since the library in PackageDescription is not populated by us
  case libM of
    Just lib -> do
      map getSymbolicPath $ hsSourceDirs $ libBuildInfo $ condTreeData lib
    Nothing -> []
  where
    libM = condLibrary gpd
sourceDirsExtractionLibrary name gpd = extractRelativeDirsFromStanza name gpd condSubLibraries libBuildInfo

-- | Extracts the source directories of the executable stanza with the given name.
sourceDirsExtractionExecutable :: Maybe StanzaName -> GenericPackageDescription -> [FilePath]
sourceDirsExtractionExecutable name gpd = extractRelativeDirsFromStanza name gpd condExecutables buildInfo

-- | Extracts the source directories of the test suite stanza with the given name.
sourceDirsExtractionTestSuite :: Maybe StanzaName -> GenericPackageDescription -> [FilePath]
sourceDirsExtractionTestSuite name gpd = extractRelativeDirsFromStanza name gpd condTestSuites testBuildInfo

-- | Extracts the source directories of benchmark stanza with the given name.
sourceDirsExtractionBenchmark :: Maybe StanzaName -> GenericPackageDescription -> [FilePath]
sourceDirsExtractionBenchmark name gpd = extractRelativeDirsFromStanza name gpd condBenchmarks benchmarkBuildInfo

-- | Takes a possible stanza name, a GenericPackageDescription,
--  a function to access the stanza information we are interested in
--  and a function to access the build info from the specific stanza.
--
--  Returns a list of relative source directory paths specified for the extracted stanza.
extractRelativeDirsFromStanza ::
  Maybe StanzaName ->
  GenericPackageDescription ->
  (GenericPackageDescription -> [(UnqualComponentName, CondTree b c a)]) ->
  (a -> BuildInfo) ->
  [FilePath]
extractRelativeDirsFromStanza Nothing _ _ _ = []
extractRelativeDirsFromStanza (Just name) gpd getStanza getBuildInfo
  | Just stanza <- stanzaM = map getSymbolicPath $ hsSourceDirs $ getBuildInfo stanza
  | otherwise = []
  where
    stanzaM = fmap (condTreeData . snd) res
    allStanzasM = getStanza gpd
    res =
      List.find
        ( \(n, _) ->
            n == mkUnqualComponentName (T.unpack name)
        )
        allStanzasM

-- | Takes a list of source directories and returns a list of path completions
--  relative to any of the passed source directories which fit the passed prefix info.
filePathsForExposedModules :: Recorder (WithPriority Log) -> [FilePath] -> CabalPrefixInfo -> IO [T.Text]
filePathsForExposedModules recorder srcDirs prefInfo = do
  concatForM
    srcDirs
    ( \dir' -> do
        let dir = FP.normalise dir'
        let pInfo =
              PathCompletionInfo
                { isStringNotationPath = Nothing,
                  pathSegment = T.pack $ FP.takeFileName prefix,
                  queryDirectory = FP.addTrailingPathSeparator $ FP.takeDirectory prefix,
                  workingDirectory = completionWorkingDir prefInfo FP.</> dir
                }
        completions <- listFileCompletions recorder pInfo
        validExposedCompletions <- filterM (isValidExposedModulePath pInfo) completions
        let toMatch = pathSegment pInfo
            scored = Fuzzy.simpleFilter Fuzzy.defChunkSize Fuzzy.defMaxResults toMatch (map T.pack validExposedCompletions)
        forM
          scored
          ( \compl' -> do
              let compl = Fuzzy.original compl'
              fullFilePath <- mkExposedModulePathCompletion pInfo $ T.unpack compl
              pure fullFilePath
          )
    )
  where
    prefix =
      exposedModulePathToFp $
        completionPrefix prefInfo
    -- \| Takes a PathCompletionInfo and a path segment and checks whether
    --    the path segment can be completed for an exposed module.
    --
    --    This is the case if the segment represents either a directory or a Haskell file.
    --
    isValidExposedModulePath :: PathCompletionInfo -> FilePath -> IO Bool
    isValidExposedModulePath pInfo path = do
      let dir = mkCompletionDirectory pInfo
      fileExists <- doesFileExist (dir FP.</> path)
      pure $ not fileExists || FP.takeExtension path `elem` [".hs", ".lhs"]

-- | Takes a pathCompletionInfo and a path segment and generates the whole
--  filepath to be written on completion including a possibly already written prefix;
--  using the cabal syntax for exposed modules.
--
--  Examples:
--  When the partial directory path `Dir.Dir2.` is stored in the PathCompletionInfo
--  and the completed file `HaskellFile.hs` is passed along with that PathCompletionInfo,
--  the result would be `Dir1.Dir2.HaskellFile`
--
--  When the partial directory path `Dir.` is stored in the PathCompletionInfo
--  and the completed directory `Dir2` is passed along with that PathCompletionInfo,
--  the result would be `Dir1.Dir2.`
mkExposedModulePathCompletion :: PathCompletionInfo -> FilePath -> IO T.Text
mkExposedModulePathCompletion complInfo completion = do
  let combinedPath = queryDirectory complInfo FP.</> completion
  isFilePath <- doesFileExist (workingDirectory complInfo FP.</> combinedPath)
  let addTrailingDot modPath = if isFilePath then modPath else modPath <> "."
  let exposedPath = FP.makeRelative "." combinedPath
  pure $ addTrailingDot $ fpToExposedModulePath "" exposedPath

-- | Takes a source directory path and a module path and returns
--  the module path relative to the source directory
--  in exposed module syntax where the separators are '.'
--  and the file ending is removed.
--
-- Synopsis: @'fpToExposedModulePath' sourceDir modPath@.
fpToExposedModulePath :: FilePath -> FilePath -> T.Text
fpToExposedModulePath sourceDir modPath =
  T.intercalate "." $ fmap T.pack $ FP.splitDirectories $ FP.dropExtension fp
  where
    fp = fromMaybe modPath $ stripPrefix sourceDir modPath

-- | Takes a path in the exposed module syntax and translates it to a platform-compatible file path.
exposedModulePathToFp :: T.Text -> FilePath
exposedModulePathToFp fp = T.unpack $ T.replace "." (T.singleton FP.pathSeparator) fp
