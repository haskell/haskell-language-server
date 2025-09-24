{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Completion.Completer.Module where

import           Control.Monad                                  (filterM)
import           Control.Monad.Extra                            (concatForM,
                                                                 forM)
import           Data.List                                      (stripPrefix)
import           Data.Maybe                                     (fromMaybe)
import qualified Data.Text                                      as T
import           Distribution.PackageDescription                (GenericPackageDescription)
import           Ide.Logger                                     (Priority (..),
                                                                 Recorder,
                                                                 WithPriority,
                                                                 logWith)
import           Ide.Plugin.Cabal.Completion.Completer.FilePath (listFileCompletions,
                                                                 mkCompletionDirectory)
import           Ide.Plugin.Cabal.Completion.Completer.Paths
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
      filePathCompletions <- filePathsForExposedModules recorder sourceDirs prefInfo (matcher cData)
      pure $ map (\compl -> mkSimpleCompletionItem (completionRange prefInfo) compl) filePathCompletions
    Nothing -> do
      logWith recorder Debug LogUseWithStaleFastNoResult
      pure []
  where
    sName = stanzaName cData
    prefInfo = cabalPrefixInfo cData

-- | Takes a list of source directories and returns a list of path completions
--  relative to any of the passed source directories which fit the passed prefix info.
filePathsForExposedModules
  :: Recorder (WithPriority Log)
  -> [FilePath]
  -> CabalPrefixInfo
  -> Matcher T.Text
  -> IO [T.Text]
filePathsForExposedModules recorder srcDirs prefInfo matcher = do
  concatForM
    srcDirs
    ( \dir' -> do
        let dir = FP.normalise dir'
            pathInfo = pathCompletionInfoFromCabalPrefixInfo dir modPrefInfo
        completions <- listFileCompletions recorder pathInfo
        validExposedCompletions <- filterM (isValidExposedModulePath pathInfo) completions
        let toMatch = pathSegment pathInfo
            scored = runMatcher
              matcher
              toMatch
              (map T.pack validExposedCompletions)
        forM
          scored
          ( \compl' -> do
              let compl = Fuzzy.original compl'
              fullFilePath <- mkExposedModulePathCompletion pathInfo $ T.unpack compl
              pure fullFilePath
          )
    )
  where
    prefix =
      T.pack $ exposedModulePathToFp $
        completionPrefix prefInfo
    -- build completion info relative to the source dir,
    -- we overwrite the prefix written in the cabal file with its translation
    -- to filepath syntax, since it is in exposed module syntax
    modPrefInfo = prefInfo{completionPrefix=prefix}

    --    Takes a PathCompletionInfo and a path segment and checks whether
    --    the path segment can be completed for an exposed module.
    --
    --    This is the case if the segment represents either a directory or a Haskell file.
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
