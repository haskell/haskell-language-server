{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.Cabal.Completion.Completer.FilePath where

import           Control.Exception                            (evaluate, try)
import           Control.Monad                                (filterM)
import           Control.Monad.Extra                          (concatForM, forM)
import qualified Data.Text                                    as T
import           Distribution.PackageDescription              (GenericPackageDescription)
import           Ide.Logger
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types
import           Ide.Plugin.Cabal.Completion.Types
import           System.Directory                             (doesDirectoryExist,
                                                               doesFileExist,
                                                               listDirectory)
import qualified System.FilePath                              as FP
import qualified System.FilePath.Posix                        as Posix
import qualified Text.Fuzzy.Parallel                          as Fuzzy

-- | Completer to be used when a file path can be completed for a field.
--  Completes file paths as well as directories.
filePathCompleter :: Completer
filePathCompleter recorder cData = do
  let prefInfo = cabalPrefixInfo cData
      complInfo = pathCompletionInfoFromCabalPrefixInfo "" prefInfo
  filePathCompletions <- listFileCompletions recorder complInfo
  let scored =
        Fuzzy.simpleFilter
          Fuzzy.defChunkSize
          Fuzzy.defMaxResults
          (pathSegment complInfo)
          (map T.pack filePathCompletions)
  forM
    scored
    ( \compl' -> do
        let compl = Fuzzy.original compl'
        fullFilePath <- mkFilePathCompletion complInfo compl
        pure $ mkCompletionItem (completionRange prefInfo) fullFilePath fullFilePath
    )

mainIsCompleter :: (Maybe StanzaName -> GenericPackageDescription -> [FilePath]) -> Completer
mainIsCompleter extractionFunction recorder cData = do
  mGPD <- getLatestGPD cData
  case mGPD of
    Just gpd -> do
      let srcDirs = extractionFunction sName gpd
      concatForM srcDirs
        (\dir' -> do
        let dir = FP.normalise dir'
        let pathInfo = pathCompletionInfoFromCabalPrefixInfo dir prefInfo
        completions <- listFileCompletions recorder pathInfo
        let scored = Fuzzy.simpleFilter
              Fuzzy.defChunkSize
              Fuzzy.defMaxResults
              (pathSegment pathInfo)
              (map T.pack completions)
        forM
          scored
          ( \compl' -> do
              let compl = Fuzzy.original compl'
              fullFilePath <- mkFilePathCompletion pathInfo compl
              pure $ mkCompletionItem (completionRange prefInfo) fullFilePath fullFilePath
          )
        )
    Nothing -> do
      logWith recorder Debug LogUseWithStaleFastNoResult
      pure []
  where
    sName = stanzaName cData
    prefInfo = cabalPrefixInfo cData


-- | Completer to be used when a directory can be completed for the field.
--  Only completes directories.
directoryCompleter :: Completer
directoryCompleter recorder cData = do
  let prefInfo = cabalPrefixInfo cData
      complInfo = pathCompletionInfoFromCabalPrefixInfo "" prefInfo
  directoryCompletions <- listDirectoryCompletions recorder complInfo
  let scored =
        Fuzzy.simpleFilter
          Fuzzy.defChunkSize
          Fuzzy.defMaxResults
          (pathSegment complInfo)
          (map T.pack directoryCompletions)
  forM
    scored
    ( \compl' -> do
        let compl = Fuzzy.original compl'
        let fullDirPath = mkPathCompletionDir complInfo compl
        pure $ mkCompletionItem (completionRange prefInfo) fullDirPath fullDirPath
    )

{- Note [Using correct file path separators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Since cabal files only allow for posix style file paths
  we need to be careful to use the correct path separators
  whenever we work with file paths in cabal files.

  Thus we are using two different kinds of imports.
  We use "FP" for platform-compatible file paths with which
  we can query files independently of the platform.
  We use "Posix" for the posix syntax paths which need to
  be used for file path completions to be written to the cabal file.
-}

-- | Takes a PathCompletionInfo and returns the list of files and directories
--  in the directory which match the path completion info in posix style.
--
--  The directories end with a posix trailing path separator.
--  Since this is used for completions to be written to the cabal file,
--  we use posix separators here.
--  See Note [Using correct file path separators].
listFileCompletions :: Recorder (WithPriority Log) -> PathCompletionInfo -> IO [FilePath]
listFileCompletions recorder complInfo = do
  let complDir = mkCompletionDirectory complInfo
  try (evaluate =<< listDirectory complDir) >>= \case
    Right dirs -> do
      forM dirs $ \d -> do
        isDir <- doesDirectoryExist $ mkDirFromCWD complInfo d
        pure $ if isDir then Posix.addTrailingPathSeparator d else d
    Left (err :: IOError) -> do
      logWith recorder Warning $ LogFilePathCompleterIOError complDir err
      pure []

-- | Returns a list of all (and only) directories in the
--  directory described by path completion info.
listDirectoryCompletions :: Recorder (WithPriority Log) -> PathCompletionInfo -> IO [FilePath]
listDirectoryCompletions recorder complInfo = do
  filepaths <- listFileCompletions recorder complInfo
  filterM (doesDirectoryExist . mkDirFromCWD complInfo) filepaths

-- | Returns the directory where files and directories can be queried from
--  for the passed PathCompletionInfo.
--
--  Returns the full path to the directory pointed to by the path prefix
--  by combining it with the working directory.
--
--  Since this is used for querying paths we use platform
--  compatible separators here.
--  See Note [Using correct file path separators].
mkCompletionDirectory :: PathCompletionInfo -> FilePath
mkCompletionDirectory complInfo =
  FP.addTrailingPathSeparator $
    workingDirectory complInfo FP.</> (FP.normalise $ queryDirectory complInfo)

-- | Returns the full path for the given path segment
--  by combining the working directory with the path prefix
--  and the path segment.
--
--  Since this is used for querying paths we use platform
--  compatible separators here.
--  See Note [Using correct file path separators].
mkDirFromCWD :: PathCompletionInfo -> FilePath -> FilePath
mkDirFromCWD complInfo fp =
  FP.addTrailingPathSeparator $
    mkCompletionDirectory complInfo FP.</> FP.normalise fp

-- | Takes a PathCompletionInfo and a directory and
--  returns the complete cabal path to be written on completion action
--  by combining the previously written path prefix and the completed
--  path segment.
--
--  Since this is used for completions we use posix separators here.
--  See Note [Using correct file path separators].
mkPathCompletionDir :: PathCompletionInfo -> T.Text -> T.Text
mkPathCompletionDir complInfo completion =
  T.pack $
    queryDirectory complInfo Posix.</> T.unpack completion

-- | Takes a PathCompletionInfo and a completed path segment and
--  generates the whole filepath to be completed.
--
--  The returned text combines the completion with a relative path
--  generated from a possible previously written path prefix and
--  is relative to the cabal file location.
--
--  If the completion results in a filepath, we know this is a
--  completed path and can thus apply wrapping of apostrophes if needed.
mkFilePathCompletion :: PathCompletionInfo -> T.Text -> IO T.Text
mkFilePathCompletion complInfo completion = do
  let combinedPath = mkPathCompletionDir complInfo completion
  isFilePath <- doesFileExist $ T.unpack combinedPath
  let completedPath = if isFilePath then applyStringNotation (isStringNotationPath complInfo) combinedPath else combinedPath
  pure completedPath
