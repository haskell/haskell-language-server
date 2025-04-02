module Ide.Plugin.Cabal.Completion.Completer.Paths where

import qualified Data.List                         as List
import           Data.List.Extra                   (dropPrefix)
import qualified Data.Text                         as T
import           Distribution.PackageDescription   (Benchmark (..),
                                                    BuildInfo (..),
                                                    CondTree (condTreeData),
                                                    Executable (..),
                                                    ForeignLib (..),
                                                    GenericPackageDescription (..),
                                                    Library (..),
                                                    UnqualComponentName,
                                                    mkUnqualComponentName,
                                                    testBuildInfo)
import           Distribution.Utils.Path           (getSymbolicPath)
import           Ide.Plugin.Cabal.Completion.Types
import qualified System.FilePath                   as FP
import qualified System.FilePath.Posix             as Posix


{- | Information used to query and build path completions.

  Note that pathSegment  combined with queryDirectory  results in
  the original prefix.

  Example:
  When given the written prefix, @dir1\/dir2\/fi@, the
  resulting PathCompletionInfo would be:

  @
    pathSegment = "fi"
    queryDirectory  = "dir1\/dir2\/fi"
    ...
  @
-}
data PathCompletionInfo = PathCompletionInfo
  { pathSegment          :: T.Text,
    -- ^ Partly written segment of the next part of the path.
    queryDirectory       :: FilePath,
    -- ^ Written part of path, in posix format.
    workingDirectory     :: FilePath,
    -- ^ Directory relative to which relative paths are interpreted, platform dependent.
    isStringNotationPath :: Maybe Apostrophe
    -- ^ Did the completion happen in the context of a string notation,
    -- if yes, contains the state of the string notation.
  }
  deriving (Eq, Show)


{- | Posix.splitFileName modification, that drops trailing ./ if
  if wasn't present in the original path.

  Fix for the issue #3774
  Examples:

  >>> splitFileNameNoTrailingSlash ""
  ("", "")
  >>> splitFileNameNoTrailingSlash "./"
  ("./", "")
  >>> splitFileNameNoTrailingSlash "dir"
  ("", "dir")
  >>> splitFileNameNoTrailingSlash "./dir"
  ("./", "dir")
  >>> splitFileNameNoTrailingSlash "dir1/dir2"
  ("dir1/","dir2")
  >>> splitFileNameNoTrailingSlash "./dir1/dir2"
  ("./dir1/","dir2")
-}
splitFileNameNoTrailingSlash :: FilePath -> (String, String)
splitFileNameNoTrailingSlash prefix = rmTrailingSlash ("./" `List.isPrefixOf` prefix) (Posix.splitFileName prefix)
  where rmTrailingSlash hadTrailingSlash (queryDirectory', pathSegment')
                    | hadTrailingSlash = (queryDirectory', pathSegment')
                    | otherwise        = ("./" `dropPrefix` queryDirectory', pathSegment')

{- | Takes an optional source subdirectory and a prefix info
  and creates a path completion info accordingly.

  The source directory represents some subdirectory of the working directory such as a
  path from the field @hs-source-dirs@.

  If the source subdirectory is empty, then the working directory is simply set to
  the currently handled cabal file's directory.
-}
pathCompletionInfoFromCabalPrefixInfo :: FilePath -> CabalPrefixInfo -> PathCompletionInfo
pathCompletionInfoFromCabalPrefixInfo srcDir prefInfo =
  PathCompletionInfo
    { pathSegment = T.pack pathSegment',
      queryDirectory = queryDirectory',
      workingDirectory = completionWorkingDir prefInfo FP.</> srcDir,
      isStringNotationPath = isStringNotation prefInfo
    }
  where
    prefix = T.unpack $ completionPrefix prefInfo
    (queryDirectory', pathSegment') = splitFileNameNoTrailingSlash prefix

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

-- | Extracts the source directories of foreign-lib stanza with the given name.
sourceDirsExtractionForeignLib :: Maybe StanzaName -> GenericPackageDescription -> [FilePath]
sourceDirsExtractionForeignLib name gpd = extractRelativeDirsFromStanza name gpd condForeignLibs foreignLibBuildInfo

{- | Takes a possible stanza name, a GenericPackageDescription,
  a function to access the stanza information we are interested in
  and a function to access the build info from the specific stanza.

  Returns a list of relative source directory paths specified for the extracted stanza.
-}
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
