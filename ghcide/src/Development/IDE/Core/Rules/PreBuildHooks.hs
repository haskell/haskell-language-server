-- | Handling of pre-build rules for packages with @build-type: Hooks@.
module Development.IDE.Core.Rules.PreBuildHooks
  ( PreBuildHookInfo (..)
  , emptyPreBuildHookInfo
  , parseManifestWatchFiles
  , preBuildHookDeps
  ) where

import           Control.Exception              (evaluate, throwIO)
import           Data.Char                      (isSpace)
import           Data.List                      (isPrefixOf, stripPrefix)
import           Data.Maybe                     (fromMaybe, listToMaybe,
                                                 mapMaybe)
import           Development.IDE.Core.RuleTypes (GlobPattern (..))
import           System.Directory               (doesFileExist)
import           System.FilePath                (dropTrailingPathSeparator,
                                                 isAbsolute, normalise,
                                                 takeDirectory, takeFileName,
                                                 (</>))

-- | Information extracted from the @pre-build-monitors@ manifest for a
-- @build-type: Hooks@ component.
data PreBuildHookInfo = PreBuildHookInfo
  { hookDepFiles :: [FilePath]
    -- ^ Monitored files + rule file dependencies
  , hookDepGlobs :: [GlobPattern]
    -- ^ Monitored globs
  }

emptyPreBuildHookInfo :: PreBuildHookInfo
emptyPreBuildHookInfo = PreBuildHookInfo [] []

-- | Parse the @pre-build-monitors@ manifest into a 'PreBuildHookInfo'.
parseManifestWatchFiles :: FilePath -> IO PreBuildHookInfo
parseManifestWatchFiles manifestPath = do
  content <- readFile manifestPath
  -- Force the contents of the entire manifest before proceeding, in order to
  -- avoid keeping the file handle open.
  !_ <- evaluate $ length content
  case filter (not . all isSpace) $ lines content of
    versionLine : pkgRootLine : ls
      | Just _ver  <- stripPrefix "pre-build-monitors-v" versionLine
      , Just pkgRoot <- dropWhile isSpace <$> stripPrefix "pkg-root:" pkgRootLine
      ->
        let resolve p    = normalise $ if isAbsolute p then p else pkgRoot </> p
            monitorLines = sectionLines "monitors" ls
            files        = map resolve (sectionLines "inputs" ls)
                        ++ mapMaybe (fmap resolve . parseMonitorEntry "file:") monitorLines
            globs        = map GlobPattern $
                             mapMaybe (fmap resolve . parseMonitorEntry "glob:") monitorLines
        in return $ PreBuildHookInfo { hookDepFiles = files, hookDepGlobs = globs }
    _ -> throwIO $ userError $
           "could not parse pre-build-monitors manifest at " ++ manifestPath
  where
    sectionLines section ls =
      let marker = "[" ++ section ++ "]"
      in takeWhile (\l -> not ("[" `isPrefixOf` l)) $
         drop 1 $ -- drop the marker line
         dropWhile (/= marker) ls

    parseMonitorEntry prefix l = do
      rest1 <- stripPrefix prefix l
      let (_, rest2) = break (== ':') rest1
      rest3 <- stripPrefix ":" rest2
      let (_, rest4) = break (== ':') rest3
      stripPrefix ":" rest4

-- | Parse the @pre-build-monitors@ manifest into a 'PreBuildHookInfo'
-- for a @build-type: Hooks@ component.
preBuildHookDeps :: [String] -> IO PreBuildHookInfo
preBuildHookDeps flags = do
  let mbManifest = findPreBuildRulesManifest flags
  case mbManifest of
    Nothing           -> return emptyPreBuildHookInfo
    Just manifestPath -> do
      exists <- doesFileExist manifestPath
      if not exists
      then return emptyPreBuildHookInfo
      else do
        info <- parseManifestWatchFiles manifestPath
        -- Monitor the manifest itself so that if cabal regenerates it (e.g.
        -- after the user runs `cabal build`), HLS picks up any changes to
        -- the set of inputs and monitors.
        return info { hookDepFiles = manifestPath : hookDepFiles info }

-- | Derive the @pre-build-monitors@ manifest path from the flags passed to GHC.
findPreBuildRulesManifest :: [FilePath] -> Maybe FilePath
findPreBuildRulesManifest flags =
  -- Looks for a path ending in @autogen\/cabal_macros.h@; the pre-build rules
  -- manifest file is a sibling of the @autogen@ directory.
  listToMaybe
    [ takeDirectory autogenDir </> "pre-build-monitors"
    | flag <- flags
    , let cabalMacrosPath = fromMaybe flag (stripPrefix "-optP" flag)
    , takeFileName cabalMacrosPath == "cabal_macros.h"
    , let autogenDir = dropTrailingPathSeparator (takeDirectory cabalMacrosPath)
    , takeFileName autogenDir == "autogen"
    ]
