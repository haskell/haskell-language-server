{-# LANGUAGE DerivingStrategies #-}

module Development.IDE.Core.InputPath
    ( InputPath
    , unInputPath
    , unsafeMkInputPath
    , toAllHaskellInput
    , toNoFileInput
    , toProjectHaskellInput
    , classifyAllHaskellInputs
    , classifyProjectHaskellInputs
    , generalizeProjectInput
    , isDependencyInputPath
    ) where

import           Control.DeepSeq
import           Data.Hashable
import           Data.List.Extra              (isInfixOf)
import           Data.Maybe                   (mapMaybe)
import           Development.IDE.Graph        (InputClass (..))
import           Development.IDE.Types.Location
import           System.FilePath              (splitDirectories)

-- | A NormalizedFilePath tagged with the class of rules it may be passed to.
--
-- The constructor is intentionally not exported. Callers must go through the
-- smart constructors/classifiers in this module, otherwise they could stamp a
-- dependency file as a ProjectHaskellFiles input and bypass the type-level
-- safety we are building.
newtype InputPath (i :: InputClass) =
    InputPath { unInputPath :: NormalizedFilePath }
    deriving newtype (Eq, Hashable, NFData, Show)

-- | Construct an InputPath without checking whether the path belongs to the
-- requested input class.
--
-- This is only for trusted internals that are rehydrating already-typed rule
-- keys from the Shake database. Normal call sites should use the smart
-- constructors below.
unsafeMkInputPath :: NormalizedFilePath -> InputPath i
unsafeMkInputPath = InputPath

-- | Any Haskell source path HLS may inspect.
--
-- This includes generated dependency source files. Rules accepting
-- AllHaskellFiles must not assume the file belongs to the project build graph.
toAllHaskellInput :: NormalizedFilePath -> InputPath AllHaskellFiles
toAllHaskellInput = InputPath

-- | The sentinel input for rules that do not operate on a real file.
toNoFileInput :: InputPath NoFile
toNoFileInput = InputPath emptyFilePath

-- | Classify a path as a project Haskell file, if it is safe to do so.
--
-- Generated dependency files are deliberately rejected here. This is the key
-- boundary that prevents dependency files from reaching project-only rules such
-- as TypeCheck, GenerateCore, GhcSessionDeps, GetModSummary, and completions.
toProjectHaskellInput :: NormalizedFilePath -> Maybe (InputPath ProjectHaskellFiles)
toProjectHaskellInput nfp
    | isDependencyInputPath nfp = Nothing
    | otherwise                 = Just (InputPath nfp)

-- | Classify many files as all-Haskell inputs.
classifyAllHaskellInputs :: [NormalizedFilePath] -> [InputPath AllHaskellFiles]
classifyAllHaskellInputs = map toAllHaskellInput

-- | Keep only paths that are safe to pass to project-only rules.
classifyProjectHaskellInputs :: [NormalizedFilePath] -> [InputPath ProjectHaskellFiles]
classifyProjectHaskellInputs = mapMaybe toProjectHaskellInput

-- | A project file can always be used where an all-Haskell file is expected.
--
-- The opposite direction is intentionally not provided. To go from
-- AllHaskellFiles to ProjectHaskellFiles, callers must classify the raw path
-- through toProjectHaskellInput.
generalizeProjectInput :: InputPath ProjectHaskellFiles -> InputPath AllHaskellFiles
generalizeProjectInput = InputPath . unInputPath

-- | Detect generated dependency source files.
--
-- Matches the layout used by the goto-dependency implementation:
-- generated dependency sources live under .hls/dependencies.

isDependencyInputPath :: NormalizedFilePath -> Bool
isDependencyInputPath nfp =
    dependencyDirectory `isInfixOf` splitDirectories (fromNormalizedFilePath nfp)
  where
    dependencyDirectory :: [FilePath]
    dependencyDirectory = [".hls", "dependencies"]
