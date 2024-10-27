{-# LANGUAGE DerivingStrategies #-}

module Development.IDE.Core.InputPath where

import Control.DeepSeq
import Data.Hashable
import Data.List (isInfixOf)
import Data.Typeable
import Development.IDE.Types.Location (emptyFilePath)
import Language.LSP.Protocol.Types (NormalizedFilePath, fromNormalizedFilePath)
import System.FilePath (splitDirectories)

data InputClass
    = ProjectHaskellFiles
    | AllHaskellFiles

newtype InputPath (i :: InputClass) =
    InputPath { unInputPath :: NormalizedFilePath }
    deriving newtype (Eq, Hashable, NFData, Typeable, Show)

class HasNormalizedFilePath input where
    getNormalizedFilePath :: input -> NormalizedFilePath

instance HasNormalizedFilePath (InputPath ProjectHaskellFiles) where
    getNormalizedFilePath (InputPath nfp) = nfp

instance HasNormalizedFilePath (InputPath AllHaskellFiles) where
    getNormalizedFilePath (InputPath nfp) = nfp

instance HasNormalizedFilePath () where
    getNormalizedFilePath _ = emptyFilePath

-- All Haskell files are valid, and we assume all
-- files are Haskell files (for now) so there is
-- no need to filter out any FilePaths.
classifyAllHaskellInputs :: [NormalizedFilePath] -> [InputPath ProjectHaskellFiles]
classifyAllHaskellInputs = map InputPath

-- Dependency files should not be considered
-- ProjectHaskellFiles, so we filter them out
-- before classifying all other files as
-- ProjectHaskellFiles.
classifyProjectHaskellInputs :: [NormalizedFilePath] -> [InputPath ProjectHaskellFiles]
classifyProjectHaskellInputs = foldr classifyInputPath []
  where
    classifyInputPath :: NormalizedFilePath -> [InputPath ProjectHaskellFiles] -> [InputPath ProjectHaskellFiles]
    classifyInputPath nfp projectInputs =
        case dependencyDirectory `isInfixOf` rawInput of
            -- The input is a dependency, so don't include
            -- it in the project inputs.
            True -> projectInputs
            -- The input is not a depencency, so include it
            -- in the project inputs
            False -> InputPath nfp : projectInputs
        where
            dependencyDirectory :: [FilePath]
            dependencyDirectory = [".hls", "dependencies"]
            rawInput :: [FilePath]
            rawInput = splitDirectories (fromNormalizedFilePath nfp)
