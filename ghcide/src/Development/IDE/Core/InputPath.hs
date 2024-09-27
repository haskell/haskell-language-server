module Development.IDE.Core.InputPath where

import Development.IDE.Graph.Internal.RuleInput (Input(ProjectHaskellFile, DependencyHaskellFile))
import Language.LSP.Protocol.Types (NormalizedFilePath, fromNormalizedFilePath)
import Data.List (isInfixOf)
import System.FilePath (splitDirectories)

newtype InputPath (i :: Input) =
    InputPath { unInputPath :: NormalizedFilePath } deriving Eq

data PartitionedInputs = PartitionedInputs
  { projectFiles :: [InputPath ProjectHaskellFile]
  , dependencyFiles :: [InputPath DependencyHaskellFile]
  }

partitionInputs :: [NormalizedFilePath] -> PartitionedInputs
partitionInputs = foldr classifyInputPath emptyInputs
  where
    classifyInputPath :: NormalizedFilePath -> PartitionedInputs -> PartitionedInputs
    classifyInputPath nfp inputs@(PartitionedInputs projectInputs dependencyInputs) =
        case [".hls", "dependencies"] `isInfixOf` splitDirectories (fromNormalizedFilePath nfp) of
            True -> inputs{ projectFiles = InputPath nfp : projectInputs }
            False -> inputs{ dependencyFiles = InputPath nfp : dependencyInputs }
    emptyInputs = PartitionedInputs [] []