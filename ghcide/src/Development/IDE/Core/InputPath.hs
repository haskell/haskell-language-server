module Development.IDE.Core.InputPath where

import Development.IDE.Graph.Internal.RuleInput (Input)
import Development.IDE (NormalizedFilePath)

newtype InputPath (i :: Input) =
    InputPath { unInputPath :: NormalizedFilePath }