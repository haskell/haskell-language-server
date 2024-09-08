module Development.IDE.Core.InputPath where

import Development.IDE.Graph.Internal.RuleInput (Input)
import Language.LSP.Protocol.Types (NormalizedFilePath)

newtype InputPath (i :: Input) =
    InputPath { unInputPath :: NormalizedFilePath } deriving Eq