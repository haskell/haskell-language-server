module Development.IDE.Types.Path
  ()
where

import           Development.IDE (NormalizedFilePath)

data Abs
data Rel

newtype Path a = Path { getRawPath :: NormalizedFilePath}

