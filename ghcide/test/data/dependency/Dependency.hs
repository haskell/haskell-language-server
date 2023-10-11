module Dependency where

import           Control.Concurrent.Async (AsyncCancelled (..))

asyncCancelled :: AsyncCancelled
asyncCancelled = AsyncCancelled
