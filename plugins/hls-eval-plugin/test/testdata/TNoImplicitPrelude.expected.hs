{-# LANGUAGE NoImplicitPrelude #-}

module TNoImplicitPrelude where

import Data.List (unwords)
import Data.String (String)

-- >>> unwords example
-- "This is an example of evaluation"
example :: [String]
example = ["This","is","an","example","of","evaluation"]
