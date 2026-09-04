module FieldClashDef where

data R = R { name :: String, n :: Int }

-- the body uses R's selector
e :: R -> String
e r = name r
