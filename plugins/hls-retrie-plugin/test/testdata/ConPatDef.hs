module ConPatDef where

data R = R { name :: String, n :: Int }

-- the body matches R by a field label
e :: R -> String
e r = case r of R { name = s } -> s
