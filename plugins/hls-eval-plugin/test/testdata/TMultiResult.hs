module TMultiResult where
-- test multiline show instance (see #2907)

data Multiline = M {l1 :: String, l2 :: String} deriving Read

instance Show Multiline where
  show m = "M {\n  l1=" <> show (l1 m) <> ",\n  l2=" <> show (l2 m) <> "\n}"

-- >>> M "first line" "second line"
