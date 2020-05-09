module Highlight where
foo :: Int
foo = 3
bar = foo
  where baz = let x = foo in x
