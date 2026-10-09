module RemoveExport (foo, Bar, Baz (Baz1)) where

foo :: Int
foo = 1

data Bar = Bar
data Baz = Baz1 | Baz2
