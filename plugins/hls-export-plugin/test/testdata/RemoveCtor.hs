module RemoveCtor (Foo (Foo1, Foo2), Bar (..), Baz1) where

data Foo = Foo1 | Foo2 | Foo3
data Bar = Bar1 | Bar2
data Baz = Baz1 | Baz2
