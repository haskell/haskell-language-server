module Context where

data (Eq a) => Foo a =
    forall b c. (Show b, Show a) =>
  Bar a b | forall c. (Show c) => Baz c c
