{-# LANGUAGE NamedFieldPuns #-}


data Foo = Foo { a :: Bool, b :: Bool }

foo Foo {a, b} = _

