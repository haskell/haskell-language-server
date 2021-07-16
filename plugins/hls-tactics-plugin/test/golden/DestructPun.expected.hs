{-# LANGUAGE NamedFieldPuns #-}


data Foo = Foo { a :: Bool, b :: Bool }

foo Foo {a = False, b} = _w0
foo Foo {a = True, b} = _w1

