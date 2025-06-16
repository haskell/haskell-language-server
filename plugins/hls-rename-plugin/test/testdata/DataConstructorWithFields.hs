{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
data Foo = Foo { a :: Int, b :: Bool }

foo1 :: Foo
foo1 = Foo { a = 1, b = True }

foo2 :: Foo
foo2 = Foo 1 True

fun1 :: Foo -> Int
fun1 Foo {a} = a

fun2 :: Foo -> Int
fun2 Foo {a = i} = i

fun3 :: Foo -> Int
fun3 Foo {..} = a
