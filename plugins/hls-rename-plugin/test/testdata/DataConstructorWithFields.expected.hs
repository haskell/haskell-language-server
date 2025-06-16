{-# LANGUAGE NamedFieldPuns #-}
data Foo = FooRenamed { a :: Int, b :: Bool }

foo1 :: Foo
foo1 = FooRenamed { a = 1, b = True }

foo2 :: Foo
foo2 = FooRenamed 1 True

fun1 :: Foo -> Int
fun1 FooRenamed {a} = a

fun2 :: Foo -> Int
fun2 FooRenamed {a = i} = i
