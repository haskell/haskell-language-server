{-# LANGUAGE RecordWildCards #-}
data Foo = FooRenamed { a :: Int, b :: Bool }

fun :: Foo -> Int
fun FooRenamed {..} = a
