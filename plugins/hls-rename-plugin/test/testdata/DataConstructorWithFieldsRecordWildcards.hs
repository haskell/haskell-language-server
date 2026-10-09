{-# LANGUAGE RecordWildCards #-}
data Foo = Foo { a :: Int, b :: Bool }

fun :: Foo -> Int
fun Foo {..} = a
