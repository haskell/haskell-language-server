{-# LANGUAGE NamedFieldPuns #-}

module IndirectPuns () where

newtype Foo = Foo { field :: Int }

unFoo :: Foo -> Int
unFoo Foo{field} = field
