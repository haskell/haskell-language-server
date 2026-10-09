{-# LANGUAGE NamedFieldPuns #-}

module FieldPun () where

newtype Foo = Foo { field :: Int }

unFoo :: Foo -> Int
unFoo Foo{field} = field
