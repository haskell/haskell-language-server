{-# LANGUAGE NamedFieldPuns #-}

module FieldPun () where

newtype Foo = Foo { bleh :: Int }

unFoo :: Foo -> Int
unFoo Foo{bleh} = bleh
