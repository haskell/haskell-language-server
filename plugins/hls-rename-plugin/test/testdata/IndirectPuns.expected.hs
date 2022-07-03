{-# LANGUAGE NamedFieldPuns #-}

module IndirectPuns () where

newtype Foo = Foo { blah :: Int }

unFoo :: Foo -> Int
unFoo Foo{blah} = blah
