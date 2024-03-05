{-# LANGUAGE RecordWildCards #-}
module TRecordWildCards where

data Foo = Foo { foo :: Int }
foo1 = Foo 1
unpackFoo :: Foo -> Int
unpackFoo Foo{..} = foo
