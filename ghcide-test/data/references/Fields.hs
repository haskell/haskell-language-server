{-# LANGUAGE RecordWildCards #-}
module Fields where

data Foo = MkFoo
  {
    barr :: String,
    bazz :: String
  }

fooUse0 :: Foo -> String
fooUse0 MkFoo{barr} = "5"

fooUse1 :: Foo -> String
fooUse1 MkFoo{..} = "6"

fooUse2 :: String -> String -> Foo
fooUse2 bar baz =
  MkFoo{..}
