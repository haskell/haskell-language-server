module Main where

import References
import Fields
main :: IO ()
main = return ()



a = 2 :: Int
b = a + 1

acc :: Account
acc = Savings

fooUse3 :: String -> String -> Foo
fooUse3 bar baz = MkFoo{barr = bar, bazz = baz}
