-- patter syn
{-# LANGUAGE PatternSynonyms #-}

module Main where

-- import Data.Set (Set, insert)


data Foo = Foo { foo :: Int }

class Boo a where
  boo :: a -> a

instance Boo Int where
    boo x = x + 1

data Dd = Dd Int

pattern One = Foo 1

ggg = One

data Doo = Doo Prelude.Int
type Bar1 = Int
type Bar2 = Doo

bb :: (Boo a) => a -> a
bb x  = boo x
aa :: cool -> Int -> cool
aa x = \c -> aa x c
    where (xx, yy) = (1, 2)
          dd = 1

(zz, kk) = (1, 2)
cc :: Foo -> (Int, Int) -> Int
cc f (gg, vv)=
    case gg of
        1 -> foo $ f { foo = 1 }
        2 -> foo $ f { foo = 1 }

go = foo
add = (Prelude.+)

-- sub :: Int -> Int -> Int
-- sub x y = add x y

main :: IO ()
main = putStrLn "Hello, Haskell!"
