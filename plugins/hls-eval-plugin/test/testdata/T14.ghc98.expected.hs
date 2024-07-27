{-# LANGUAGE TypeApplications #-}
module T14 where

foo :: Show a => a -> String
foo = show

-- >>> :type foo @Int
-- foo @Int :: Show Int => Int -> String
