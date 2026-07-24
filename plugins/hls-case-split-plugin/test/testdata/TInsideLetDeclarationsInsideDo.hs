{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T15 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> IO ()
foo x = do
         let r = case x of
          in r
