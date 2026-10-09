{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
module T where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> IO ()
foo x = do
         let r = case x of
               A -> _
               B -> _
               C _ -> _
               D _ _ -> _
               E -> _
               F -> _
          in r
