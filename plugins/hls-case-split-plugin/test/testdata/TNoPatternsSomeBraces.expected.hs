{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall #-}
module T4 where

data X = A
       | B
       | C Int
       | D Int Int
       | E
       | F

foo :: X -> Int
foo x = case x of
        {
          A -> _;
          B -> _;
          C _ -> _;
          D _ _ -> _;
          E -> _;
          F -> _}
