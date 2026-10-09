{-# LANGUAGE DeriveAnyClass #-}

module Ticket3942one where

class C a where
  foo :: a -> Int

newtype Foo = MkFoo Int deriving (C)
instance Show Foo where


main :: IO ()
main = return ()
