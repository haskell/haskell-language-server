module RuleMaybeNothing where

{-# RULES "maybe/nothing" forall x. maybe Nothing Just x = x #-}

f :: Maybe Int
f = maybe Nothing Just (Just 5)
