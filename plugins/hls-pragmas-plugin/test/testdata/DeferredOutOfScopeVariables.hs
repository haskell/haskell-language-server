module DeferredOutOfScopeVariables where

f :: ()
f = let x = Doesn'tExist
     in undefined
