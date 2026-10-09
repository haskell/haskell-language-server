module Asdf 
 (f
 , where') 
 
 where



f :: Int64 -> Int64
f = id'
  where id' = id

g :: Int -> Int 
g = id

where' :: Int -> Int 
where' = id
