module Asdf
 (f
 , where')
-- hello
-- world

 where
import Data.Int



f :: Int64 -> Int64
f = id'
  where id' = id

g :: Int -> Int
g = id

where' :: Int -> Int
where' = id
