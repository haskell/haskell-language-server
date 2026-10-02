module Tilia (Shape(..),area,total) where
import Data.List (foldl')
data Shape = Circle Double | Square Double deriving (Show,Eq)
area :: Shape->Double
area (Circle r)=pi*r*r
area (Square s)=s*s
total :: [Shape]->Double
total = foldl' (\acc s->acc+area s) 0
