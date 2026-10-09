module RightToLeftFixities where
import Data.List (sortOn)
import Control.Arrow ((&&&))
import Data.Ord (Down(Down))
functionB :: [String] -> [(Char,Int)]
functionB = reverse . sortOn snd . map (head &&& length) . id
