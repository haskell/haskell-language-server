module RightToLeftFixities where
import Data.List (sortOn)
import Control.Arrow ((&&&))
import Data.Ord (Down(Down))
functionB :: [String] -> [(Char,Int)]
functionB = sortOn (Down . snd) . map (head &&& length) . id
