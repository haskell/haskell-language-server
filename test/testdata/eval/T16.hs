{-# LANGUAGE TypeApplications #-}
module T16 where

foo :: Show a => a -> String
foo = show

-- >>> :type  +d   40+ 2 
