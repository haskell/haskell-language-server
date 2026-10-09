{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
data Something = Something {
    foo :: !String,
    bar :: !Int
}

tupleSection = (1, ) <$> Just 2

{-# INLINE addOne #-}
addOne :: Int -> Int 
addOne x = x + 1
