{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TupleSections #-}

data Something = Something {
    foo :: !String,
    bar :: !Int
}

tupleSection = (1, ) <$> Just 2

{-# INLINE addOne #-}
addOne :: Int -> Int 
addOne x = x + 1

{-# INLINE subOne #-}
subOne :: Int -> Int
subOne x = x - 1
