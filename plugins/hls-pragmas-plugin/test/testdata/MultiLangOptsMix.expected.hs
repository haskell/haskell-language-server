{-# OPTIONS_GHC -Wall 
, -Wno-unused-imports, 
 -freverse-errors #-}
{-# LANGUAGE RecordWildCards, 
   OverloadedStrings,
   BangPatterns #-}
{-# OPTIONS_GHC
    -freverse-errors
  #-}
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
