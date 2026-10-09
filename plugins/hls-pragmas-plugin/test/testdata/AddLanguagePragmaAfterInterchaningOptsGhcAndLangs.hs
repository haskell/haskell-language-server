{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

data Something = Something {
    foo :: !String,
    bar :: !Int
}

{-# INLINE addOne #-}
addOne :: Int -> Int 
addOne x = x + 1

{-# INLINE subOne #-}
subOne :: Int -> Int
subOne x = x - 1

tupleSection = (1, ) <$> Just 2
