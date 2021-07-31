{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module NeedsLanguagePragma where

tupleSection = (1,) <$> Just 2

{-# INLINE addOne #-}
addOne :: Int -> Int
addOne x = x + 1
