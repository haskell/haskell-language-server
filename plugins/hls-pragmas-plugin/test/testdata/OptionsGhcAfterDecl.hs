data Something = Something {
    foo :: !String,
    bar :: !Int
}

tupleSection = (1, ) <$> Just 2

{-# OPTIONS_GHC addOne #-}
addOne :: Int -> Int 
addOne x = x + 1
