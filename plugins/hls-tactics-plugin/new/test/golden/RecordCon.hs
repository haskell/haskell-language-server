data MyRecord a = Record
    { field1 :: a
    , field2 :: Int
    }

blah :: (a -> Int) -> a -> MyRecord a
blah = _


