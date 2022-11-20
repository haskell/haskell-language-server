{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

data Foo = Foo {f1 :: Int, f2 :: Int}

g Foo{..} Foo{f1=f3,f2=f4} (Foo f5 f6) (id -> v1) = \ v4 ->
    let v3 = 3
    in
      f1 + f3 + f5 + v1 + v2 + v3 + v4
  where
    v2 = 2