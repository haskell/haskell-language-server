{-# LANGUAGE ViewPatterns #-}

data Foo = Foo {f1 :: Int, f2 :: Int}

g Foo{f1=f1,f2=f2} (Foo f3 f4) (id -> v1) = \ v4 ->
    let v3 = 3
    in
      f1 + f3 + v1 + v2 + v3 + v4
  where
    v2 = 2