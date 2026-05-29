module A where

import {-# SOURCE #-} B (BB(BB))

data T = T Int

-- Only present in A.hs, NOT in A.hs-boot.
extraFn :: T -> T
extraFn (T n) = T (n + 1)

wrapB :: Int -> BB
wrapB = BB
