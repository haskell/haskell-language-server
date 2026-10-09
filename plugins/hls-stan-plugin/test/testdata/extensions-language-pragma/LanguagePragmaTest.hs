{-# LANGUAGE StrictData #-}

module LanguagePragmaTest () where

-- With the above `StrictData` language pragma, Stan shouldn't complain here:
data A = A Int Int

-- ...but it should still complain here!
kewlFunc = undefined
