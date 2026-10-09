{-# LANGUAGE DataKinds, TypeOperators #-}
module T10 where
import GHC.TypeNats ( type (+) )

type Dummy = 1 + 1

-- >>> type N = 1
-- >>> type M = 40
-- >>> :kind! N + M + 1
-- N + M + 1 :: Natural
-- = 42
