{-# LANGUAGE GADTs #-}

data GADT b a where
  GBool :: b -> GADT b Bool
  GInt :: GADT b Int

-- wingman would prefer to use GBool since then it can use its argument. But
-- that won't unify with GADT Int, so it is forced to pick GInt and ignore the
-- argument.
test :: b -> GADT b Int
test = _

