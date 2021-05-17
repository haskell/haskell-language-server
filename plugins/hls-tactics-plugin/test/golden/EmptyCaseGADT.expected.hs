{-# LANGUAGE GADTs #-}

data GADT a where
  MyInt :: GADT Int
  MyBool :: GADT Bool
  MyVar :: GADT a


test :: GADT Int -> GADT Bool
test x = case x of
  MyInt -> _
  MyVar -> _

