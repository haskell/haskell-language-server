{-# LANGUAGE GADTs #-}

data X f = Monad f => X

fun1 :: X f -> a -> f a
fun1 X a = pure a

