{-# LANGUAGE GADTs #-}

fun2 :: (a ~ b) => a -> b
fun2 = id -- id

