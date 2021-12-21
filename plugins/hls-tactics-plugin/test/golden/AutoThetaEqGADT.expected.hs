{-# LANGUAGE GADTs #-}

data Y a b = a ~ b => Y

fun3 :: Y a b -> a -> b
fun3 Y = id

