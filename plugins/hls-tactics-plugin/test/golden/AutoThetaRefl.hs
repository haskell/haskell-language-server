{-# LANGUAGE GADTs #-}

data Z a b where Z :: Z a a

fun4 :: Z a b -> a -> b
fun4 Z = _ -- id

