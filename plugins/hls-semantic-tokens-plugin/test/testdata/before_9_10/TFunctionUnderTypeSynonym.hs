module TFunctionUnderTypeSynonym where

type T1 = Int -> Int
type T2 = forall a. a -> a
f1 :: T1
f1 x = x
f2 :: T2
f2 x = x

