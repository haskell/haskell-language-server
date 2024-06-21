module TOperator where

-- imported operator
go f x = f$x
-- operator defined in local module
($$$$) = b
x = 1 $$$$ 2
data a :+: b = Add a b
type (:-:) a b = (a, b)
-- type take precedence over operator
add :: Int :+: Int -> Int :-: Int
-- class method take precedence over operator
add (Add x y) = (x, y)
