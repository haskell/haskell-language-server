module RightAssocOp where

e :: Int -> Int
e x = 2 ^ x

-- exponentiation is right-associative, i.e. 2^3^2 is 2^(3^2) so ensure the
-- expression is spliced with parenthesis.
f :: Int
f = e 3 ^ 4
