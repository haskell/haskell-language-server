module TOperator where

-- imported operator
go f x = f $ x
-- operator defined in local module
($$$$) = b
x = 1 $$$$ 2
-- class method take precedence over operator
b = 1 + 1
