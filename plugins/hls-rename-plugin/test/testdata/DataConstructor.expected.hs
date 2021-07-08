data Expr = Op Int Int

plus :: Expr -> Expr
plus (Op n m) = Op (n + m) 0
