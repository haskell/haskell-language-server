{-# LANGUAGE GADTs #-}

data AST a where
    BoolLit :: Bool -> AST Bool
    IntLit :: Int -> AST Int
    If :: AST Bool -> AST a -> AST a -> AST a
    Equal :: AST a -> AST a -> AST Bool

eval :: AST a -> a
eval = [wingman| intros x, cata x; collapse |]

