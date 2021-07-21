{-# LANGUAGE GADTs #-}

data AST a where
    BoolLit :: Bool -> AST Bool
    IntLit :: Int -> AST Int
    If :: AST Bool -> AST a -> AST a -> AST a
    Equal :: AST a -> AST a -> AST Bool

eval :: AST a -> a
eval (BoolLit b) = b
eval (IntLit n) = n
eval (If ast ast' ast_a)
  = let
      ast_c = eval ast
      ast'_c = eval ast'
      ast_a_c = eval ast_a
    in _w0 ast_c ast'_c ast_a_c
eval (Equal ast ast')
  = let
      ast_c = eval ast
      ast'_c = eval ast'
    in _w1 ast_c ast'_c

