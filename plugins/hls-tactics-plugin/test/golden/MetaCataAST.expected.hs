{-# LANGUAGE GADTs #-}

data AST a where
    BoolLit :: Bool -> AST Bool
    IntLit :: Int -> AST Int
    If :: AST Bool -> AST a -> AST a -> AST a
    Equal :: AST a -> AST a -> AST Bool

eval :: AST a -> a
-- NOTE(sandy): There is an unrelated bug that is shown off in this test
-- namely, that
--
-- @eval (IntLit n) = _@
--
-- but should be
--
-- @eval (IntLit n) = n@
--
-- https://github.com/haskell/haskell-language-server/issues/1937

eval (BoolLit b) = b
eval (IntLit n) = _
eval (If ast ast' ast_a)
  = let
      ast_c = eval ast
      ast'_c = eval ast'
      ast_a_c = eval ast_a
    in _ ast_c ast'_c ast_a_c
eval (Equal ast ast')
  = let
      ast_c = eval ast
      ast'_c = eval ast'
    in _ ast_c ast'_c

