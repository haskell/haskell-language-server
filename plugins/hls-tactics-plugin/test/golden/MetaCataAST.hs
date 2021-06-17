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

eval = [wingman| intros x, cata x; collapse |]

