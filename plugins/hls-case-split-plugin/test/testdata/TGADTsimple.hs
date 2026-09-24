{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_GHC -Wall -fmax-uncovered-patterns=99 #-}
{-# LANGUAGE GADTs #-}
module T where

data Expr a where
    LitInt :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Not :: Expr Bool -> Expr Bool
    If :: Expr Bool -> Expr a -> Expr a -> Expr a

prettyExpr :: Expr a -> String
prettyExpr expr = case expr of
