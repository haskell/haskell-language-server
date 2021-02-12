{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Ide.Plugin.Tactic.Simplify
  ( simplify
  ) where

import Data.Data (Data)
import Data.Generics (everywhere, somewhere, something, listify, extT, mkT, GenericT, mkQ)
import Data.List.Extra (unsnoc)
import Data.Maybe (isJust)
import Development.IDE.GHC.Compat
import GHC.Exts (fromString)
import GHC.SourceGen (var, op)
import GHC.SourceGen.Expr (lambda)


pattern Lambda :: [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
pattern Lambda pats body <-
  HsLam _
    (MG {mg_alts = L _ [L _
      (Match { m_pats = pats
             , m_grhss = GRHSs {grhssGRHSs = [L _ (
                 GRHS _ [] (L _ body))]}
             })]})
  where
    Lambda [] body   = body
    Lambda pats body = lambda pats body


simplify :: LHsExpr GhcPs -> LHsExpr GhcPs
simplify = head . drop 3 . iterate (everywhere $ removeParens . compose . etaReduce)


contains :: Data a => RdrName -> a -> Bool
contains name x = not $ null $ listify (
  \case
    ((HsVar _ (L _ a)) :: HsExpr GhcPs) | a == name -> True
    _ -> False
  ) x


etaReduce :: GenericT
etaReduce = mkT $ \case
  Lambda
      [VarPat _ (L _ pat)]
      (HsVar _ (L _ a)) | pat == a ->
    var "id"
  Lambda
      (unsnoc -> Just (pats, (VarPat _ (L _ pat))))
      (HsApp _ (L _ f) (L _ (HsVar _ (L _ a))))
      | pat == a
      , not (contains pat f) ->
    Lambda pats f
  x -> x


compose :: GenericT
compose = mkT $ \case
  Lambda
      (unsnoc -> Just (pats, (VarPat _ (L _ pat))))
      (unroll -> (fs@(_:_), (HsVar _ (L _ a))))
      | pat == a
      , not (contains pat fs) ->
    Lambda pats (foldr1 (infixCall ".") fs)
  x -> x


removeParens :: GenericT
removeParens = mkT $ \case
  HsPar _ (L _ x) | isAtomicHsExpr x -> x
  (x :: HsExpr GhcPs) -> x


infixCall :: String -> HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
infixCall s = flip op (fromString s)


unroll :: HsExpr GhcPs -> ([HsExpr GhcPs], HsExpr GhcPs)
unroll (HsPar _ (L _ x)) = unroll x
unroll (HsApp _ (L _ f) (L _ a)) =
  let (fs, r) = unroll a
   in (f : fs, r)
unroll x = ([], x)

