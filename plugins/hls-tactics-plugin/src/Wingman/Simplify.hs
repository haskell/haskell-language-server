{-# LANGUAGE OverloadedStrings #-}

module Wingman.Simplify
  ( simplify
  ) where

import Data.Generics (GenericT, everywhere, mkT)
import Data.List.Extra (unsnoc)
import Data.Monoid (Endo (..))
import Development.IDE.GHC.Compat
import GHC.SourceGen (var)
import GHC.SourceGen.Expr (lambda)
import Wingman.CodeGen.Utils
import Wingman.GHC (containsHsVar, fromPatCompat, pattern SingleLet)


------------------------------------------------------------------------------
-- | A pattern over the otherwise (extremely) messy AST for lambdas.
pattern Lambda :: [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
pattern Lambda pats body <-
  HsLam _
    MG {mg_alts = L _ [L _
      Match { m_pats = fmap fromPatCompat -> pats
            , m_grhss = GRHSs {grhssGRHSs = [L _ (
                 GRHS _ [] (L _ body))]}
            }]
        }
  where
    -- If there are no patterns to bind, just stick in the body
    Lambda [] body   = body
    Lambda pats body = lambda pats body



------------------------------------------------------------------------------
-- | Simplify an expression.
simplify :: LHsExpr GhcPs -> LHsExpr GhcPs
simplify
  = (!!3) -- Do three passes; this should be good enough for the limited
          -- amount of gas we give to auto
  . iterate (everywhere $ foldEndo
    [ simplifyEtaReduce
    , simplifyRemoveParens
    , simplifyCompose
    , simplifySingleLet
    ])


------------------------------------------------------------------------------
-- | Like 'foldMap' but for endomorphisms.
foldEndo :: Foldable t => t (a -> a) -> a -> a
foldEndo = appEndo . foldMap Endo


------------------------------------------------------------------------------
-- | Perform an eta reduction. For example, transforms @\x -> (f g) x@ into
-- @f g@.
simplifyEtaReduce :: GenericT
simplifyEtaReduce = mkT $ \case
  Lambda
      [VarPat _ (L _ pat)]
      (HsVar _ (L _ a)) | pat == a ->
    var "id"
  Lambda
      (unsnoc -> Just (pats, VarPat _ (L _ pat)))
      (HsApp _ (L _ f) (L _ (HsVar _ (L _ a))))
      | pat == a
        -- We can only perform this simplification if @pat@ is otherwise unused.
      , not (containsHsVar pat f) ->
    Lambda pats f
  x -> x

------------------------------------------------------------------------------
-- | Eliminates the unnecessary binding in @let a = b in a@
simplifySingleLet :: GenericT
simplifySingleLet = mkT $ \case
  SingleLet bind [] val (HsVar _ (L _ a)) | a == bind -> val
  x -> x


------------------------------------------------------------------------------
-- | Perform an eta-reducing function composition. For example, transforms
-- @\x -> f (g (h x))@ into @f . g . h@.
simplifyCompose :: GenericT
simplifyCompose = mkT $ \case
  Lambda
      (unsnoc -> Just (pats, VarPat _ (L _ pat)))
      (unroll -> (fs@(_:_), HsVar _ (L _ a)))
      | pat == a
        -- We can only perform this simplification if @pat@ is otherwise unused.
      , not (containsHsVar pat fs) ->
    Lambda pats (foldr1 (infixCall ".") fs)
  x -> x


------------------------------------------------------------------------------
-- | Removes unnecessary parentheses on any token that doesn't need them.
simplifyRemoveParens :: GenericT
simplifyRemoveParens = mkT $ \case
  HsPar _ (L _ x) | isAtomicHsExpr x -> x
  (x :: HsExpr GhcPs)                -> x


------------------------------------------------------------------------------
-- | Unrolls a right-associative function application of the form
-- @HsApp f (HsApp g (HsApp h x))@ into @([f, g, h], x)@.
unroll :: HsExpr GhcPs -> ([HsExpr GhcPs], HsExpr GhcPs)
unroll (HsPar _ (L _ x)) = unroll x
unroll (HsApp _ (L _ f) (L _ a)) =
  let (fs, r) = unroll a
   in (f : fs, r)
unroll x = ([], x)

