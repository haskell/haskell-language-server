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
import Data.Monoid (Endo (..))
import Development.IDE.GHC.Compat
import GHC.Exts (fromString)
import GHC.SourceGen (var, op)
import GHC.SourceGen.Expr (lambda)
import Ide.Plugin.Tactic.CodeGen.Utils
import Ide.Plugin.Tactic.GHC (fromPatCompatPs)


------------------------------------------------------------------------------
-- | A pattern over the otherwise (extremely) messy AST for lambdas.
pattern Lambda :: [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
pattern Lambda pats body <-
  HsLam _
    (MG {mg_alts = L _ [L _
      (Match { m_pats = pats
             , m_grhss = GRHSs {grhssGRHSs = [L _ (
                 GRHS _ [] (L _ body))]}
             })]})
  where
    -- If there are no patterns to bind, just stick in the body
    Lambda [] body   = body
    Lambda pats body = lambda pats body


------------------------------------------------------------------------------
-- | Simlify an expression.
simplify :: LHsExpr GhcPs -> LHsExpr GhcPs
simplify
  = head
  . drop 3   -- Do three passes; this should be good enough for the limited
             -- amount of gas we give to auto
  . iterate (everywhere $ foldEndo
    [ simplifyEtaReduce
    , simplifyRemoveParens
    , simplifyCompose
    ])


------------------------------------------------------------------------------
-- | Like 'foldMap' but for endomorphisms.
foldEndo :: Foldable t => t (a -> a) -> a -> a
foldEndo = appEndo . foldMap Endo


------------------------------------------------------------------------------
-- | Does this thing contain any references to 'HsVar's with the given
-- 'RdrName'?
containsHsVar :: Data a => RdrName -> a -> Bool
containsHsVar name x = not $ null $ listify (
  \case
    ((HsVar _ (L _ a)) :: HsExpr GhcPs) | a == name -> True
    _ -> False
  ) x


------------------------------------------------------------------------------
-- | Perform an eta reduction. For example, transforms @\x -> (f g) x@ into
-- @f g@.
simplifyEtaReduce :: GenericT
simplifyEtaReduce = mkT $ \case
  Lambda
      [fromPatCompatPs -> VarPat _ (L _ pat)]
      (HsVar _ (L _ a)) | pat == a ->
    var "id"
  Lambda
      (fmap fromPatCompatPs -> unsnoc -> Just (pats, (VarPat _ (L _ pat))))
      (HsApp _ (L _ f) (L _ (HsVar _ (L _ a))))
      | pat == a
        -- We can only perform this simplifiation if @pat@ is otherwise unused.
      , not (containsHsVar pat f) ->
    Lambda pats f
  x -> x


------------------------------------------------------------------------------
-- | Perform an eta-reducing function composition. For example, transforms
-- @\x -> f (g (h x))@ into @f . g . h@.
simplifyCompose :: GenericT
simplifyCompose = mkT $ \case
  Lambda
      (fmap fromPatCompatPs -> unsnoc -> Just (pats, (VarPat _ (L _ pat))))
      (unroll -> (fs@(_:_), (HsVar _ (L _ a))))
      | pat == a
        -- We can only perform this simplifiation if @pat@ is otherwise unused.
      , not (containsHsVar pat fs) ->
    Lambda pats (foldr1 (infixCall ".") fs)
  x -> x


------------------------------------------------------------------------------
-- | Removes unnecessary parentheses on any token that doesn't need them.
simplifyRemoveParens :: GenericT
simplifyRemoveParens = mkT $ \case
  HsPar _ (L _ x) | isAtomicHsExpr x -> x
  (x :: HsExpr GhcPs) -> x


------------------------------------------------------------------------------
-- | Unrolls a right-associative function application of the form
-- @HsApp f (HsApp g (HsApp h x))@ into @([f, g, h], x)@.
unroll :: HsExpr GhcPs -> ([HsExpr GhcPs], HsExpr GhcPs)
unroll (HsPar _ (L _ x)) = unroll x
unroll (HsApp _ (L _ f) (L _ a)) =
  let (fs, r) = unroll a
   in (f : fs, r)
unroll x = ([], x)

