{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.LocalBindings
  ( Bindings (..)
  , bindings
  , mostSpecificSpan
  , holify
  ) where

import           Bag
import           Control.Lens
import           Control.Monad
import           Data.Data.Lens
import           Data.Function
import           Data.Generics
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat (TypecheckedModule (..), GhcTc, NoExt (..), RefMap, identType)
import           HsBinds
import           HsExpr
import           Id
import           OccName
import           SrcLoc


------------------------------------------------------------------------------
-- | WIP function for getting 'bindings' from HIE, rather than stupidly
-- traversing the entire AST.
_bindigsHIE :: RefMap -> SrcSpan -> Set Id
_bindigsHIE _ (UnhelpfulSpan _) = mempty
_bindigsHIE refmap (RealSrcSpan span) = S.fromList $ do
  (ident, refs) <- M.toList refmap
  Right _name <- pure ident
  (ref_span, ident_details) <- refs
  Just _ty <- pure $ identType ident_details
  guard $ ref_span `containsSpan` span
  mempty



------------------------------------------------------------------------------
-- | The available bindings at every point in a Haskell tree.
data Bindings = Bindings
  { bGlobalBinds :: Set Id
  , bLocalBinds  :: Map SrcSpan (Set Id)
  } deriving (Eq, Ord)

instance Semigroup Bindings where
  Bindings g1 l1 <> Bindings g2 l2 = Bindings (g1 <> g2) (l1 <> l2)

instance Monoid Bindings where
  mempty = Bindings mempty mempty


------------------------------------------------------------------------------
-- | Determine what bindings are in scope at every point in a program.
--
-- **WARNING:** This doesn't find bindings inside of TH splices or arrow syntax
-- --- and possibly other obscure pieces of the AST.
bindings :: TypecheckedModule -> Bindings
bindings = uncurry Bindings . bindsBindings mempty . tm_typechecked_source


------------------------------------------------------------------------------
-- | Helper function for implementing 'binding'.
--
-- **WARNING:** This doesn't yet work over TH splices or arrow syntax --- and
-- possibly other obscure pieces of the AST.
dataBindings :: Data a => S.Set Id -> a -> M.Map SrcSpan (S.Set Id)
dataBindings in_scope = foldMapOf biplate $ cool collect
  where
    cool
        :: (HsExpr GhcTc -> M.Map SrcSpan (S.Set Id))
        -> LHsExpr GhcTc -> M.Map SrcSpan (S.Set Id)
    cool f (L src expr) = M.union (f expr) (M.singleton src in_scope)

    collect :: HsExpr GhcTc -> M.Map SrcSpan (S.Set Id)
    collect (HsLam _ matches)     = matchGroupBindings in_scope matches
    collect (HsLamCase _ matches) = matchGroupBindings in_scope matches
    collect (HsCase _ scrutinee matches) =
      M.union (dataBindings in_scope scrutinee) $ matchGroupBindings in_scope matches
    collect (HsLet _ (L _ binds) expr) =
      let (new, res) = localBindsBindings in_scope binds
          in_scope' = S.union new in_scope
       in M.union (dataBindings in_scope' expr) res
    collect (HsVar _ _)         = mempty
    collect (HsUnboundVar _ _)  = mempty
    collect (HsConLikeOut _ _)  = mempty
    collect (HsRecFld _ _)      = mempty
    collect (HsOverLabel _ _ _) = mempty
    collect (HsIPVar _ _)       = mempty
    collect (HsOverLit _ _)     = mempty
    collect (HsLit _ _)         = mempty
    collect (HsApp _ a b)       = M.union (dataBindings in_scope a) (dataBindings in_scope b)
    collect (HsAppType _ _ a)     = dataBindings in_scope a
    collect (OpApp _ a b c) =
      mconcat
        [ dataBindings in_scope a
        , dataBindings in_scope b
        , dataBindings in_scope c
        ]
    collect (NegApp _ a _) = dataBindings in_scope a
    collect (HsPar _ a)    = dataBindings in_scope a
    collect (SectionL _ a b) =
      mconcat
       [ dataBindings in_scope a
       , dataBindings in_scope b
       ]
    collect (SectionR _ a b) =
      mconcat
        [ dataBindings in_scope a
        , dataBindings in_scope b
        ]
    collect (ExplicitTuple _ a _) = dataBindings in_scope a
    collect (ExplicitSum _ _ _ a) = dataBindings in_scope a
    collect (HsIf _ _ a b c) =
      mconcat
        [ dataBindings in_scope a
        , dataBindings in_scope b
        , dataBindings in_scope c
        ]
    collect (HsMultiIf _ a)      = dataBindings in_scope a
    collect (HsDo _ _ a)         = dataBindings in_scope a
    collect (ExplicitList _ _ a) = dataBindings in_scope a
    collect (RecordCon _ _ a)    = dataBindings in_scope a
    collect (RecordUpd _ _ a)    = dataBindings in_scope a
    collect (ExprWithTySig _ _ a)  = dataBindings in_scope a
    collect (ArithSeq _ _ a)     = dataBindings in_scope a
    collect (HsSCC _ _ _ a)      = dataBindings in_scope a
    collect (HsBracket _ a)      = dataBindings in_scope a
    collect (HsStatic _ a)       = dataBindings in_scope a
    -- TODO(sandy): This doesn't do arrow syntax
    collect _ = mempty


------------------------------------------------------------------------------
-- | Map the binds from a match group into over their containing spans.
matchGroupBindings :: S.Set Id -> MatchGroup GhcTc (LHsExpr GhcTc) -> M.Map SrcSpan (S.Set Id)
matchGroupBindings _ (XMatchGroup _) = M.empty
matchGroupBindings in_scope (MG _ (L _ alts) _) = M.fromList $ do
  L _ (Match _ _ pats body) <- alts
  let bound = S.filter isId $ everything S.union (mkQ S.empty S.singleton) pats
  M.toList $ dataBindings (S.union bound in_scope) body


------------------------------------------------------------------------------
-- | Map the binds from a local binds into over their containing spans.
localBindsBindings :: S.Set Id -> HsLocalBindsLR GhcTc GhcTc -> (S.Set Id, M.Map SrcSpan (S.Set Id))
localBindsBindings in_scope (HsValBinds _ (ValBinds _ binds _sigs)) = bindsBindings in_scope binds
localBindsBindings in_scope (HsValBinds _ (XValBindsLR (NValBinds groups _sigs))) =
  flip foldMap groups $ bindsBindings in_scope . snd
localBindsBindings _ _  = (mempty, mempty)


------------------------------------------------------------------------------
-- | Map the binds from a hsbindlr into over their containing spans.
bindsBindings :: S.Set Id -> Bag (LHsBindLR GhcTc GhcTc) -> (S.Set Id, M.Map SrcSpan (S.Set Id))
bindsBindings in_scope binds =
  flip foldMap (fmap unLoc $ bagToList binds) $ \case
    FunBind _ (L _ name) matches _ _ ->
      (S.singleton name, matchGroupBindings (S.insert name in_scope) matches)
    PatBind _ pat rhs _ ->
      let bound = S.filter isId $ everything S.union (mkQ S.empty S.singleton) pat
       in (bound, dataBindings (S.union bound in_scope) rhs)
    AbsBinds _ _ _ _ _ binds' _ ->  bindsBindings in_scope binds'
    VarBind _ name c _ ->  (S.singleton name, dataBindings in_scope c)
    PatSynBind _ _ ->  mempty
    XHsBindsLR _ -> mempty


------------------------------------------------------------------------------
-- | How many lines and columns does a SrcSpan span?
srcSpanSize :: SrcSpan -> (Int, Int)
srcSpanSize (UnhelpfulSpan _) = maxBound
srcSpanSize (RealSrcSpan span) =
  ( srcSpanEndLine span - srcSpanStartLine span
  , srcSpanEndCol span - srcSpanStartCol span
  )


------------------------------------------------------------------------------
-- | Given a SrcSpan, find the smallest LHsExpr that entirely contains that
-- span. Useful for determining what node in the tree your cursor is hovering over.
mostSpecificSpan :: (Data a, Typeable pass) => SrcSpan -> a -> Maybe (LHsExpr pass)
mostSpecificSpan span z
  = listToMaybe
  $ sortBy (comparing srcSpanSize `on` getLoc)
  $ everything (<>) (mkQ mempty $ \case
      l@(L span' _) | span `isSubspanOf` span' -> [l]
      _                                        -> [])
  $ z

------------------------------------------------------------------------------
-- | Convert an HsVar back into an HsUnboundVar if it isn't actually in scope.
-- TODO(sandy): this will throw away the type >:(
holify :: Bindings -> LHsExpr GhcTc -> LHsExpr GhcTc
holify (Bindings _ local) v@(L span (HsVar _ (L _ var))) =
  let occ = occName var
   in case M.lookup span local of
        Nothing -> v
        Just binds ->
          -- Make sure the binding is not in scope and that it begins with an
          -- underscore
          case not (S.member var binds) && take 1 (occNameString occ) == "_" of
            True  -> L span $ HsUnboundVar NoExt $ TrueExprHole occ
            False -> v
holify _ v = v

