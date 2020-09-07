{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.LocalBindings
  ( Bindings (..)
  , bindings
  , isItAHole
  ) where

import           Bag
import           Control.Lens
import           Data.Data.Lens
import           Data.Generics
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           GHC (TypecheckedModule (..), GhcTc)
import           HsBinds
import           HsExpr
import           Id
import           SrcLoc


data Bindings = Bindings
  { bGlobalBinds :: Set Id
  , bLocalBinds  :: Map SrcSpan (Set Id)
  } deriving (Eq, Ord)

instance Semigroup Bindings where
  Bindings g1 l1 <> Bindings g2 l2 = Bindings (g1 <> g2) (l1 <> l2)

instance Monoid Bindings where
  mempty = Bindings mempty mempty


bindings :: TypecheckedModule -> Bindings
bindings = uncurry Bindings . bindsBindings mempty . tm_typechecked_source


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


matchGroupBindings :: S.Set Id -> MatchGroup GhcTc (LHsExpr GhcTc) -> M.Map SrcSpan (S.Set Id)
matchGroupBindings _ (XMatchGroup _) = M.empty
matchGroupBindings in_scope (MG _ (L _ alts) _) = M.fromList $ do
  L _ (Match _ _ pats body) <- alts
  let bound = S.filter isId $ everything S.union (mkQ S.empty S.singleton) pats
  M.toList $ dataBindings (S.union bound in_scope) body


localBindsBindings :: S.Set Id -> HsLocalBindsLR GhcTc GhcTc -> (S.Set Id, M.Map SrcSpan (S.Set Id))
localBindsBindings in_scope (HsValBinds _ (ValBinds _ binds _sigs)) = bindsBindings in_scope binds
localBindsBindings in_scope (HsValBinds _ (XValBindsLR (NValBinds groups _sigs))) =
  flip foldMap groups $ bindsBindings in_scope . snd
localBindsBindings _ _  = (mempty, mempty)


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


isItAHole :: TypecheckedModule -> SrcSpan -> Maybe UnboundVar
isItAHole tcm span = getFirst $
  everything (<>) (
    mkQ mempty $ \case
      L span' (HsUnboundVar _ z :: HsExpr GhcTc)
        | span == span' -> pure z
      _ -> mempty
    ) $ tm_typechecked_source tcm

