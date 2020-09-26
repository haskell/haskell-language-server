module Ide.Plugin.Tactic.GHC where

import TcType
import TyCoRep
import Var
import Unique

tcTyVar_maybe :: Type -> Maybe Var
tcTyVar_maybe ty | Just ty' <- tcView ty = tcTyVar_maybe ty'
tcTyVar_maybe (CastTy ty _) = tcTyVar_maybe ty  -- look through casts, as
                                                -- this is only used for
                                                -- e.g., FlexibleContexts
tcTyVar_maybe (TyVarTy v)   = Just v
tcTyVar_maybe _             = Nothing


oneWayUnify
    :: [TyVar]  -- ^ binders
    -> Type     -- ^ type to instiate
    -> Type     -- ^ at this type
    -> Maybe TCvSubst
oneWayUnify binders toinst res =
  case tcTyVar_maybe toinst of
    Just var ->
      case elem var binders of
        True  -> pure $ mkTvSubstPrs $ pure (var, res)
        False -> Nothing
    Nothing ->
      case eqType toinst res of
        True  -> pure emptyTCvSubst
        False -> Nothing


instantiateType :: Type -> Type
instantiateType t = do
  let vs  = tyCoVarsOfTypeList t
      vs' = fmap cloneTyVar vs
      subst = foldr (\(v,t) a -> extendTCvSubst a v t) emptyTCvSubst $ zip vs vs'
   in substTy subst t


cloneTyVar :: TyVar -> Type
cloneTyVar t =
  let uniq = getUnique t
      some_magic_number = 49
   in TyVarTy $ setVarUnique t (deriveUnique uniq some_magic_number)

