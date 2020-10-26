{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.Tactic.GHC where

import Control.Arrow
import           Control.Monad.State
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Traversable
import           Development.IDE.GHC.Compat
import           Generics.SYB (mkT, everywhere)
import           Ide.Plugin.Tactic.Types
import           OccName
import           TcType
import           TyCoRep
import           Type
import           TysWiredIn (unitTy, intTyCon, floatTyCon, doubleTyCon, charTyCon)
import           Unique
import           Var
import TcRnMonad
import TcEvidence (TcEvBinds (..), evBindMapBinds, TcEvBinds(TcEvBinds))
import TcEnv (tcLookupTyCon)
import Id (mkVanillaGlobal)
import TcSMonad (runTcS)
import TcSimplify (solveWanteds)


tcTyVar_maybe :: Type -> Maybe Var
tcTyVar_maybe ty | Just ty' <- tcView ty = tcTyVar_maybe ty'
tcTyVar_maybe (CastTy ty _) = tcTyVar_maybe ty  -- look through casts, as
                                                -- this is only used for
                                                -- e.g., FlexibleContexts
tcTyVar_maybe (TyVarTy v)   = Just v
tcTyVar_maybe _             = Nothing


instantiateType :: Type -> ([TyVar], Type)
instantiateType t = do
  let vs  = tyCoVarsOfTypeList t
      vs' = fmap cloneTyVar vs
      subst = foldr (\(v,t) a -> extendTCvSubst a v $ TyVarTy t) emptyTCvSubst
            $ zip vs vs'
   in (vs', substTy subst t)


cloneTyVar :: TyVar -> TyVar
cloneTyVar t =
  let uniq = getUnique t
      some_magic_number = 49
   in setVarUnique t $ deriveUnique uniq some_magic_number


------------------------------------------------------------------------------
-- | Is this a function type?
isFunction :: Type -> Bool
isFunction (tacticsSplitFunTy -> (_, _, [], _)) = False
isFunction _ = True


------------------------------------------------------------------------------
-- | Split a function, also splitting out its quantified variables and theta
-- context.
tacticsSplitFunTy :: Type -> ([TyVar], ThetaType, [Type], Type)
tacticsSplitFunTy t
  = let (vars, theta, t') = tcSplitSigmaTy t
        (args, res) = tcSplitFunTys t'
     in (vars, theta, args, res)


------------------------------------------------------------------------------
-- | Rip the theta context out of a regular type.
tacticsThetaTy :: Type -> ThetaType
tacticsThetaTy (tcSplitSigmaTy -> (_, theta,  _)) = theta


------------------------------------------------------------------------------
-- | Instantiate all of the quantified type variables in a type with fresh
-- skolems.
freshTyvars :: MonadState TacticState m => Type -> m Type
freshTyvars t = do
  let (tvs, _, _, _) = tacticsSplitFunTy t
  reps <- fmap M.fromList
        $ for tvs $ \tv -> do
            uniq <- freshUnique
            pure $ (tv, setTyVarUnique tv uniq)
  pure $
    everywhere
      (mkT $ \tv ->
        case M.lookup tv reps of
          Just tv' -> tv'
          Nothing -> tv
      ) t


------------------------------------------------------------------------------
-- | Is this an algebraic type?
algebraicTyCon :: Type -> Maybe TyCon
algebraicTyCon (splitTyConApp_maybe -> Just (tycon, _))
  | tycon == intTyCon    = Nothing
  | tycon == floatTyCon  = Nothing
  | tycon == doubleTyCon = Nothing
  | tycon == charTyCon   = Nothing
  | tycon == funTyCon    = Nothing
  | otherwise = Just tycon
algebraicTyCon _ = Nothing

------------------------------------------------------------------------------
-- | Can ths type be lambda-cased?
--
-- Return: 'Nothing' if no
--         @Just False@ if it can't be homomorphic
--         @Just True@ if it can
lambdaCaseable :: Type -> Maybe Bool
lambdaCaseable (splitFunTy_maybe -> Just (arg, res))
  | isJust (algebraicTyCon arg)
  = Just $ isJust $ algebraicTyCon res
lambdaCaseable _ = Nothing

fromPatCompat :: PatCompat GhcTc -> Pat GhcTc
#if __GLASGOW_HASKELL__ == 808
type PatCompat pass = Pat pass
fromPatCompat = id
#else
type PatCompat pass = LPat pass
fromPatCompat = unLoc
#endif

------------------------------------------------------------------------------
-- | Should make sure it's a fun bind
pattern TopLevelRHS :: OccName -> [PatCompat GhcTc] -> LHsExpr GhcTc -> Match GhcTc (LHsExpr GhcTc)
pattern TopLevelRHS name ps body <-
  Match _
    (FunRhs (L _ (occName -> name)) _ _)
    ps
    (GRHSs _
      [L _ (GRHS _ [] body)] _)

getPatName :: PatCompat GhcTc -> Maybe OccName
getPatName (fromPatCompat -> p0) =
  case p0 of
    VarPat  _ x   -> Just $ occName $ unLoc x
    LazyPat _ p   -> getPatName p
    AsPat   _ x _ -> Just $ occName $ unLoc x
    ParPat  _ p   -> getPatName p
    BangPat _ p   -> getPatName p
    ViewPat _ _ p -> getPatName p
#if __GLASGOW_HASKELL__ >= 808
    SigPat  _ p _ -> getPatName p
#endif
#if __GLASGOW_HASKELL__ == 808
    XPat   p      -> getPatName $ unLoc p
#endif
    _             -> Nothing


-- Generates the evidence for `Show ()`.
generateDictionary :: Name -> [Type] -> TcM (Var, TcEvBinds)
generateDictionary cls tys = do
  traceMX "generating a dict for" $ unsafeRender (cls, tys)
  showTyCon <- tcLookupTyCon cls
  dictName <- newName $ mkDictOcc $ mkVarOcc "magic"
  let dict_ty  = mkTyConApp showTyCon tys
      dict_var = mkVanillaGlobal dictName dict_ty
  traceMX "looking for a dict: " $ CType dict_ty
  ev <- getDictionaryBindings dict_var
  return (dict_var, ev)


-- Pass in a variable `x` which has type `Show ()` (for example) to generate
-- evidence for `Show ()` which will be bound to `x`.
getDictionaryBindings :: Var -> TcM TcEvBinds
getDictionaryBindings dict_var = do
    loc <- getCtLocM (GivenOrigin UnkSkol) Nothing
    let nonC = mkNonCanonical CtWanted
            { ctev_pred = varType dict_var
            , ctev_nosh = WDeriv
            , ctev_dest = EvVarDest dict_var
            , ctev_loc = loc
            }
        wCs = mkSimpleWC [cc_ev nonC]
    traceMX "looking for a wanted: " $ unsafeRender wCs
    (_, evBinds) <- second evBindMapBinds <$> runTcS (solveWanteds wCs)
    return (EvBinds evBinds)

