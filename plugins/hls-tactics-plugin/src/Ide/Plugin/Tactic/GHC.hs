{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.Tactic.GHC where

import           Control.Lens
import           Control.Monad.State
import           Data.Function (on)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Traversable
import qualified DataCon as DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts (IsString(fromString))
import           GHC.SourceGen (funBinds, match, case', lambda)
import           Generics.SYB (mkT, everywhere)
import           Ide.Plugin.Tactic.Types
import           OccName
import           TcType
import           TyCoRep
import           Type
import           TysWiredIn (intTyCon, floatTyCon, doubleTyCon, charTyCon)
import           Unique
import           Var


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
            pure (tv, setTyVarUnique tv uniq)
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


data AgdaMatch = AgdaMatch
  { amPats :: [Pat GhcPs]
  , amBody :: HsExpr GhcPs
  }
  deriving (Show)


mkFirstAgda :: [Pat GhcPs] -> HsExpr GhcPs -> AgdaMatch
mkFirstAgda pats (Lambda pats' body) = mkFirstAgda (pats <> pats') body
mkFirstAgda pats body = AgdaMatch pats body


agdaSplit :: AgdaMatch -> [AgdaMatch]
agdaSplit (AgdaMatch pats (Case (HsVar _ (L _ var)) matches)) = do
  (i, pat) <- zip [id @Int 0 ..] pats
  case pat of
    VarPat _ (L _ patname) | on (==) (occNameString . occName) patname var -> do
      (case_pat, body) <- matches
      -- TODO(sandy): use an at pattern if necessar
      pure $ AgdaMatch (pats & ix i .~ case_pat) body
    _ -> []
agdaSplit x = [x]


splitToDecl :: OccName -> [AgdaMatch] -> LHsDecl GhcPs
splitToDecl name ams = noLoc $ funBinds (fromString . occNameString . occName $ name) $ do
  AgdaMatch pats body <- ams
  pure $ match pats body


iterateSplit :: AgdaMatch -> [AgdaMatch]
iterateSplit am =
  let iterated = iterate (agdaSplit =<<) $ pure am
   in head . drop 5 $ iterated




------------------------------------------------------------------------------
-- | A pattern over the otherwise (extremely) messy AST for lambdas.
pattern Lambda :: [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
pattern Lambda pats body <-
  HsLam _
    (MG {mg_alts = L _ [L _
      (Match { m_pats = pats
             , m_grhss = UnguardedRHSs body
             })]})
  where
    -- If there are no patterns to bind, just stick in the body
    Lambda [] body   = body
    Lambda pats body = lambda pats body


pattern UnguardedRHSs :: HsExpr GhcPs -> GRHSs GhcPs (LHsExpr GhcPs)
pattern UnguardedRHSs body <-
  GRHSs {grhssGRHSs = [L _ (GRHS _ [] (L _ body))]}


pattern SinglePatMatch :: Pat GhcPs -> HsExpr GhcPs -> Match GhcPs (LHsExpr GhcPs)
pattern SinglePatMatch pat body <-
  Match { m_pats = [pat]
        , m_grhss = UnguardedRHSs body
        }


unpackMatches :: [Match GhcPs (LHsExpr GhcPs)] -> Maybe [(Pat GhcPs, HsExpr GhcPs)]
unpackMatches [] = Just []
unpackMatches (SinglePatMatch pat body : matches) =
  (:) <$> pure (pat, body) <*> unpackMatches matches
unpackMatches _ = Nothing


------------------------------------------------------------------------------
-- | A pattern over the otherwise (extremely) messy AST for lambdas.
pattern Case :: HsExpr GhcPs -> [(Pat GhcPs, HsExpr GhcPs)] -> HsExpr GhcPs
pattern Case scrutinee matches <-
  HsCase _ (L _ scrutinee)
    (MG {mg_alts = L _ (fmap unLoc -> unpackMatches -> Just matches)})
  where
    Case scrutinee matches =
      case' scrutinee $ fmap (\(pat, body) -> match [pat] body) matches


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

dataConExTys :: DataCon -> [TyCoVar]
#if __GLASGOW_HASKELL__ >= 808
dataConExTys = DataCon.dataConExTyCoVars
#else
dataConExTys = DataCon.dataConExTyVars
#endif
