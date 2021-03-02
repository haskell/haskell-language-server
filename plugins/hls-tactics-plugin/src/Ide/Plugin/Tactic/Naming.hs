{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Tactic.Naming where

import           Control.Monad.State.Strict
import           Data.Bool                  (bool)
import           Data.Char
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Traversable
import           Name
import           TcType
import           TyCon
import           Type
import           TysWiredIn                 (listTyCon, pairTyCon, unitTyCon, boolTy, intTy)
import Ide.Plugin.Tactic.Types (CType(CType))
import GhcPlugins (Unique, listTyConKey,Uniquable (getUnique))


------------------------------------------------------------------------------
-- | Use type information to create a reasonable name.
mkTyName :: Type -> String
-- eg. mkTyName (a -> B) = "fab"
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  = "f" ++ mkTyName a ++ mkTyName b
-- eg. mkTyName (a -> b -> C) = "f_C"
mkTyName (tcSplitFunTys -> (_:_, b))
  = "f_" ++ mkTyName b
-- eg. mkTyName (Either A B) = "eab"
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = mkTyConName c ++ foldMap mkTyName args
-- eg. mkTyName (f a) = "fa"
mkTyName (tcSplitAppTys -> (t, args@(_:_)))
  = mkTyName t ++ foldMap mkTyName args
-- eg. mkTyName a = "a"
mkTyName (getTyVar_maybe -> Just tv)
  = occNameString $ occName tv
-- eg. mkTyName (forall x. y) = "y"
mkTyName (tcSplitSigmaTy -> (_:_, _, t))
  = mkTyName t
mkTyName _ = "x"


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String
mkTyConName tc
  | tc == listTyCon = "l_"
  | tc == pairTyCon = "p_"
  | tc == unitTyCon = "unit"
  | otherwise
      = take 1
      . fmap toLower
      . filterReplace isSymbol      's'
      . filterReplace isPunctuation 'p'
      . occNameString
      $ getOccName tc


------------------------------------------------------------------------------
-- | Maybe replace an element in the list if the predicate matches
filterReplace :: (a -> Bool) -> a -> [a] -> [a]
filterReplace f r = fmap (\a -> bool a r $ f a)


------------------------------------------------------------------------------
-- | Produce a unique, good name for a type.
mkGoodName
    :: Set OccName  -- ^ Bindings in scope; used to ensure we don't shadow anything
    -> Type       -- ^ The type to produce a name for
    -> OccName
mkGoodName in_scope t =
  let tn = mkTyName t
   in mkVarOcc $ case S.member (mkVarOcc tn) in_scope of
        True  -> tn ++ show (length in_scope)
        False -> tn


------------------------------------------------------------------------------
-- | Like 'mkGoodName' but creates several apart names.
mkManyGoodNames
  :: (Traversable t, Monad m)
  => Set OccName
  -> t Type
  -> m (t OccName)
mkManyGoodNames in_scope args =
  flip evalStateT in_scope $ for args $ \at -> do
    in_scope <- get
    let n = mkGoodName in_scope at
    modify $ S.insert n
    pure n


------------------------------------------------------------------------------
-- | Which names are in scope?
getInScope :: Map OccName a -> [OccName]
getInScope = M.keys



data VarPurpose
  = PredicatePurpose
  | ContinuationPurpose
  | CountPurpose
  | FunctionPurpose
  | MonadPurpose
  | ListPurpose
  | ErrorPurpose
  deriving (Eq, Ord, Show, Enum, Bounded, Read)

data Modifier
  = Prefix String
  | Suffix String
  deriving (Eq, Ord, Show, Read)


getNames :: Set VarPurpose -> Set String
getNames ps = S.fromList $ mconcat $
  [ [ "p", "q" ]
  | S.member PredicatePurpose ps
  ] <>
  [ [ "k" ]
  | S.member ContinuationPurpose ps
  ] <>
  [ [ "n", "i", "j" ]
  | S.member CountPurpose ps
  ] <>
  [ [ "f", "g", "h" ]
  | S.member FunctionPurpose ps
  ] <>
  [ [ "m" ]
  | S.member MonadPurpose ps
  ] <>
  [ [ "e" ]
  | S.member ErrorPurpose ps
  ] <>
  [ [ "l" ]
  | S.member ListPurpose ps
  ]


regularNames :: Set String
regularNames = S.fromList
  [ "a", "b", "c", "d"
  , "o", "r", "s", "t"
  , "u", "v", "w", "x"
  , "y", "z"
  ]


getModifiers :: Set VarPurpose -> Set Modifier
getModifiers ps = S.fromList $ mconcat $
  [ [ Suffix "s" ]
  | S.member FunctionPurpose ps
  ]


getPurpose :: Type -> Type -> Set VarPurpose
getPurpose goal ty = mconcat
  [ purpose PredicatePurpose $ const hasPredicatePurpose
  , purpose CountPurpose     $ const hasCountPurpose
  , purpose FunctionPurpose  $ const hasFunctionPurpose
  , purpose ListPurpose      $ const hasListPurpose
  , purpose ContinuationPurpose hasContinuationPurpose
  ]
  where
    purpose p f = bool mempty (S.singleton p) $ f goal ty


hasPredicatePurpose :: Type -> Bool
hasPredicatePurpose (tcSplitFunTys -> ([_], ty)) = isBoolTy ty
hasPredicatePurpose _ = False


hasContinuationPurpose :: Type -> Type -> Bool
hasContinuationPurpose (tcSplitFunTys -> (_, goal)) (tcSplitFunTys -> (_:_, ty))
  | CType goal == CType ty
  , isTyVarTy ty = True
hasContinuationPurpose _ _ = False


hasCountPurpose :: Type -> Bool
hasCountPurpose ty = isIntTy ty || isIntegerTy ty


hasFunctionPurpose :: Type -> Bool
hasFunctionPurpose (tcSplitFunTys -> (_:_, _)) = True
hasFunctionPurpose _ = False


hasListPurpose :: Type -> Bool
hasListPurpose = is_tc listTyConKey


is_tc :: Unique -> Type -> Bool
-- Newtypes are opaque to this
is_tc uniq ty = case tcSplitTyConApp_maybe ty of
                        Just (tc, _) -> uniq == getUnique tc
                        Nothing      -> False


