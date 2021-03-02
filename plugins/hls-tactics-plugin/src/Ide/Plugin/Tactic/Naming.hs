{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns   #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Ide.Plugin.Tactic.Naming where

import           Control.Monad.State.Strict
import           Data.Bool                  (bool)
import           Data.Char
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Traversable
import           GhcPlugins (Unique, listTyConKey,Uniquable (getUnique), RdrName)
import           Ide.Plugin.Tactic.Types (CType(CType), AgdaMatch (amPats), TacticState, freshUnique)
import           Name
import           TcType
import           TyCon
import           Type
import           TysWiredIn                 (listTyCon, pairTyCon, unitTyCon, boolTy, intTy)
import Ide.Plugin.Tactic.GHC
import Development.IDE.GHC.Compat (GhcPs, Pat)
import Ide.Plugin.Tactic.SYB (gzipQ, mkQQ)
import Data.Data (Data)
import Control.Applicative (ZipList(ZipList))
import Data.List (transpose)


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
    :: MonadState TacticState m
    => Type       -- ^ The type to produce a name for
    -> m OccName
mkGoodName ty = do
  uniq <- freshUnique
  pure $ mkVarOcc $ mkTyName ty <> "_" <> show uniq


------------------------------------------------------------------------------
-- | Like 'mkGoodName' but creates several apart names.
mkManyGoodNames
  :: (Traversable t, MonadState TacticState m)
  => Set OccName
  -> t Type
  -> m (t OccName)
mkManyGoodNames _ args = for args mkGoodName


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


data PatternUnification
  = SameVar RdrName RdrName
  | Analogous RdrName RdrName
  deriving (Eq, Ord, Show)


unifyPatterns
    :: Data a
    => (RdrName -> RdrName -> r)
    -> a
    -> a
    -> [r]
unifyPatterns f = gzipQ $ mkQQ $ \a b -> pure $ f a b

data Order = MatchMajorOrder | ArgMajorOrder

newtype BoundTogether (o :: Order) = BoundTogether
  { unBoundTogether :: [[Pat GhcPs]]
  }


getBoundTogether :: [AgdaMatch] -> BoundTogether 'MatchMajorOrder
getBoundTogether ms = BoundTogether $ fmap amPats ms


toArgMajor :: BoundTogether 'MatchMajorOrder -> BoundTogether 'ArgMajorOrder
toArgMajor (BoundTogether l_l_pgp) = BoundTogether $ transpose l_l_pgp


pairwise :: Monoid m => (a -> a -> m) -> [a] -> m
pairwise f = foldMap (uncurry f) . allPairs


allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = go x xs
  where
    go _ [] = allPairs xs
    go x (y:ys) = (x, y) : go x ys


analogous :: [AgdaMatch] -> [PatternUnification]
analogous ams =
  let bt = getBoundTogether ams
   in pairwise (unifyPatterns SameVar) (unBoundTogether bt)
        <> pairwise (unifyPatterns Analogous) (unBoundTogether $ toArgMajor bt)

