{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE DeriveDataTypeable #-}
module Ide.Plugin.Tactic.Subst
  ( Subst
  , subst
  , mkSubst
  , singleSubst
  , flatten
  ) where

import           Data.Data (Data)
import           Data.Function (fix)
import           Data.Functor ((<&>))
import           Data.Generics
import           Data.List.NonEmpty
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Semigroup

newtype Subst a = Subst
  { unSubst :: Map a a }
  deriving (Eq, Show)

class Sub s a where
  subst' :: Subst s -> a -> a

instance Ord a => Semigroup (Subst a) where
  s1 <> (Subst s2) = Subst $ M.filterWithKey (/=) $ fmap (subst' s1) s2 <> unSubst s1

instance Ord a => Monoid (Subst a) where
  mempty = Subst mempty


mkSubst :: Ord a => [(a, a)] -> Subst a
mkSubst = mconcat . fmap (uncurry singleSubst)


singleSubst :: a -> a -> Subst a
singleSubst a b = Subst $ M.singleton a b



flatten :: Ord a => Subst a -> Subst a
flatten (Subst x) = fix $ \(Subst final) ->
  Subst $ M.fromList $ M.assocs x <&> \(a, b) -> (a,) $
    subst' (Subst final) $ maybe b id $ M.lookup b final


instance {-# OVERLAPPING #-} Ord a => Sub a a where
  subst' (Subst x) a = fromMaybe a $ M.lookup a x

instance {-# OVERLAPPABLE #-} (Typeable a, Data b, Ord a) => Sub a b where
  subst' x = everywhere $ mkT $ \case
    (b :: a) -> subst' x b

subst :: (Ord s, Sub s a) => Subst s -> a -> a
subst = subst' . flatten

