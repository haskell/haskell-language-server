{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ide.Plugin.Tactic.Subst
  ( Subst
  , subst
  , substFromList
  , substSingleton
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
  subst ::Subst s -> a -> a

instance Ord a => Semigroup (Subst a) where
  s1 <> s2 = flatten $ add s1 s2
  sconcat (a :| as) = flatten $ foldr add a as

instance Ord a => Monoid (Subst a) where
  mempty = Subst mempty
  mconcat [] = mempty
  mconcat (x : xs) = sconcat $ x :| xs


substFromList :: Ord a => [(a, a)] -> Subst a
substFromList = mconcat . fmap (uncurry substSingleton)


substSingleton :: a -> a -> Subst a
substSingleton a b = Subst $ M.singleton a b


add :: Ord a => Subst a -> Subst a -> Subst a
add s1 (Subst s2) = Subst $ fmap (subst s1) s2 <> unSubst s1


flatten :: Ord a => Subst a -> Subst a
flatten (Subst x) = fix $ \(Subst final) ->
  Subst $ M.fromList $ M.assocs x <&> \(a, b) -> (a,) $
    subst (Subst final) $ maybe b id $ M.lookup b final


instance {-# OVERLAPPING #-} Ord a => Sub a a where
  subst (Subst x) a = fromMaybe a $ M.lookup a x

instance {-# OVERLAPPABLE #-} (Typeable a, Data b, Ord a) => Sub a b where
  subst x = everywhere $ mkT $ \case
    (b :: a) -> subst x b

