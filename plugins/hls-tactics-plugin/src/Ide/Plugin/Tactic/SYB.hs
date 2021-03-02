{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Tactic.SYB where

import Control.Monad (join)
import Data.Generics


------------------------------------------------------------------------------
-- | Traverse two trees simultaneously, running the query at every zipped pair
-- of nodes from the trees.  Whenever the two data constructors agree, recurse
-- into zipping their arguments.
gzipQ
    :: forall r
     . GenericQ (GenericQ [r])
    -> GenericQ (GenericQ [r])
gzipQ f = go
  where
    go :: GenericQ (GenericQ [r])
    go x y = f x y <>
      if toConstr x == toConstr y
        then join $ gzipWithQ go x y
        else mempty


------------------------------------------------------------------------------
-- | Lift a 2-ary function into a twin query for use with 'gzipQ'
mkQQ
    :: (Monoid r, Typeable a, Typeable b)
    => (a -> b -> r)
    -> GenericQ (GenericQ r)
mkQQ f a1 a2 =
  case (cast a1, cast a2) of
    (Just x, Just y) -> f x y
    _ -> mempty

