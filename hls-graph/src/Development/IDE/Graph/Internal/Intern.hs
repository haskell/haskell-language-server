{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.IDE.Graph.Internal.Intern(
    Intern, Id,
    empty, insert, add, lookup, toList, fromList
    ) where

import qualified Data.HashMap.Strict       as Map
import           Data.List                 (foldl')
import           Development.IDE.Graph.Classes
import           Prelude                   hiding (lookup)


-- Invariant: The first field is the highest value in the Map
data Intern a = Intern {-# UNPACK #-} !Int !(Map.HashMap a Id)

type Id = Int

empty :: Intern a
empty = Intern 0 Map.empty


insert :: (Eq a, Hashable a) => a -> Id -> Intern a -> Intern a
insert k v (Intern n mp) = Intern (max n v) $ Map.insert k v mp


add :: (Eq a, Hashable a) => a -> Intern a -> (Intern a, Id)
add k (Intern v mp) = (Intern v2 $ Map.insert k v2 mp, v2)
    where v2 = v + 1


lookup :: (Eq a, Hashable a) => a -> Intern a -> Maybe Id
lookup k (Intern _ mp) = Map.lookup k mp


toList :: Intern a -> [(a, Id)]
toList (Intern _ mp) = Map.toList mp


fromList :: (Eq a, Hashable a) => [(a, Id)] -> Intern a
fromList xs = Intern (foldl' max 0 [i | (_, i) <- xs]) (Map.fromList xs)
