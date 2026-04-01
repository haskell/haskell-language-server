module ImportAliasShared (Maybe.fromMaybe, Maybe.isJust, Maybe.maybe, M.mapM) where

import qualified Control.Monad as M
import qualified Data.Maybe as Maybe

bar :: Maybe a -> Bool
bar = Maybe.isJust

baz :: b -> (a -> b) -> Maybe a -> b
baz = Maybe.maybe

buzz :: a -> Maybe a -> a
buzz = Maybe.fromMaybe
