module ImportAliasShared (M.fromMaybe, M.isJust, M.maybe, M.mapM) where

import qualified Control.Monad as M
import qualified Data.Maybe as M

bar :: Maybe a -> Bool
bar = M.isJust

baz :: b -> (a -> b) -> Maybe a -> b
baz = M.maybe

buzz :: a -> Maybe a -> a
buzz = M.fromMaybe
