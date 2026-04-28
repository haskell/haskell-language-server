module ImportAliasShared (Maybe.fromMaybe, M.mapM) where

import qualified Control.Monad as M
import qualified Data.Maybe as Maybe

bar :: Maybe a -> Bool
bar = Maybe.isJust
