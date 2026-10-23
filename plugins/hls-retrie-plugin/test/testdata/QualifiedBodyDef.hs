module QualifiedBodyDef where

import qualified Data.Maybe as DM

e :: Maybe Int -> Int
e m = DM.fromMaybe 0 m
