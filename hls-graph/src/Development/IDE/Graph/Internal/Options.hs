{-# LANGUAGE RecordWildCards #-}

module Development.IDE.Graph.Internal.Options where

import qualified Development.Shake as Shake
import qualified Data.HashMap.Strict as Map
import Development.IDE.Graph.Internal.Action
import Development.IDE.Graph.Internal.Rules
import Data.Dynamic
import Data.Typeable

data ShakeOptions = ShakeOptions {
    shakeThreads :: Int,
    shakeFiles :: FilePath,
    shakeExtra :: Map.HashMap TypeRep Dynamic
    }

shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions 0 ".shake" Map.empty

fromShakeOptions :: ShakeOptions -> Shake.ShakeOptions
fromShakeOptions ShakeOptions{..} =
    Shake.shakeOptions{Shake.shakeThreads = shakeThreads, Shake.shakeFiles = shakeFiles, Shake.shakeExtra = shakeExtra}


getShakeExtra :: Typeable a => Action (Maybe a)
getShakeExtra = Action Shake.getShakeExtra

getShakeExtraRules :: Typeable a => Rules (Maybe a)
getShakeExtraRules = Rules Shake.getShakeExtraRules

addShakeExtra :: Typeable a => a -> Map.HashMap TypeRep Dynamic -> Map.HashMap TypeRep Dynamic
addShakeExtra = Shake.addShakeExtra
