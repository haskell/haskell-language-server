{-# LANGUAGE RecordWildCards #-}

module Development.IDE.Graph.Internal.Options where

import           Data.Dynamic
import qualified Data.HashMap.Strict                   as Map
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Rules
import qualified Development.Shake                     as Shake

data ShakeOptions = ShakeOptions {
    shakeThreads            :: Int,
    shakeFiles              :: FilePath,
    shakeExtra              :: Maybe Dynamic,
    shakeAllowRedefineRules :: Bool,
    shakeTimings            :: Bool
    }

shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions 0 ".shake" Nothing False False

fromShakeOptions :: ShakeOptions -> Shake.ShakeOptions
fromShakeOptions ShakeOptions{..} = Shake.shakeOptions{
    Shake.shakeThreads = shakeThreads,
    Shake.shakeFiles = shakeFiles,
    Shake.shakeExtra = maybe Map.empty f shakeExtra,
    Shake.shakeAllowRedefineRules = shakeAllowRedefineRules,
    Shake.shakeTimings = shakeTimings
    }
    where f x = Map.singleton (dynTypeRep x) x


getShakeExtra :: Typeable a => Action (Maybe a)
getShakeExtra = Action Shake.getShakeExtra

getShakeExtraRules :: Typeable a => Rules (Maybe a)
getShakeExtraRules = Rules Shake.getShakeExtraRules

newShakeExtra :: Typeable a => a -> Maybe Dynamic
newShakeExtra = Just . toDyn
