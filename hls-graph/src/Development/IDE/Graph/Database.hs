
{-# LANGUAGE ExistentialQuantification #-}
module Development.IDE.Graph.Database(
    Shake.ShakeDatabase,
    SomeShakeValue(..),
    shakeOpenDatabase,
    shakeRunDatabaseForKeys,
    Shake.shakeProfileDatabase,
    ) where

import           Data.Typeable
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import           Development.Shake                      (ShakeValue)
import           Development.Shake.Classes
import qualified Development.Shake.Database             as Shake

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO Shake.ShakeDatabase, IO ())
shakeOpenDatabase a b = Shake.shakeOpenDatabase (fromShakeOptions a) (fromRules b)

data SomeShakeValue = forall k . ShakeValue k => SomeShakeValue k
instance Eq SomeShakeValue where SomeShakeValue a == SomeShakeValue b = cast a == Just b
instance Hashable SomeShakeValue where hashWithSalt s (SomeShakeValue x) = hashWithSalt s x
instance Show SomeShakeValue where show (SomeShakeValue x) = show x

shakeRunDatabaseForKeys
    :: Maybe [SomeShakeValue]
      -- ^ Set of keys changed since last run. 'Nothing' means everything has changed
    -> Shake.ShakeDatabase
    -> [Action a]
    -> IO ([a], [IO ()])
shakeRunDatabaseForKeys _keys a b =
    -- Shake upstream does not accept the set of keys changed yet
    -- https://github.com/ndmitchell/shake/pull/802
    Shake.shakeRunDatabase a (map fromAction b)
