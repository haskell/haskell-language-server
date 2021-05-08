
module Development.IDE.Graph.Database(
    Shake.ShakeDatabase,
    Shake.SomeShakeValue(..),
    shakeOpenDatabase,
    shakeRunDatabaseForKeys,
    Shake.shakeProfileDatabase,
    ) where

import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import qualified Development.Shake.Database             as Shake

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO Shake.ShakeDatabase, IO ())
shakeOpenDatabase a b = Shake.shakeOpenDatabase (fromShakeOptions a) (fromRules b)

shakeRunDatabaseForKeys
    :: Maybe [Shake.SomeShakeValue]       -- ^ Set of keys changed since last run
    -> Shake.ShakeDatabase
    -> [Action a]
    -> IO ([a], [IO ()])
shakeRunDatabaseForKeys keys a b = Shake.shakeRunDatabaseForKeys keys a (map fromAction b)
