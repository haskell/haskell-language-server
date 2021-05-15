
module Development.IDE.Graph.Database(
    Shake.ShakeDatabase,
    shakeOpenDatabase,
    shakeRunDatabase,
    Shake.shakeProfileDatabase,
    ) where

import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import qualified Development.Shake.Database             as Shake

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO Shake.ShakeDatabase, IO ())
shakeOpenDatabase a b = Shake.shakeOpenDatabase (fromShakeOptions a) (fromRules b)

shakeRunDatabase :: Shake.ShakeDatabase -> [Action a] -> IO ([a], [IO ()])
shakeRunDatabase a b = Shake.shakeRunDatabase a (map fromAction b)
