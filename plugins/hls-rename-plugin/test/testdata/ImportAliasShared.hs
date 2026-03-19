import qualified Control.Monad as M
import qualified Data.Maybe as M

bar :: Maybe a -> Bool
bar = M.isJust
