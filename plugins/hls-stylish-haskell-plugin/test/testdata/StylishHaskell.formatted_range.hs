import           Data.Char
import qualified Data.List
import           Data.String

bar :: Maybe (Either String Integer) -> Integer
bar Nothing = 0
bar (Just (Left _)) = 0
bar (Just (Right x)) = x
