import Control.Applicative (Alternative)
import qualified Data.List

main :: IO ()
main = putStrLn "hello"

foo :: Either a b -> Either a b
foo = id
