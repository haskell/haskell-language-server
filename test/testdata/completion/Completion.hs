{-# LANGUAGE OverloadedStrings #-}
import           Data.Maybe
import qualified Data.List

main :: IO ()
main = putStrLn "hello"

foo :: Either a b -> Either a b
foo = id