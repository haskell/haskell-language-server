module Main where
import Text.ParserCombinators.Parsec

parseStr :: CharParser () String
parseStr = do
  char '"'
  str <- many1 (noneOf "\"")
  char '"'
  return str

main = putStrLn "hello"
