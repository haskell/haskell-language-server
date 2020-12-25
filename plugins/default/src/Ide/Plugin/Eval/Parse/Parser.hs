{-# LANGUAGE DeriveFunctor #-}

-- |Simple List Parser, used for both line and test parsing.
module Ide.Plugin.Eval.Parse.Parser (
    Parser,
    runParser,
    satisfy,
    alphaNumChar,
    letterChar,
    space,
    string,
    char,
    tillEnd,
) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, (>=>))
import Control.Monad.Combinators (
    empty,
    (<|>),
 )
import Data.Char (
    isAlphaNum,
    isLetter,
 )
import Data.List (isPrefixOf)

type CharParser = Parser Char

{- $setup
 >>> import Control.Monad.Combinators
-}

{- |
>>> runParser  (string "aa" <|> string "bb") "bb"
Right "bb"

>>> runParser  (some (string "aa")) "aaaaaa"
Right ["aa","aa","aa"]
-}
string :: String -> CharParser String
string t = Parser $
    \s -> if t `isPrefixOf` s then Just (t, drop (length t) s) else Nothing

letterChar :: Parser Char Char
letterChar = satisfy isLetter

alphaNumChar :: Parser Char Char
alphaNumChar = satisfy isAlphaNum

space :: Parser Char Char
space = char ' '

{- |
 >>> runParser (some $ char 'a') "aa"
 Right "aa"
-}
char :: Char -> CharParser Char
char ch = satisfy (== ch)

{- |
>>> runParser tillEnd "abc\ndef"
Right "abc\ndef"
-}
tillEnd :: Parser t [t]
tillEnd = Parser $ \s -> Just (s, [])

satisfy :: (t -> Bool) -> Parser t t
satisfy f = Parser sel
  where
    sel [] = Nothing
    sel (t : ts)
        | f t = Just (t, ts)
        | otherwise = Nothing

newtype Parser t a = Parser {parse :: [t] -> Maybe (a, [t])} deriving (Functor)

instance Applicative (Parser t) where
    pure a = Parser (\s -> Just (a, s))
    (Parser p1) <*> (Parser p2) =
        Parser (p1 >=> (\(f, s1) -> p2 s1 >>= \(a, s2) -> return (f a, s2)))

instance Alternative (Parser t) where
    empty = Parser (const Nothing)
    p <|> q = Parser $ \s -> parse p s <|> parse q s

instance Monad (Parser t) where
    return = pure
    (>>=) f g = Parser (parse f >=> (\(a, s') -> parse (g a) s'))

instance MonadPlus (Parser t)

runParser :: Show t => Parser t a -> [t] -> Either String a
runParser m s = case parse m s of
    Just (res, []) -> Right res
    Just (_, ts) ->
        Left $ "Parser did not consume entire stream, left: " ++ show ts
    Nothing -> Left "No match"
