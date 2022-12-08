{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Wingman.Metaprogramming.Lexer where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable (asum)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Development.IDE.GHC.Compat.Core (OccName, mkVarOcc)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = P.Parsec Void Text



lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space P.space1 lineComment blockComment

ichar :: Parser Char
ichar = P.alphaNumChar <|> P.char '_' <|> P.char '\''

symchar :: Parser Char
symchar = asum
  [ P.symbolChar
  , P.char '!'
  , P.char '#'
  , P.char '$'
  , P.char '%'
  , P.char '^'
  , P.char '&'
  , P.char '*'
  , P.char '-'
  , P.char '='
  , P.char '+'
  , P.char ':'
  , P.char '<'
  , P.char '>'
  , P.char ','
  , P.char '.'
  , P.char '/'
  , P.char '?'
  , P.char '~'
  , P.char '|'
  , P.char '\\'
  ]

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol_ :: Text -> Parser ()
symbol_ = void . symbol

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = P.between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = P.between (symbol "(") (symbol ")")

identifier :: Text -> Parser ()
identifier i = lexeme (P.string i *> P.notFollowedBy ichar)

variable :: Parser OccName
variable = lexeme $ do
    c <- P.alphaNumChar <|> P.char '('
    fmap mkVarOcc $ case c of
      '(' -> do
        cs <- P.many symchar
        void $ P.char ')'
        pure cs
      _ -> do
        cs <- P.many ichar
        pure $ c : cs

name :: Parser Text
name = lexeme $ do
    c <- P.alphaNumChar
    cs <- P.many (ichar <|> P.char '-')
    pure $ T.pack (c:cs)

keyword :: Text -> Parser ()
keyword = identifier

