{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Wingman.Metaprogramming.Lexer where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Reader (ReaderT)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void
import           Development.IDE.GHC.Compat (HscEnv, Module)
import           GhcPlugins (GlobalRdrElt)
import           Name
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L
import Wingman.Types (Context)


------------------------------------------------------------------------------
-- | Everything we need in order to call 'Wingman.Machinery.getOccNameType'.
data ParserContext = ParserContext
  { ps_hscEnv :: HscEnv
  , ps_occEnv :: OccEnv [GlobalRdrElt]
  , ps_module :: Module
  , ps_context :: Context
  }

type Parser = P.ParsecT Void Text (ReaderT ParserContext IO)



lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space P.space1 lineComment blockComment

ichar :: Parser Char
ichar = P.alphaNumChar <|> P.char '_' <|> P.char '\''

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

-- FIXME [Reed M. 2020-10-18] Check to see if the variables are in the reserved list
variable :: Parser OccName
variable = lexeme $ do
    c <- P.alphaNumChar
    cs <- P.many ichar
    pure $ mkVarOcc (c:cs)

-- FIXME [Reed M. 2020-10-18] Check to see if the variables are in the reserved list
name :: Parser Text
name = lexeme $ do
    c <- P.alphaNumChar
    cs <- P.many (ichar <|> P.char '-')
    pure $ T.pack (c:cs)

keyword :: Text -> Parser ()
keyword = identifier

