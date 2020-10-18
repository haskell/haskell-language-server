{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.Parser where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.Function
import           Data.List (foldl')
import           Data.Text (Text)
import           Data.Void

import qualified Control.Monad.Combinators.Expr as P
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

import           Name

import qualified Refinery.Tactic as R

import           Ide.Plugin.Tactic.Auto
import           Ide.Plugin.Tactic.Tactics
import           Ide.Plugin.Tactic.Types

type Parser = P.Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space P.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol_ :: Text -> Parser ()
symbol_ = void . symbol

brackets :: Parser a -> Parser a
brackets = P.between (symbol "[") (symbol "]")

identifier :: Text -> Parser ()
identifier i = lexeme (P.string i *> P.notFollowedBy P.alphaNumChar)

-- FIXME [Reed M. 2020-10-18] Check to see if the variables are in the reserved list
variable :: Parser OccName
variable = lexeme $ do
    c <- P.alphaNumChar
    cs <- P.many (P.alphaNumChar <|> P.char '\'')
    pure $ mkVarOcc (c:cs)

named :: Text -> TacticsM () -> Parser (TacticsM ())
named name tac = identifier name $> tac

named' :: Text -> (OccName -> TacticsM ()) -> Parser (TacticsM ())
named' name tac = tac <$> (identifier name *> variable)

keyword :: Text -> Parser ()
keyword = identifier

tactic :: Parser (TacticsM ())
tactic = flip P.makeExprParser operators $  P.choice
    [ named  "assumption" assumption
    , named' "assume" assume
    , named  "intros" intros
    , named' "intro" intro
    , named' "destruct" destruct
    , named' "homo" homo
    , named' "apply" apply
    , named  "split" split
    , named  "auto" auto
    , R.try <$> (keyword "try" *> tactics)
    ]

multitactic :: Parser (TacticsM () -> TacticsM ())
multitactic = P.choice
    [ (flip (R.<@>)) <$> brackets (P.sepBy1 tactic (symbol ","))
    , (flip (>>)) <$> tactic
    ]

operators :: [[P.Operator Parser (TacticsM ())]]
operators =
    [ [ P.Prefix (symbol "*" $> R.many_) ]
    , [ P.InfixR (symbol "|" $> (R.<%>) )]
    ]

tactics :: Parser (TacticsM ())
tactics = do
    t <- tactic
    ts <- P.many ((symbol ";") *> multitactic)
    pure $ foldl' (&) t ts
