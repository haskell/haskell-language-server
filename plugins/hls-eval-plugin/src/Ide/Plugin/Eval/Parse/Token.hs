{-# OPTIONS_GHC -Wwarn #-}

-- | Parse source code into a list of line Tokens.
module Ide.Plugin.Eval.Parse.Token(Token(..),TokenS,tokensFrom,unsafeContent,isStatement,isTextLine,isPropLine,isCodeLine,isBlockOpen,isBlockClose) where

import           Control.Monad.Combinators    (skipManyTill, many, optional, (<|>))
import           Data.List                    (foldl')
import           Ide.Plugin.Eval.Parse.Parser (satisfy, Parser, alphaNumChar, char,
                                               letterChar, runParser, space,
                                               string, tillEnd)
import           Ide.Plugin.Eval.Types        (Format (..), Language (..), Loc,
                                               Located (Located))
import           Maybes                       (fromJust, fromMaybe)
import Data.Functor ( ($>) )

type TParser = Parser Char (State, [TokenS])

data State = InCode | InSingleComment | InMultiComment deriving (Eq, Show)

commentState :: Bool -> State
commentState True  = InMultiComment
commentState False = InSingleComment

type TokenS = Token String

data Token s
  = -- | Text, without prefix "(--)? >>>"
    Statement s
  | -- | Text, without prefix "(--)? prop>"
    PropLine s
  | -- | Text inside a comment
    TextLine s
  | -- | Line of code (outside comments)
    CodeLine
  | -- | Open of comment
    BlockOpen {blockName :: Maybe s, blockLanguage :: Language, blockFormat :: Format}
  | -- | Close of multi-line comment
    BlockClose
  deriving (Eq, Show)

isStatement :: Token s -> Bool
isStatement (Statement _) = True
isStatement _             = False

isTextLine :: Token s -> Bool
isTextLine (TextLine _) = True
isTextLine _            = False

isPropLine :: Token s -> Bool
isPropLine (PropLine _) = True
isPropLine _            = False

isCodeLine :: Token s -> Bool
isCodeLine CodeLine = True
isCodeLine _        = False

isBlockOpen :: Token s -> Bool
isBlockOpen (BlockOpen _ _ _) = True
isBlockOpen _                 = False

isBlockClose :: Token s -> Bool
isBlockClose BlockClose = True
isBlockClose _            = False

unsafeContent :: Token a -> a
unsafeContent = fromJust . contentOf

contentOf :: Token a -> Maybe a
contentOf (Statement c) = Just c
contentOf (PropLine c)  = Just c
contentOf (TextLine c)  = Just c
contentOf _             = Nothing

{- | Parse source code and return a list of located Tokens
>>> import           Ide.Plugin.Eval.Types        (unLoc)
>>> tks src = map unLoc . tokensFrom  <$> readFile src

>>> tks "test/testdata/eval/T1.hs"
[CodeLine,CodeLine,CodeLine,CodeLine,BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = SingleLine},Statement " unwords example",CodeLine,CodeLine]

>>> tks "test/testdata/eval/TLanguageOptions.hs"
[BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = SingleLine},TextLine "Support for language options",CodeLine,CodeLine,CodeLine,CodeLine,BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = SingleLine},TextLine "Language options set in the module source (ScopedTypeVariables)",TextLine "also apply to tests so this works fine",Statement " f = (\\(c::Char) -> [c])",CodeLine,BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine},TextLine "Multiple options can be set with a single `:set`",TextLine "",Statement " :set -XMultiParamTypeClasses -XFlexibleInstances",Statement " class Z a b c",BlockClose,CodeLine,BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine},TextLine "",TextLine "",TextLine "Options apply only in the section where they are defined (unless they are in the setup section), so this will fail:",TextLine "",Statement " class L a b c",BlockClose,CodeLine,CodeLine,BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine},TextLine "",TextLine "Options apply to all tests in the same section after their declaration.",TextLine "",TextLine "Not set yet:",TextLine "",Statement " class D",TextLine "",TextLine "Now it works:",TextLine "",Statement ":set -XMultiParamTypeClasses",Statement " class C",TextLine "",TextLine "It still works",TextLine "",Statement " class F",BlockClose,CodeLine,BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine},TextLine "Wrong option names are reported.",Statement " :set -XWrong",BlockClose]

-}
tokensFrom :: String -> [Loc (Token String)]
tokensFrom = tokens . lines

{- |
>>> tokens ["-- |$setup >>> 4+7","x=11"]
[Located {location = 0, located = BlockOpen {blockName = Just "setup", blockLanguage = Haddock, blockFormat = SingleLine}},Located {location = 0, located = Statement " 4+7"},Located {location = 1, located = CodeLine}]

>>> tokens ["-- $start"]
[Located {location = 0, located = BlockOpen {blockName = Just "start", blockLanguage = Plain, blockFormat = SingleLine}},Located {location = 0, located = TextLine ""}]

>>> tokens ["--","-- >>> 4+7"]
[Located {location = 0, located = BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = SingleLine}},Located {location = 0, located = TextLine ""},Located {location = 1, located = Statement " 4+7"}]

>>> tokens ["-- |$setup  44","-- >>> 4+7"]
[Located {location = 0, located = BlockOpen {blockName = Just "setup", blockLanguage = Haddock, blockFormat = SingleLine}},Located {location = 0, located = TextLine "44"},Located {location = 1, located = Statement " 4+7"}]

>>> tokens ["{"++"- |$doc",">>> 2+2","4","prop> x-x==0","--minus","-"++"}"]
[Located {location = 0, located = BlockOpen {blockName = Just "doc", blockLanguage = Haddock, blockFormat = MultiLine}},Located {location = 0, located = TextLine ""},Located {location = 1, located = Statement " 2+2"},Located {location = 2, located = TextLine "4"},Located {location = 3, located = PropLine " x-x==0"},Located {location = 4, located = TextLine "--minus"},Located {location = 5, located = BlockClose}]

Multi lines, closed on following line:

>>> tokens ["{"++"-","-"++"}"]
[Located {location = 0, located = BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine}},Located {location = 0, located = TextLine ""},Located {location = 1, located = BlockClose}]

>>> tokens [" {"++"-","-"++"} "]
[Located {location = 0, located = BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine}},Located {location = 0, located = TextLine ""},Located {location = 1, located = BlockClose}]

>>> tokens ["{"++"- SOME TEXT "," MORE -"++"}"]
[Located {location = 0, located = BlockOpen {blockName = Nothing, blockLanguage = Plain, blockFormat = MultiLine}},Located {location = 0, located = TextLine "SOME TEXT "},Located {location = 1, located = BlockClose}]

Multi lines, closed on the same line:

>>> tokens $ ["{--}"]
[Located {location = 0, located = CodeLine}]

>>> tokens $ ["  {- IGNORED -}  "]
[Located {location = 0, located = CodeLine}]

>>> tokens ["{-# LANGUAGE TupleSections","#-}"]
[Located {location = 0, located = CodeLine},Located {location = 1, located = CodeLine}]

>>> tokens []
[]
-}
tokens :: [String] -> [Loc TokenS]
tokens = concatMap (\(l, vs) -> map (Located l) vs) . zip [0 ..] . reverse . snd . foldl' next (InCode, [])
  where
    next (st, tokens) ln = case runParser (aline st) ln of
      Right (st', tokens') -> (st', tokens' : tokens)
      Left err -> error $ unwords ["Tokens.next failed to parse", ln, err]

-- | Parse a line of input
aline :: State -> TParser
aline InCode = optionStart <|> multi <|> singleOpen <|> codeLine
aline InSingleComment = optionStart <|> multi <|> commentLine False <|> codeLine
aline InMultiComment = multiClose <|> commentLine True

multi :: TParser
multi = multiOpenClose <|> multiOpen

codeLine :: TParser
codeLine = (InCode, [CodeLine]) <$ tillEnd

{- | A multi line comment that starts and ends on the same line.

>>> runParser multiOpenClose $ concat ["{","--","}"]
Right (InCode,[CodeLine])

>>> runParser multiOpenClose $ concat [" {","-| >>> IGNORED -","} "]
Right (InCode,[CodeLine])
-}
multiOpenClose :: TParser
multiOpenClose = (multiStart >> multiClose) $> (InCode, [CodeLine])

{-| Parses the opening of a multi line comment.
>>> runParser multiOpen $ "{"++"- $longSection this is also parsed"
Right (InMultiComment,[BlockOpen {blockName = Just "longSection", blockLanguage = Plain, blockFormat = MultiLine},TextLine "this is also parsed"])

>>> runParser multiOpen $ "{"++"- $longSection >>> 2+3"
Right (InMultiComment,[BlockOpen {blockName = Just "longSection", blockLanguage = Plain, blockFormat = MultiLine},Statement " 2+3"])
-}
multiOpen :: TParser
multiOpen =
  ( \() (maybeLanguage, maybeName) tk ->
      (InMultiComment, [BlockOpen maybeName (defLang maybeLanguage) MultiLine, tk])
  )
    <$> multiStart
    <*> languageAndName
    <*> commentRest

{- | Parse the first line of a sequence of single line comments
>>> runParser singleOpen "-- |$doc >>>11"
Right (InSingleComment,[BlockOpen {blockName = Just "doc", blockLanguage = Haddock, blockFormat = SingleLine},Statement "11"])
-}
singleOpen :: TParser
singleOpen =
  ( \() (maybeLanguage, maybeName) tk ->
      (InSingleComment, [BlockOpen maybeName (defLang maybeLanguage) SingleLine, tk])
  )
    <$> singleStart
    <*> languageAndName
    <*> commentRest

{- | Parse a line in a comment
>>> runParser (commentLine False) "x=11"
Left "No match"

>>> runParser (commentLine False) "-- >>>11"
Right (InSingleComment,[Statement "11"])

>>> runParser (commentLine True) "-- >>>11"
Right (InMultiComment,[TextLine "-- >>>11"])
-}
commentLine :: Bool -> TParser
commentLine noPrefix =
  (\tk -> (commentState noPrefix, [tk])) <$> (optLineStart noPrefix *> commentBody)

commentRest :: Parser Char (Token [Char])
commentRest = many space *> commentBody

commentBody :: Parser Char (Token [Char])
commentBody = stmt <|> prop <|> txt
  where
    txt = TextLine <$> tillEnd
    stmt = Statement <$> (string ">>>" *> tillEnd)
    prop = PropLine <$> (string "prop>" *> tillEnd)

-- | Remove comment line prefix, if needed
optLineStart :: Bool -> Parser Char ()
optLineStart noPrefix
  | noPrefix = pure ()
  | otherwise = singleStart

singleStart :: Parser Char ()
singleStart = (string "--" *> optional space) $> ()

multiStart :: Parser Char ()
multiStart = sstring "{-" $> ()

{- Parse the close of a multi-line comment
>>> runParser multiClose $ "-"++"}"
Right (InCode,[BlockClose])

>>> runParser multiClose $ "-"++"}  "
Right (InCode,[BlockClose])

As there is currently no way of handling tests in the final line of a multi line comment, it ignores anything that precedes the closing marker:

>>> runParser multiClose $ "IGNORED -"++"}  "
Right (InCode,[BlockClose])
-}
multiClose :: TParser
multiClose = skipManyTill (satisfy (const True)) (string "-}" *> many space) >> return (InCode, [BlockClose])

optionStart :: Parser Char (State, [Token s])
optionStart = (string "{-#" *> tillEnd) $> (InCode, [CodeLine])

name :: Parser Char [Char]
name = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

sstring :: String -> Parser Char [Char]
sstring s = many space *> string s *> many space

{- |
>>>runParser languageAndName "|$"
Right (Just Haddock,Just "")

>>>runParser languageAndName "|$start"
Right (Just Haddock,Just "start")

>>>runParser languageAndName "| $start"
Right (Just Haddock,Just "start")

>>>runParser languageAndName "^"
Right (Just Haddock,Nothing)

>>>runParser languageAndName "$start"
Right (Nothing,Just "start")
-}
languageAndName :: Parser Char (Maybe Language, Maybe String)
languageAndName =
  (,) <$> optional ((char '|' <|> char '^') >> pure Haddock)
    <*> optional
      (char '$' *> (fromMaybe "" <$> optional name))

defLang :: Maybe Language -> Language
defLang = fromMaybe Plain
