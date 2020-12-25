{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |Parse a Section, a group of zero or more tests defined in a multiline comment or a sequence of one line comments.
module Ide.Plugin.Eval.Parse.Section (
    allSections,
    validSections,
    Section (..),
) where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Monad.Combinators (
    many,
    optional,
    some,
    (<|>),
 )
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)
import Ide.Plugin.Eval.Parse.Parser (
    Parser,
    runParser,
    satisfy,
 )
import Ide.Plugin.Eval.Parse.Token (
    Token (BlockOpen, blockFormat, blockLanguage, blockName),
    TokenS,
    isBlockClose,
    isBlockOpen,
    isCodeLine,
    isPropLine,
    isStatement,
    isTextLine,
    unsafeContent,
 )
import Ide.Plugin.Eval.Types (
    Format (SingleLine),
    Loc,
    Located (Located, located, location),
    Section (..),
    Test (Example, Property),
    hasTests,
    unLoc,
 )

type Tk = Loc TokenS

validSections :: [Tk] -> Either String [Section]
validSections = (filter hasTests <$>) . allSections

allSections :: [Tk] -> Either String [Section]
allSections = runParser sections

{-
>>> import Ide.Plugin.Eval.Parse.Token
>>> import  System.IO.Extra(readFileUTF8')
>>> testSource_ = runParser sections . tokensFrom
>>> testSource fp = testSource_ <$> readFileUTF8' fp

>>> testSource "plugins/default/src/Ide/Plugin/Eval/Test/TestGHC.hs"
Right [Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 36, located = Property {testline = " \\(l::[Bool]) -> reverse (reverse l) == l", testOutput = []}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 40, located = Example {testLines = " :set -XScopedTypeVariables -XExplicitForAll" :| [" import qualified Test.QuickCheck as Q11"," runProp11 p = Q11.quickCheckWithResult Q11.stdArgs p >>= return . Q11.output"," prop11 = \\(l::[Int]) -> reverse (reverse l) == l"," runProp11 prop11"], testOutput = []}},Located {location = 46, located = Property {testline = " \\(l::[Int]) -> reverse (reverse l) == l", testOutput = []}}], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [Located {location = 50, located = Example {testLines = " t" :| [], testOutput = []}}], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 55, located = Example {testLines = " run $ runEval \"3+2\"" :| [], testOutput = []}}], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [Located {location = 125, located = Example {testLines = " isStmt \"\"" :| [], testOutput = ["stmt = let x =33;print x"]}}], sectionLanguage = Haddock, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = MultiLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine},Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = SingleLine}]

>>> testSource "test/testdata/eval/T11.hs"
Right [Section {sectionName = "", sectionTests = [Located {location = 2, located = Example {testLines = " :kind! a" :| [], testOutput = []}}], sectionLanguage = Plain, sectionFormat = SingleLine}]

>>> testSource "test/testdata/eval/T12.hs"
Right [Section {sectionName = "", sectionTests = [Located {location = 6, located = Example {testLines = " type N = 1" :| [" type M = 40"," :kind N + M + 1"], testOutput = []}}], sectionLanguage = Plain, sectionFormat = SingleLine}]

>>> testSource_ $ "{"++"-\n       -" ++ "}"
Right [Section {sectionName = "", sectionTests = [], sectionLanguage = Plain, sectionFormat = MultiLine}]
-}
sections :: Parser Tk [Section]
sections =
    catMaybes <$> many (const Nothing <$> some code <|> Just <$> section)

section :: Parser Tk Section
section = sectionBody >>= sectionEnd

sectionBody :: Parser Tk Section
sectionBody =
    ( \(unLoc -> BlockOpen{..}) ts ->
        Section (fromMaybe "" blockName) (catMaybes ts) blockLanguage blockFormat
    )
        <$> open <*> many (Just <$> example <|> Just <$> property <|> const Nothing <$> doc)

sectionEnd :: Section -> Parser Tk Section
sectionEnd s
    | sectionFormat s == SingleLine = optional code *> return s
    | otherwise = close *> return s

-- section = do
--   s <-
--     maybe
--       (Section "" [] Plain SingleLine)
--       ( \(Located _ BlockOpen {..}) ->
--           Section (fromMaybe "" blockName) [] blockLanguage blockFormat
--       )
--       <$> optional open
--   ts <- many (Just <$> example <|> Just <$> property <|> const Nothing <$> doc)
--   optional close
--   return $ s {sectionTests = catMaybes ts}

-- singleSection :: Parser Tk Section
-- singleSection = (\ts -> Section "" (catMaybes ts) Plain SingleLine) <$> tests

-- tests :: Parser Tk [Maybe (Loc Test)]
-- tests = some (Just <$> example <|> Just <$> property <|> const Nothing <$> doc)

doc :: Parser Tk [Tk]
doc = some text

example, property :: Parser Tk (Loc Test)
property =
    ( \(Located l p) rs ->
        Located l (Property (unsafeContent p) (unsafeContent . located <$> rs))
    )
        <$> prop
        <*> many nonEmptyText
example =
    ( \es rs ->
        Located
            (location (NE.head es))
            (Example (unsafeContent . located <$> es) (unsafeContent . located <$> rs))
    )
        <$> NE.some statement
        <*> many nonEmptyText

open, close, statement, nonEmptyText, text, prop, code :: Parser Tk Tk
statement = is isStatement
text = is isTextLine
prop = is isPropLine
open = is isBlockOpen
close = is isBlockClose
code = is isCodeLine
nonEmptyText = is (\l -> isTextLine l && not (null (unsafeContent l)))

is :: (b -> Bool) -> Parser (Loc b) (Loc b)
is p = satisfy (p . unLoc)
