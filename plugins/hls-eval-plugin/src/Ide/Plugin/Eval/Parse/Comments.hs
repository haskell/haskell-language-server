{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Eval.Parse.Comments where

import Control.Arrow ((&&&), (>>>))
import Control.Monad.Combinators
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Development.IDE.GHC.Compat
import Ide.Plugin.Eval.Types
import Text.Megaparsec (Parsec)
import Data.Void (Void)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Text.Megaparsec as P
import qualified Data.Set as Set
import qualified Control.Applicative.Combinators.NonEmpty as NE

parseSections ::
    Comments -> Sections
parseSections Comments {..} = undefined

groupLineComments ::
    Map RealSrcSpan String -> [NonEmpty (RealSrcSpan, String)]
groupLineComments =
    contiguousGroupOn (fst >>> srcSpanStartLine &&& srcSpanEndLine)
        . Map.toList

type Parser inputs = Parsec Void inputs

-- >>> readPropLine $ dropLineComment "-- prop> foo"
-- Just (PropLine {runPropLine = "foo"})

dropLineComment
    :: String -> String
dropLineComment =
    L.dropWhile C.isSpace .
    drop 2 .
    L.dropWhile C.isSpace

-- | Example line, with ">>>" stripped off
newtype ExampleLine = ExampleLine { getExampleLine :: String }
    deriving (Show)

exampleLinesP :: Parser [String] (NonEmpty ExampleLine)
exampleLinesP = NE.some exampleLineP

exampleLineP :: Parser [String] ExampleLine
exampleLineP = P.token readExampleLine mempty

propLineP :: Parser [String] PropLine
propLineP = P.token readPropLine mempty

readExampleLine
    :: String -> Maybe ExampleLine
readExampleLine ('>' : '>' : '>' : rest@(c : _))
    | c /= '>' = Just $ ExampleLine $ L.dropWhile C.isSpace rest
readExampleLine _ = Nothing

-- | Prop line, with "prop>" stripped off
newtype PropLine = PropLine { runPropLine :: String }
    deriving (Show)

readPropLine
    :: String -> Maybe PropLine
readPropLine ('p' : 'r' : 'o' : 'p' : '>' : rest@(c : _))
    | c /= '>' = Just $ PropLine $ L.dropWhile C.isSpace rest
readPropLine _ = Nothing


{- |
Given a sequence of tokens increasing in their starting position,
groups them into sublists consisting of contiguous tokens;
Two adjacent tokens are considered to be contiguous if

    * line number increases by 1, and
    * they have same starting column.

>>> contiguousGroupOn id [(1,2),(2,2),(3,4),(4,4),(5,4),(7,0),(8,0)]
NOW [(1,2) :| [(2,2)],(3,4) :| [(4,4),(5,4)],(7,0) :| [(8,0)]]
-}
contiguousGroupOn :: (a -> (Int, Int)) -> [a] -> [NonEmpty a]
contiguousGroupOn toLineCol = foldr step []
    where
        step a [] = [pure a]
        step a bss0@((b :| bs) : bss)
            | let (aLine, aCol) = toLineCol a
              , let (bLine, bCol) = toLineCol b
              , aLine + 1 == bLine && aCol == bCol =
                (a :| b : bs) : bss
            | otherwise = pure a : bss0
