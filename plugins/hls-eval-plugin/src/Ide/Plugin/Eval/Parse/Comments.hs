{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Eval.Parse.Comments where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Arrow (second, (&&&), (>>>))
import Control.Monad.Combinators ()
import qualified Data.Char as C
import Data.Coerce (coerce)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Data.Set as Set
import Data.Void (Void)
import Development.IDE.GHC.Compat
import Ide.Plugin.Eval.Types
import SrcLoc (mkRealSrcLoc, mkRealSrcSpan, realSrcSpanEnd, realSrcSpanStart)
import Text.Megaparsec
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, space, space1)

parseSections ::
    Comments -> Sections
parseSections Comments {..} = undefined

groupLineComments ::
    Map RealSrcSpan String -> [NonEmpty (RealSrcSpan, String)]
groupLineComments =
    contiguousGroupOn (fst >>> srcSpanStartLine &&& srcSpanEndLine)
        . Map.toList

type Parser inputs = Parsec Void inputs

data LineCommentSection
    = SingleProp RealSrcSpan PropLine
    | Examples RealSrcSpan (NonEmpty ExampleLine)
    | NormalCommentLines RealSrcSpan String
    deriving (Show)

data CommentFlavour = Vanilla | HaddockNext | HaddockPrev | Named String
    deriving (Read, Show, Eq, Ord)

-- >>> parse lineCommentFlavour "" "-- $a a"
-- Right (Named "a")

lineCommentFlavour :: Parser String CommentFlavour
lineCommentFlavour =
    commentHeadP
        -- N.B. Haddock assumes at most one space before modifiers:
        *> space
        *> P.option
            Vanilla
            ( HaddockNext <$ char '|'
                <|> HaddockPrev <$ char '^'
                <|> Named <$ char '$'
                    <* optional space
                    <*> P.takeWhile1P (Just "alphabet number") C.isAlphaNum
            )

commentHeadP :: Parser String ()
commentHeadP =
    space *> chunk "--"
        *> P.notFollowedBy (oneOf "!#$%&*.+=/<>?@\\~^-:|")

lineCommentSectionsP ::
    Parser [(RealSrcSpan, String)] [LineCommentSection]
lineCommentSectionsP =
    many $
        toExamples <$> exampleLinesP
            <|> uncurry SingleProp . second snd <$> propLineP
            <|> uncurry NormalCommentLines <$> anySingle

toExamples :: NonEmpty (RealSrcSpan, ExampleLine) -> LineCommentSection
toExamples lns =
    Examples (convexHullSpan $ fst <$> lns) $ snd <$> lns

convexHullSpan :: NonEmpty RealSrcSpan -> RealSrcSpan
convexHullSpan lns@(headSpan :| _) =
    let aFile = srcSpanFile headSpan
        (mbeg, mend) =
            foldMap
                ( (fmap (Just . Min) . mkRealSrcLoc aFile <$> srcSpanStartLine <*> srcSpanStartCol)
                    &&& (fmap (Just . Max) . mkRealSrcLoc aFile <$> srcSpanEndLine <*> srcSpanEndCol)
                )
                lns
        beg = maybe (realSrcSpanStart headSpan) coerce mbeg
        end = maybe (realSrcSpanEnd headSpan) coerce mend
     in mkRealSrcSpan beg end

dropLineComment ::
    String -> String
dropLineComment =
    L.dropWhile C.isSpace
        . drop 2
        . L.dropWhile C.isSpace

-- | Example line, with @>>>@ stripped off
newtype ExampleLine = ExampleLine {getExampleLine :: String}
    deriving (Show)

exampleLinesP :: Parser [(RealSrcSpan, String)] (NonEmpty (RealSrcSpan,  ExampleLine))
exampleLinesP = NE.some $ second snd <$> exampleLineP

exampleLineP :: Parser [(RealSrcSpan, String)] (RealSrcSpan, (CommentFlavour, ExampleLine))
exampleLineP = P.token (mapM $ parseMaybe exampleLineStrP) mempty

propLineP :: Parser [(RealSrcSpan, String)] (RealSrcSpan, (CommentFlavour, PropLine))
propLineP = P.token (mapM $ parseMaybe propLineStrP) mempty

exampleLineStrP :: Parser String (CommentFlavour, ExampleLine)
exampleLineStrP =
    (,) <$> lineCommentFlavour
        <*  chunk ">>>" <* P.notFollowedBy (char '>')
        <*> (ExampleLine <$> P.takeRest)

propLineStrP :: Parser String (CommentFlavour, PropLine)
propLineStrP =
    (,) <$> lineCommentFlavour
        <*  chunk "prop>" <* P.notFollowedBy (char '>')
        <*> (PropLine <$> P.takeRest)

-- | Prop line, with "prop>" stripped off
newtype PropLine = PropLine {runPropLine :: String}
    deriving (Show)

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
