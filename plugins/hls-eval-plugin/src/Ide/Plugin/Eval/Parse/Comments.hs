{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Ide.Plugin.Eval.Parse.Comments where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Arrow (first, second, (&&&), (>>>))
import Control.Lens (view, (^.))
import Control.Monad (guard, void)
import Control.Monad.Combinators ()
import qualified Data.Char as C
import Data.Coerce (coerce)
import qualified Data.DList as DL
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Semigroup
import Data.Void (Void)
import Development.IDE (Range)
import Development.IDE.Types.Location (Range (Range))
import GHC.Generics
import Ide.Plugin.Eval.Types
import Language.Haskell.LSP.Types.Lens
    ( character,
      end,
      line,
      start,
    )
import Text.Megaparsec
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, space)

parseSections ::
    Comments -> Sections
parseSections Comments {..} = undefined

groupLineComments ::
    Map Range String -> [NonEmpty (Range, String)]
groupLineComments =
    contiguousGroupOn (fst >>> view start >>> view line &&& view character)
        . Map.toList

type Parser inputs = Parsec Void inputs

-- | Prop line, with "prop>" stripped off
newtype PropLine = PropLine {getPropLine :: String}
    deriving (Show)

-- | Example line, with @>>>@ stripped off
newtype ExampleLine = ExampleLine {getExampleLine :: String}
    deriving (Show)

data LineCommentTest
    = AProp
        { lineCommentSectionSpan :: Range
        , lineProp :: PropLine
        , propResults :: [(Range, String)]
        }
    | AnExample
        { lineCommentSectionSpan :: Range
        , lineExamples :: NonEmpty ExampleLine
        , exampleResults :: [(Range, String)]
        }
    deriving (Show)

data CommentFlavour = Vanilla | HaddockNext | HaddockPrev | Named String
    deriving (Read, Show, Eq, Ord)

data CommentStyle = Line | Block
    deriving (Read, Show, Eq, Ord, Generic)

commentsToSections :: Comments -> Sections
commentsToSections Comments {..} =
    let (lineSectionSeeds, lineSetupSeeds) =
            foldMap
                ( \lcs ->
                    case parseMaybe lineGroupP $ NE.toList lcs of
                        Nothing -> mempty
                        Just (mls, rs) ->
                            (maybe DL.empty DL.singleton mls, DL.fromList rs)
                )
                $ groupLineComments lineComments
        (multilineSections, blockSetups) = ([], [])
        lineSections =
            map (uncurry linesToSection) $
                DL.toList lineSectionSeeds
        lineSetups = linesToSection (Named "setup") $ DL.toList lineSetupSeeds
        setupSections = lineSetups : blockSetups
     in Sections {..}

linesToSection ::
    CommentFlavour ->
    [LineCommentTest] ->
    Section
linesToSection flav tests =
    let sectionName
            | Named name <- flav = name
            | otherwise = ""
        sectionLanguage = case flav of
            HaddockNext -> Haddock
            HaddockPrev -> Haddock
            _ -> Plain
        sectionTests = map fromLineTest tests
        sectionFormat = SingleLine
     in Section {..}

fromLineTest :: LineCommentTest -> Loc Test
fromLineTest AProp {..} =
    Located
        (lineCommentSectionSpan ^. start . line)
        Property
            { testline = getPropLine lineProp
            , testOutput = map snd propResults
            }
fromLineTest AnExample {..} =
    Located
        (lineCommentSectionSpan ^. start . line)
        Example
            { testLines = getExampleLine <$> lineExamples
            , testOutput = map snd exampleResults
            }

-- >>> :set -XOverloadedStrings
-- >>> import Language.Haskell.LSP.Types (Position(..))
-- >>> dummyLoc = Position 0 0
-- >>> dummySpan = Range dummyLoc dummyLoc
-- >>> import Control.Arrow
-- >>> parse lineCommentSectionsP "" [(dummySpan,  "-- >>> 12"), (dummySpan, "--")]
-- Right [AnExample {lineCommentSectionSpan = Range {_start = Position {_line = 0, _character = 0}, _end = Position {_line = 0, _character = 0}}, lineExamples = ExampleLine {getExampleLine = " 12"} :| [], exampleResults = []}]

{- |
Result: a tuple of ordinary line tests and setting sections.

TODO: Haddock comment can adjacent to vanilla comment:

    @
        -- Vanilla comment
        -- Another vanilla
        -- | This parses as Haddock comment as GHC
    @

This behaviour is not yet handled correctly in Eval Plugin;
but for future extension for this, we use a tuple here instead of 'Either'.
-}
lineGroupP ::
    Parser
        [(Range, String)]
        (Maybe (CommentFlavour, [LineCommentTest]), [LineCommentTest])
lineGroupP = do
    (_, flav) <-
        lookAhead $
            token (mapM $ parseMaybe $ lineCommentFlavour <* takeRest) mempty
    case flav of
        Named "setup" -> (Nothing,) <$> lineCommentSectionsP
        flav -> (,mempty) . Just . (flav,) <$> lineCommentSectionsP

-- >>> :set -XOverloadedStrings
-- >>> dummyLoc = mkRealSrcLoc "<afile>" 0 0
-- >>> dummySpan = mkRealSrcSpan dummyLoc dummyLoc
-- >>> parseMaybe lineCommentSectionsP $ (dummySpan,) <$> ["-- | >>> unwords example", "-- \"Stale output\""]
-- Just []

-- >>> parseMaybe (lineCommentFlavour *> takeRest) "-- >>> a"
-- Just ">>> a"

-- >>> parseMaybe (lineCommentFlavour *> takeRest) "-- | >>> a"
-- Just ">>> a"

lineCommentFlavour :: Parser String CommentFlavour
lineCommentFlavour =
    lineCommentHeadP
        -- N.B. Haddock assumes at most one space before modifiers:
        *> optional (satisfy C.isSpace)
        *> P.option
            Vanilla
            ( HaddockNext <$ char '|'
                <|> HaddockPrev <$ char '^'
                <|> Named <$ char '$'
                    <* optional space
                    <*> P.takeWhile1P (Just "alphabet number") C.isAlphaNum
            )
        <* space

lineCommentHeadP :: Parser String ()
lineCommentHeadP =
    space *> chunk "--"
        *> P.notFollowedBy (oneOf "!#$%&*.+=/<>?@\\~^-:|")

{- $setup
>>> :set -XOverloadedStrings
>>> dummyLoc = mkRealSrcLoc "<afile>" 0 0
>>> dummySpan = mkRealSrcSpan dummyLoc dummyLoc
-}

lineCommentSectionsP ::
    Parser [(Range, String)] [LineCommentTest]
lineCommentSectionsP = do
    skipMany normalCommentP
    lexemeLine $
        many $
            exampleLinesP
                <|> uncurry AProp . second snd <$> propLineP <*> resultLinesP
                    <* skipMany normalCommentP

lexemeLine :: Parser [(Range, String)] a -> Parser [(Range, String)] a
lexemeLine p = p <* skipMany normalCommentP

resultLinesP :: Parser [(Range, String)] [(Range, String)]
resultLinesP = many nonEmptyCommentP

emptyLineP :: Parser [(Range, String)] ()
emptyLineP =
    void $
        satisfy $
            isJust . parseMaybe (lineCommentHeadP *> space) . snd

normalCommentP :: Parser [(Range, String)] (Range, String)
normalCommentP =
    P.token
        ( mapM $ \ln -> do
            guard $ isNothing $ parseMaybe (void (try exampleLineStrP) <|> void propLineStrP) ln
            pure $ dropWhile C.isSpace $ drop 2 ln
        )
        mempty

nonEmptyCommentP :: Parser [(Range, String)] (Range, String)
nonEmptyCommentP = try $ do
    (spn, str) <- normalCommentP
    guard $ not $ null str
    pure (spn, str)

convexHullSpan :: NonEmpty Range -> Range
convexHullSpan lns@(headSpan :| _) =
    let (mbeg, mend) =
            foldMap
                ( (Just . Min . view start)
                    &&& (Just . Max . view end)
                )
                lns
        beg = maybe (headSpan ^. start) coerce mbeg
        end_ = maybe (headSpan ^. end) coerce mend
     in Range beg end_

dropLineComment ::
    String -> String
dropLineComment =
    L.dropWhile C.isSpace
        . drop 2
        . L.dropWhile C.isSpace

exampleLinesP :: Parser [(Range, String)] LineCommentTest
exampleLinesP =
    lexemeLine $
        uncurry AnExample . first convexHullSpan . NE.unzip
            <$> NE.some (second snd <$> exampleLineP)
            <*> resultLinesP

exampleLineP :: Parser [(Range, String)] (Range, (CommentFlavour, ExampleLine))
exampleLineP = do
    P.token (mapM $ parseMaybe exampleLineStrP) mempty

propLineP :: Parser [(Range, String)] (Range, (CommentFlavour, PropLine))
propLineP = P.token (mapM $ parseMaybe propLineStrP) mempty

-- >>> either (error . errorBundlePretty) id $ parse exampleLineStrP "" "-- | >>> 12"
-- (HaddockNext,ExampleLine {getExampleLine = " 12"})

exampleLineStrP :: Parser String (CommentFlavour, ExampleLine)
exampleLineStrP =
    (,) <$> lineCommentFlavour
        <* chunk ">>>"
        <* P.notFollowedBy (char '>')
        <*> (ExampleLine <$> P.takeRest)

propLineStrP :: Parser String (CommentFlavour, PropLine)
propLineStrP =
    (,) <$> lineCommentFlavour
        <* chunk "prop>"
        <* P.notFollowedBy (char '>')
        <*> (PropLine <$> P.takeRest)

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
