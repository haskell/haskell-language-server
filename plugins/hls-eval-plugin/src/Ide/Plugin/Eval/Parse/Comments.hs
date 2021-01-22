{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Ide.Plugin.Eval.Parse.Comments where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Arrow (first, (&&&), (>>>))
import Control.Lens (view, (^.))
import Control.Monad (guard, void)
import Control.Monad.Combinators ()
import qualified Data.Char as C
import Data.Coerce (coerce)
import qualified Data.DList as DL
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import Data.Void (Void)
import Development.IDE (Range)
import Development.IDE.Types.Location (Position (..), Range (Range))
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
import Text.Megaparsec.Char
    ( alphaNumChar,
      char,
      eol,
      hspace,
      letterChar,
    )

{- |
We build parsers combining the following three kinds of them:

    *   Line parser - paring a single line into an input,
        works both for line- and block-comments.
        A line should be a proper content of lines contained in comment:
        doesn't include starting @--@ and @{\-@ and no ending @-\}@

    *   Line comment group parser: parses a contiguous group of
        tuples of range and line comment into sections of line comments.
        Each input MUST start with @--@.

    *   Block comment parser: Parsing entire block comment into sections.
        Input must be surrounded by @{\-@ and @-\}@.
-}
type Parser inputs = Parsec Void inputs

-- | Line parser
type LineParser = Parser String

-- | Line comment group parser
type LineGroupParser = Parser [(Range, RawLineComment)]

-- | Block comment parser
type BlockCommentParser = Parser String

-- | Prop line, with "prop>" stripped off
newtype PropLine = PropLine {getPropLine :: String}
    deriving (Show)

-- | Example line, with @>>>@ stripped off
newtype ExampleLine = ExampleLine {getExampleLine :: String}
    deriving (Show)

data TestComment
    = AProp
        { lineCommentSectionSpan :: Range
        , lineProp :: PropLine
        , propResults :: [String]
        }
    | AnExample
        { lineCommentSectionSpan :: Range
        , lineExamples :: NonEmpty ExampleLine
        , exampleResults :: [String]
        }
    deriving (Show)

-- | Classification of comments
data CommentFlavour = Vanilla | HaddockNext | HaddockPrev | Named String
    deriving (Read, Show, Eq, Ord)

-- | Single line or block comments?
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
                            ( maybe DL.empty DL.singleton mls
                            , -- orders setup sections in ascending order
                              if null rs
                                then mempty
                                else
                                    Map.singleton (fst $ NE.head lcs) $
                                        DL.singleton (Line, rs)
                            )
                )
                $ groupLineComments lineComments
        (blockSeed, blockSetupSeeds) =
            foldMap
                ( \(ran, lcs) ->
                    case parseMaybe (blockCommentBP $ ran ^. start) $ getRawBlockComment lcs of
                        Nothing -> mempty
                        Just (Named "setup", grp) ->
                            -- orders setup sections in ascending order
                            ( mempty
                            , Map.singleton ran $
                                DL.singleton (Block, grp)
                            )
                        Just grp ->
                            ( DL.singleton grp
                            , mempty
                            )
                )
                $ Map.toList blockComments
        lineSections =
            map (uncurry $ testsToSection Line) $
                DL.toList lineSectionSeeds
        multilineSections =
            map (uncurry $ testsToSection Block) $
                DL.toList blockSeed
        setupSections =
            map (uncurry (`testsToSection` Named "setup")) $
                DL.toList $
                F.fold $
                    Map.unionWith (<>) lineSetupSeeds blockSetupSeeds
     in Sections {..}

testsToSection ::
    CommentStyle ->
    CommentFlavour ->
    [TestComment] ->
    Section
testsToSection style flav tests =
    let sectionName
            | Named name <- flav = name
            | otherwise = ""
        sectionLanguage = case flav of
            HaddockNext -> Haddock
            HaddockPrev -> Haddock
            _ -> Plain
        sectionTests = map fromTestComment tests
        sectionFormat =
            case style of
                Line -> SingleLine
                Block -> MultiLine
     in Section {..}

fromTestComment :: TestComment -> Loc Test
fromTestComment AProp {..} =
    Located
        (lineCommentSectionSpan ^. start . line)
        Property
            { testline = getPropLine lineProp
            , testOutput = propResults
            }
fromTestComment AnExample {..} =
    Located
        (lineCommentSectionSpan ^. start . line)
        Example
            { testLines = getExampleLine <$> lineExamples
            , testOutput = exampleResults
            }

-- * Block comment parser

-- >>> parseMaybe (blockCommentBP $ Position 0 0) "{- $setup\n>>> dummyPos = Position 0 0\n>>> dummyRange = Range dummyPos dummyPos\n-}"
-- Just (Named "setup",[AnExample {lineCommentSectionSpan = Range {_start = Position {_line = 1, _character = 0}, _end = Position {_line = 3, _character = 0}}, lineExamples = ExampleLine {getExampleLine = " dummyPos = Position 0 0"} :| [ExampleLine {getExampleLine = " dummyRange = Range dummyPos dummyPos"}], exampleResults = []}])

blockCommentBP ::
    Position -> BlockCommentParser (CommentFlavour, [TestComment])
blockCommentBP pos = do
    updateParserState $ \st ->
        st
            { statePosState =
                (statePosState st)
                    { pstateSourcePos = positionToSourcePos pos
                    }
            }
    skipCount 2 anySingle -- "{-"
    void $ optional $ char ' '
    flav <- commentFlavourP
    hit <- skipNormalCommentBlock
    if hit
        then do
            body <- many $ (blockExamples <|> blockProp) <* skipNormalCommentBlock
            void takeRest -- just consume the rest
            pure (flav, body)
        else pure (Vanilla, [])

skipNormalCommentBlock :: Parser String Bool
skipNormalCommentBlock =
    skipManyTill (normalLineP Block) $
        False <$ try (optional (chunk "-}") *> eof) <|> True <$ lookAhead (propSymbol <|> exampleSymbol)

eob :: BlockCommentParser ()
eob = eof <|> try (optional (chunk "-}") *> eof) <|> void eol

blockExamples, blockProp :: BlockCommentParser TestComment
blockExamples = do
    (ran, examples) <- withRange $ NE.some $ exampleLineStrP Block
    AnExample ran examples <$> resultBlockP
blockProp = do
    (ran, prop) <- withRange $ propLineStrP Block
    AProp ran prop <$> resultBlockP

withRange :: (TraversableStream s, Stream s) => Parser s a -> Parser s (Range, a)
withRange p = do
    beg <- sourcePosToPosition <$> getSourcePos
    a <- p
    fin <- sourcePosToPosition <$> getSourcePos
    pure (Range beg fin, a)

resultBlockP :: BlockCommentParser [String]
resultBlockP = many $ nonEmptyNormalLineP Block

positionToSourcePos :: Position -> SourcePos
positionToSourcePos pos =
    P.SourcePos
        { sourceName = "<block comment>"
        , sourceLine = P.mkPos $ 1 + pos ^. line
        , sourceColumn = P.mkPos $ 1 + pos ^. character
        }

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition SourcePos {..} =
    Position (unPos sourceLine - 1) (unPos sourceColumn - 1)

-- * Line Group Parser

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
    LineGroupParser
        (Maybe (CommentFlavour, [TestComment]), [TestComment])
lineGroupP = do
    (_, flav) <- lookAhead $ parseLine (commentFlavourP <* takeRest)
    case flav of
        Named "setup" -> (Nothing,) <$> lineCommentSectionsP
        flav -> (,mempty) . Just . (flav,) <$> lineCommentSectionsP

{- $setup
>>> dummyPos = Position 0 0
>>> dummyRange = Range dummyPos dummyPos
-}

-- >>>  parse (lineGroupP <*eof) "" $ (dummyRange, ) . RawLineComment <$> ["-- a", "-- b"]
-- Variable not in scope: dummyRange :: Range

commentFlavourP :: LineParser CommentFlavour
commentFlavourP =
    P.option
        Vanilla
        ( HaddockNext <$ char '|'
            <|> HaddockPrev <$ char '^'
            <|> Named <$ char '$'
                <* optional hspace
                <*> ((:) <$> letterChar <*> P.many alphaNumChar)
        )
        <* optional (char ' ')

lineCommentHeadP :: LineParser ()
lineCommentHeadP = do
    -- and no operator symbol character follows.
    void $ chunk "--"
    skipMany $ char '-'
    void $ optional $ char ' '

lineCommentSectionsP ::
    LineGroupParser [TestComment]
lineCommentSectionsP = do
    skipMany normalLineCommentP
    many $
        exampleLinesGP
            <|> uncurry AProp <$> propLineGP <*> resultLinesP
                <* skipMany normalLineCommentP

lexemeLine :: LineGroupParser a -> LineGroupParser a
lexemeLine p = p <* skipMany normalLineCommentP

resultLinesP :: LineGroupParser [String]
resultLinesP = many nonEmptyLGP

normalLineCommentP :: LineGroupParser (Range, String)
normalLineCommentP =
    parseLine (commentFlavourP *> normalLineP Line)

nonEmptyLGP :: LineGroupParser String
nonEmptyLGP = try $ fmap snd $ parseLine $ commentFlavourP *> nonEmptyNormalLineP Line

exampleLinesGP :: LineGroupParser TestComment
exampleLinesGP =
    lexemeLine $
        uncurry AnExample . first convexHullSpan . NE.unzip
            <$> NE.some exampleLineGP
            <*> resultLinesP

exampleLineGP :: LineGroupParser (Range, ExampleLine)
exampleLineGP = parseLine (commentFlavourP *> exampleLineStrP Line)

propLineGP :: LineGroupParser (Range, PropLine)
propLineGP = parseLine (commentFlavourP *> propLineStrP Line)

{- |
Turning a line parser into line group parser consuming a single line comment.
Parses a sinlge line comment, skipping prefix "--[-*]" with optional one horizontal space.
fails if the input does not start with "--".

__N.B.__ We don't strip comment flavours.

>>> parseMaybe (parseLine $ takeRest) $ map (:[]) ["-- >>> A"]
Just [">>> A"]

>>> parseMaybe (parseLine $ takeRest) $ map (:[]) ["---  >>> A"]
Just [" >>> A"]

>>> parseMaybe (parseLine takeRest) $ map (:[]) [""]
Nothing
-}
parseLine ::
    (Ord (f RawLineComment), Traversable f) =>
    LineParser a ->
    Parser [f RawLineComment] (f a)
parseLine p =
    P.token
        (mapM $ parseMaybe (lineCommentHeadP *> p) . getRawLineComment)
        mempty

-- * Line Parsers

-- | Non-empty normal line.
nonEmptyNormalLineP :: CommentStyle -> LineParser String
nonEmptyNormalLineP style = try $ do
    ln <- normalLineP style
    guard $ not $ all C.isSpace ln
    pure ln

{- | Normal line is a line neither a example nor prop.
 Empty line is normal.
-}
normalLineP :: CommentStyle -> LineParser String
normalLineP style = do
    notFollowedBy (try $ exampleSymbol <|> propSymbol)
    consume style

-- >>> parse (skipMany (consume Block)) "" "foo\nbar"
-- Right ()
consume :: CommentStyle -> Parser String String
consume style =
    case style of
        Line -> takeRest
        Block -> manyTill anySingle eob

-- | Parses example test line.
exampleLineStrP :: CommentStyle -> LineParser ExampleLine
exampleLineStrP style =
    exampleSymbol *> (ExampleLine <$> consume style)

exampleSymbol :: Parser String ()
exampleSymbol = chunk ">>>" *> P.notFollowedBy (char '>')

propSymbol :: Parser String ()
propSymbol = chunk "prop>" *> P.notFollowedBy (char '>')

-- | Parses prop test line.
propLineStrP :: CommentStyle -> LineParser PropLine
propLineStrP style =
    chunk "prop>"
        *> P.notFollowedBy (char '>')
        *> (PropLine <$> consume style)

-- * Utilities

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

{- | Given a map from ranges, divides them into subgroup
 with contiguous line and columns.
-}
groupLineComments ::
    Map Range a -> [NonEmpty (Range, a)]
groupLineComments =
    contiguousGroupOn (fst >>> view start >>> view line &&& view character)
        . Map.toList
