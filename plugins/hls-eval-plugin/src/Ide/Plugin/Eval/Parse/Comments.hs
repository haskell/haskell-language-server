{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Ide.Plugin.Eval.Parse.Comments where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import           Control.Arrow                            (first, (&&&), (>>>))
import           Control.Lens                             (lensField, lensRules,
                                                           view, (.~), (^.))
import           Control.Lens.Extras                      (is)
import           Control.Lens.TH                          (makeLensesWith,
                                                           makePrisms,
                                                           mappingNamer)
import           Control.Monad                            (guard, void, when)
import           Control.Monad.Combinators                ()
import           Control.Monad.Reader                     (ask)
import           Control.Monad.Trans.Reader               (Reader, runReader)
import qualified Data.Char                                as C
import qualified Data.DList                               as DL
import qualified Data.Foldable                            as F
import           Data.Function                            ((&))
import           Data.Functor                             ((<&>))
import           Data.Functor.Identity
import           Data.List.NonEmpty                       (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                       as NE hiding (unzip)
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as Map
import qualified Data.Text                                as T
import           Data.Void                                (Void)
import           GHC.Generics                             hiding (UInt, to)
import           Ide.Plugin.Eval.Types
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Types

import qualified Text.Megaparsec                          as P
import           Text.Megaparsec
import           Text.Megaparsec.Char                     (alphaNumChar, char,
                                                           eol, hspace,
                                                           letterChar)

#if MIN_VERSION_base(4,19,0)
import qualified Data.Functor                             as NE (unzip)
#else
import qualified Data.List.NonEmpty                       as NE (unzip)
#endif

{-
We build parsers combining the following three kinds of them:

    *   Line parser - paring a single line into an input,
        works both for line- and block-comments.
        A line should be a proper content of lines contained in comment:
        doesn't include starting @--@ and @{\-@ and no ending @-\}@

    *   Line comment group parser: parses a contiguous group of
        tuples of position and line comment into sections of line comments.
        Each input MUST start with @--@.

    *   Block comment parser: Parsing entire block comment into sections.
        Input must be surrounded by @{\-@ and @-\}@.
-}

-- | Line parser
type LineParser a = forall m. ParsecT Void String m a

-- | Line comment group parser
type LineGroupParser = Parsec Void [(Range, RawLineComment)]

data BlockEnv = BlockEnv
    { isLhs      :: Bool
    , blockRange :: Range
    }
    deriving (Show, Eq, Ord)

makeLensesWith
    (lensRules & lensField .~ mappingNamer (pure . (++ "L")))
    ''BlockEnv

-- | Block comment parser
type BlockCommentParser = ParsecT Void String (Reader BlockEnv)

-- | Prop line, with "prop>" stripped off
newtype PropLine = PropLine {getPropLine :: String}
    deriving (Show)

-- | Example line, with @>>>@ stripped off
newtype ExampleLine = ExampleLine {getExampleLine :: String}
    deriving (Show)

data TestComment
    = AProp
        { testCommentRange :: Range
        , lineProp         :: PropLine
        , propResults      :: [String]
        }
    | AnExample
        { testCommentRange :: Range
        , lineExamples     :: NonEmpty ExampleLine
        , exampleResults   :: [String]
        }
    deriving (Show)

-- | Classification of comments
data CommentFlavour = Vanilla | HaddockNext | HaddockPrev | Named String
    deriving (Read, Show, Eq, Ord)

-- | Single line or block comments?
data CommentStyle = Line | Block Range
    deriving (Show, Eq, Ord, Generic)

makePrisms ''CommentStyle

commentsToSections ::
    -- | True if it is literate Haskell
    Bool ->
    Comments ->
    Sections
commentsToSections isLHS Comments {..} =
    let (lineSectionSeeds, lineSetupSeeds) =
            foldMap
                ( \lcs ->
                    let theRan =
                            Range
                                (view L.start $ fst $ NE.head lcs)
                                (view L.end $ fst $ NE.last lcs)
                     in case parseMaybe lineGroupP $ NE.toList lcs of
                            Nothing -> mempty
                            Just (mls, rs) ->
                                ( maybe mempty (Map.singleton theRan) mls
                                , -- orders setup sections in ascending order
                                  if null rs
                                    then mempty
                                    else
                                        Map.singleton theRan $
                                            DL.singleton (Line, rs)
                                )
                )
                $ groupLineComments $
                    Map.filterWithKey
                        -- FIXME:
                        -- To comply with the initial behaviour of
                        -- Extended Eval Plugin;
                        -- but it also rejects modules with
                        -- non-zero base indentation level!
                        ( \pos _ ->
                            if isLHS
                                then pos ^. L.start . L.character == 2
                                else pos ^. L.start . L.character == 0
                        )
                        lineComments
        (blockSeed, blockSetupSeeds) =
            foldMap
                ( \(ran, lcs) ->
                    case parseBlockMaybe isLHS ran blockCommentBP $
                        getRawBlockComment lcs of
                        Nothing -> mempty
                        Just (Named "setup", grp) ->
                            -- orders setup sections in ascending order
                            ( mempty
                            , Map.singleton ran $
                                DL.singleton (Block ran, grp)
                            )
                        Just grp ->
                            ( Map.singleton ran grp
                            , mempty
                            )
                )
                -- It seems Extended Eval Plugin doesn't constraint
                -- starting indentation level for block comments.
                -- Rather, it constrains the indentation level /inside/
                -- block comment body.
                $ Map.toList blockComments
        lineSections =
            lineSectionSeeds <&> uncurry (testsToSection Line)
        multilineSections =
            Map.mapWithKey
                (uncurry . testsToSection . Block)
                blockSeed
        setupSections =
            -- Setups doesn't need Dummy position
            map
                ( \(style, tests) ->
                    testsToSection
                        style
                        (Named "setup")
                        tests
                )
                $ DL.toList $
                    F.fold $
                        Map.unionWith (<>) lineSetupSeeds blockSetupSeeds
        nonSetupSections = F.toList $ lineSections `Map.union` multilineSections
     in Sections {..}

parseBlockMaybe :: Bool -> Range -> BlockCommentParser a -> String -> Maybe a
parseBlockMaybe isLhs blockRange p i =
    case runReader (runParserT p' "" i) BlockEnv {..} of
        Left {} -> Nothing
        Right a -> Just a
    where
        p' = do
            updateParserState $ \st ->
                st
                    { statePosState =
                        (statePosState st)
                            { pstateSourcePos = positionToSourcePos $ blockRange ^. L.start
                            }
                    }
            p

type CommentRange = Range

type SectionRange = Range

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
            _           -> Plain
        sectionTests = map fromTestComment tests
        sectionFormat =
            case style of
                Line      -> SingleLine
                Block ran -> MultiLine ran
     in Section {..}

fromTestComment :: TestComment -> Test
fromTestComment AProp {..} =
    Property
        { testline = getPropLine lineProp
        , testOutput = propResults
        , testRange = testCommentRange
        }
fromTestComment AnExample {..} =
    Example
        { testLines = getExampleLine <$> lineExamples
        , testOutput = exampleResults
        , testRange = testCommentRange
        }

-- * Block comment parser

{- $setup
>>> dummyPos = Position 0 0
>>> parseE p = either (error . errorBundlePretty) id . parse p ""
-}

-- >>> parseE (blockCommentBP True dummyPos) "{- |\n  >>> 5+5\n  11\n  -}"
-- (HaddockNext,[AnExample {testCommentRange = Position {_line = 1, _character = 0}, lineExamples = ExampleLine {getExampleLine = " 5+5"} :| [], exampleResults = ["  11"]}])

blockCommentBP ::
    -- | True if Literate Haskell
    BlockCommentParser (CommentFlavour, [TestComment])
blockCommentBP = do
    skipCount 2 anySingle -- "{-"
    void $ optional $ char ' '
    flav <- commentFlavourP
    hit <- skipNormalCommentBlock
    if hit
        then do
            body <-
                many $
                    (blockExamples <|> blockProp)
                        <* skipNormalCommentBlock
            void takeRest -- just consume the rest
            pure (flav, body)
        else pure (flav, [])

skipNormalCommentBlock :: BlockCommentParser Bool
skipNormalCommentBlock = do
    BlockEnv {..} <- ask
    skipManyTill (normalLineP isLhs $ Block blockRange) $
        False <$ try (optional (chunk "-}") *> eof)
            <|> True <$ lookAhead (try $ testSymbol isLhs $ Block blockRange)

testSymbol :: Bool -> CommentStyle -> LineParser ()
testSymbol isLHS style =
    -- FIXME: To comply with existing Extended Eval Plugin Behaviour;
    -- it must skip one space after a comment!
    -- This prevents Eval Plugin from working on
    -- modules with non-standard base indentation-level.
    when (isLHS && is _Block style) (void $ count' 0 2 $ char ' ')
        *> (exampleSymbol <|> propSymbol)

eob :: LineParser ()
eob = eof <|> try (optional (chunk "-}") *> eof) <|> void eol

blockExamples
    , blockProp ::
        BlockCommentParser TestComment
blockExamples = do
    BlockEnv {..} <- ask
    (ran, examples) <- withRange $ NE.some $ exampleLineStrP isLhs $ Block blockRange
    AnExample ran examples <$> resultBlockP
blockProp = do
    BlockEnv {..} <- ask
    (ran, Identity prop) <- withRange $ fmap Identity $ propLineStrP isLhs $ Block blockRange
    AProp ran prop <$> resultBlockP

withRange ::
    (TraversableStream s, Ord v, Traversable t) =>
    ParsecT v s m (t (a, Position)) ->
    ParsecT v s m (Range, t a)
withRange p = do
    beg <- sourcePosToPosition <$> getSourcePos
    as <- p
    let fin
            | null as = beg
            | otherwise = snd $ last $ F.toList as
    pure (Range beg fin, fst <$> as)

resultBlockP :: BlockCommentParser [String]
resultBlockP = do
    BlockEnv {..} <- ask
    many $
        fmap fst $ nonEmptyNormalLineP isLhs $
            Block blockRange

positionToSourcePos :: Position -> SourcePos
positionToSourcePos pos =
    P.SourcePos
        { sourceName = "<block comment>"
        , sourceLine = P.mkPos $ fromIntegral $ 1 + pos ^. L.line
        , sourceColumn = P.mkPos $ fromIntegral $ 1 + pos ^. L.character
        }

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition SourcePos {..} =
    Position (fromIntegral $ unPos sourceLine - 1) (fromIntegral $ unPos sourceColumn - 1)

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
        flav          -> (,mempty) . Just . (flav,) <$> lineCommentSectionsP

-- >>>  parse (lineGroupP <*eof) "" $ (dummyPosition, ) . RawLineComment <$> ["-- a", "-- b"]
-- Variable not in scope: dummyPosition :: Position

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
    parseLine (fst <$ commentFlavourP <*> normalLineP False Line)

nonEmptyLGP :: LineGroupParser String
nonEmptyLGP =
    try $
        fmap snd $
            parseLine $
                fst <$ commentFlavourP <*> nonEmptyNormalLineP False Line

exampleLinesGP :: LineGroupParser TestComment
exampleLinesGP =
    lexemeLine $
        uncurry AnExample . first convexHullRange . NE.unzip
            <$> NE.some exampleLineGP
            <*> resultLinesP

convexHullRange :: NonEmpty Range -> Range
convexHullRange nes =
    Range (NE.head nes ^. L.start) (NE.last nes ^. L.end)

exampleLineGP :: LineGroupParser (Range, ExampleLine)
exampleLineGP =
    -- In line-comments, indentation-level inside comment doesn't matter.
    parseLine (fst <$ commentFlavourP <*> exampleLineStrP False Line)

propLineGP :: LineGroupParser (Range, PropLine)
propLineGP =
    -- In line-comments, indentation-level inside comment doesn't matter.
    parseLine (fst <$ commentFlavourP <*> propLineStrP False Line)

{- |
Turning a line parser into line group parser consuming a single line comment.
Parses a sinlge line comment, skipping prefix "--[-*]" with optional one horizontal space.
fails if the input does not start with "--".

__N.B.__ We don't strip comment flavours.

>>> pck = (:[]).(:[]) . RawLineComment

>>> parseMaybe (parseLine $ takeRest) $ pck "-- >>> A"
Just [">>> A"]

>>> parseMaybe (parseLine $ takeRest) $ pck "---  >>> A"
Just [" >>> A"]

>>> parseMaybe (parseLine takeRest) $ pck ""
Nothing
-}
parseLine ::
    (Ord (f RawLineComment), Traversable f) =>
    LineParser a ->
    Parsec Void [f RawLineComment] (f a)
parseLine p =
    P.token
        (mapM $ parseMaybe (lineCommentHeadP *> p) . getRawLineComment)
        mempty

-- * Line Parsers

-- | Non-empty normal line.
nonEmptyNormalLineP ::
    -- | True if Literate Haskell
    Bool ->
    CommentStyle ->
    LineParser (String, Position)
nonEmptyNormalLineP isLHS style = try $ do
    (ln, pos) <- normalLineP isLHS style
    guard $
        case style of
            Block{} -> T.strip (T.pack ln) `notElem` ["{-", "-}", ""]
            _       -> not $ all C.isSpace ln
    pure (ln, pos)

{- | Normal line is a line neither a example nor prop.
 Empty line is normal.
-}
normalLineP ::
    -- | True if Literate Haskell
    Bool ->
    CommentStyle ->
    LineParser (String, Position)
normalLineP isLHS style = do
    notFollowedBy
        (try $ testSymbol isLHS style)
    when (isLHS && is _Block style) $
        void $ count' 0 2 $ char ' '
    consume style

consume :: CommentStyle -> LineParser (String, Position)
consume style =
    case style of
        Line     -> (,) <$> takeRest <*> getPosition
        Block {} -> manyTill_ anySingle (getPosition <* eob)

getPosition :: (Ord v, TraversableStream s) => ParsecT v s m Position
getPosition = sourcePosToPosition <$> getSourcePos

-- | Parses example test line.
exampleLineStrP ::
    -- | True if Literate Haskell
    Bool ->
    CommentStyle ->
    LineParser (ExampleLine, Position)
exampleLineStrP isLHS style =
    try $
        -- FIXME: To comply with existing Extended Eval Plugin Behaviour;
        -- it must skip one space after a comment!
        -- This prevents Eval Plugin from working on
        -- modules with non-standard base indentation-level.
        when (isLHS && is _Block style) (void $ count' 0 2 $ char ' ')
            *> exampleSymbol
            *> (first ExampleLine <$> consume style)

exampleSymbol :: LineParser ()
exampleSymbol =
    chunk ">>>" *> P.notFollowedBy (char '>')

propSymbol :: LineParser ()
propSymbol = chunk "prop>" *> P.notFollowedBy (char '>')

-- | Parses prop test line.
propLineStrP ::
    -- | True if Literate HAskell
    Bool ->
    CommentStyle ->
    LineParser (PropLine, Position)
propLineStrP isLHS style =
    -- FIXME: To comply with existing Extended Eval Plugin Behaviour;
    -- it must skip one space after a comment!
    -- This prevents Eval Plugin from working on
    -- modules with non-standard base indentation-level.
    when (isLHS && is _Block style) (void $ count' 0 2 $ char ' ')
        *> chunk "prop>"
        *> P.notFollowedBy (char '>')
        *> (first PropLine <$> consume style)

-- * Utilities

{- |
Given a sequence of tokens increasing in their starting position,
groups them into sublists consisting of contiguous tokens;
Two adjacent tokens are considered to be contiguous if

    * line number increases by 1, and
    * they have same starting column.

>>> contiguousGroupOn id [(1,2),(2,2),(3,4),(4,4),(5,4),(7,0),(8,0)]
[(1,2) :| [(2,2)],(3,4) :| [(4,4),(5,4)],(7,0) :| [(8,0)]]
-}
contiguousGroupOn :: (a -> (UInt, UInt)) -> [a] -> [NonEmpty a]
contiguousGroupOn toLineCol = foldr step []
    where
        step a [] = [pure a]
        step a bss0@((b :| bs) : bss)
            | let (aLine, aCol) = toLineCol a
              , let (bLine, bCol) = toLineCol b
              , aLine + 1 == bLine && aCol == bCol =
                (a :| b : bs) : bss
            | otherwise = pure a : bss0

{- | Given a map from positions, divides them into subgroup
 with contiguous line and columns.
-}
groupLineComments ::
    Map Range a -> [NonEmpty (Range, a)]
groupLineComments =
    contiguousGroupOn (fst >>> view L.start >>> view L.line &&& view L.character)
        . Map.toList
