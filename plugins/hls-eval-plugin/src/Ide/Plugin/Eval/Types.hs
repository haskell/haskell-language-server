{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wwarn #-}

module Ide.Plugin.Eval.Types (
    locate,
    locate0,
    Test (..),
    isProperty,
    Format (..),
    Language (..),
    Section (..),
    Sections(..),
    hasTests,
    hasPropertyTest,
    splitSections,
    Loc,
    Located (..),
    Comments(..),
    RawBlockComment(..),
    RawLineComment(..),
    unLoc,
    Txt,
) where

import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import Development.IDE (Range)
import qualified Text.Megaparsec as P

-- | A thing with a location attached.
data Located l a = Located {location :: l, located :: a}
    deriving (Eq, Show, Ord, Functor, Generic, FromJSON, ToJSON)

-- | Discard location information.
unLoc :: Located l a -> a
unLoc (Located _ a) = a

instance (NFData l, NFData a) => NFData (Located l a) where
    rnf (Located loc a) = loc `deepseq` a `deepseq` ()

type Loc = Located Line

type Line = Int

locate :: Loc [a] -> [Loc a]
locate (Located l tst) = zipWith Located [l ..] tst

locate0 :: [a] -> [Loc a]
locate0 = locate . Located 0

type Txt = String

data Sections =
    Sections
    { setupSections :: [Section]
    , lineSections :: [Section]
    , multilineSections :: [Section]
    }
    deriving (Show, Eq, Generic)

data Section
    = Section
        { sectionName :: Txt
        , sectionTests :: [Loc Test]
        , sectionLanguage :: Language
        , sectionFormat :: Format
        , sectionRange :: Range
        }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

hasTests :: Section -> Bool
hasTests = not . null . sectionTests

hasPropertyTest :: Section -> Bool
hasPropertyTest = any (isProperty . unLoc) . sectionTests

-- |Split setup and normal sections
splitSections :: [Section] -> ([Section], [Section])
splitSections = partition ((== "setup") . sectionName)

data Test
    = Example {testLines :: NonEmpty Txt, testOutput :: [Txt]}
    | Property {testline :: Txt, testOutput :: [Txt]}
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

data Comments =
    Comments
        { lineComments :: Map Range RawLineComment
        , blockComments :: Map Range RawBlockComment
        }
    deriving (Show, Eq, Ord, Generic)

newtype RawBlockComment = RawBlockComment {getRawBlockComment :: String}
    deriving (Show, Eq, Ord)
    deriving newtype
        ( IsString
        , P.Stream
        , P.TraversableStream
        , P.VisualStream
        , Semigroup
        , Monoid
        )

newtype RawLineComment = RawLineComment {getRawLineComment :: String}
    deriving (Show, Eq, Ord)
    deriving newtype
        ( IsString
        , P.Stream
        , P.TraversableStream
        , P.VisualStream
        , Semigroup
        , Monoid
        )

instance Semigroup Comments where
    Comments ls bs <> Comments ls' bs' = Comments (ls <> ls') (bs <> bs')

instance Monoid Comments where
    mempty = Comments mempty mempty

isProperty :: Test -> Bool
isProperty (Property _ _) = True
isProperty _ = False

data Format
    = SingleLine
    | -- | @Range@ is that of surrounding entire block comment, not section.
      -- Used for detecting no-newline test commands.
      MultiLine Range
    deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, NFData)
data Language = Plain | Haddock deriving (Eq, Show, Generic, Ord, FromJSON, ToJSON, NFData)

data ExpectedLine = ExpectedLine [LineChunk] | WildCardLine
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

instance IsString ExpectedLine where
    fromString = ExpectedLine . return . LineChunk

data LineChunk = LineChunk String | WildCardChunk
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

instance IsString LineChunk where
    fromString = LineChunk
