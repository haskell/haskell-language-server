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
    hasTests,
    hasPropertyTest,
    splitSections,
    Loc,
    Located (..),
    unLoc,
    Txt,
) where

import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString (..))
import GHC.Generics (Generic)

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

data Section = Section
    { sectionName :: Txt
    , sectionTests :: [Loc Test]
    , sectionLanguage :: Language
    , sectionFormat :: Format
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

isProperty :: Test -> Bool
isProperty (Property _ _) = True
isProperty _ = False

data Format = SingleLine | MultiLine deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON, NFData)

data Language = Plain | Haddock deriving (Eq, Show, Generic, Ord, FromJSON, ToJSON, NFData)

data ExpectedLine = ExpectedLine [LineChunk] | WildCardLine
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

instance IsString ExpectedLine where
    fromString = ExpectedLine . return . LineChunk

data LineChunk = LineChunk String | WildCardChunk
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

instance IsString LineChunk where
    fromString = LineChunk
