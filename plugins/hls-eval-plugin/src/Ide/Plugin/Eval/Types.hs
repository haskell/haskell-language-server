{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE RecordWildCards            #-}

module Ide.Plugin.Eval.Types
    ( locate,
      locate0,
      Test (..),
      isProperty,
      Format (..),
      Language (..),
      Section (..),
      Sections (..),
      hasTests,
      hasPropertyTest,
      splitSections,
      Loc,
      Located (..),
      Comments (..),
      RawBlockComment (..),
      RawLineComment (..),
      unLoc,
      Txt,
      EvalParams(..),
      GetEvalComments(..)
    ,nullComments)
where

import           Control.DeepSeq               (deepseq)
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.List                     (partition)
import           Data.List.NonEmpty            (NonEmpty)
import           Data.Map.Strict               (Map)
import           Data.String                   (IsString (..))
import           Development.IDE               (Range, RuleResult)
import           Development.IDE.Graph.Classes
import           GHC.Generics                  (Generic)
import           Language.LSP.Types            (TextDocumentIdentifier)
import qualified Text.Megaparsec               as P

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

data Sections = Sections
    { nonSetupSections :: [Section]
    , setupSections    :: [Section]
    }
    deriving (Show, Eq, Generic)

data Section = Section
    { sectionName     :: Txt
    , sectionTests    :: [Test]
    , sectionLanguage :: Language
    , sectionFormat   :: Format
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

hasTests :: Section -> Bool
hasTests = not . null . sectionTests

hasPropertyTest :: Section -> Bool
hasPropertyTest = any isProperty . sectionTests

-- |Split setup and normal sections
splitSections :: [Section] -> ([Section], [Section])
splitSections = partition ((== "setup") . sectionName)

data Test
    = Example {testLines :: NonEmpty Txt, testOutput :: [Txt], testRange :: Range}
    | Property {testline :: Txt, testOutput :: [Txt], testRange :: Range}
    deriving (Eq, Show, Generic, FromJSON, ToJSON, NFData)

data GetEvalComments = GetEvalComments
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetEvalComments
instance NFData   GetEvalComments

type instance RuleResult GetEvalComments = Comments
data Comments = Comments
    { lineComments  :: Map Range RawLineComment
    , blockComments :: Map Range RawBlockComment
    }
    deriving (Show, Eq, Ord, Generic)

nullComments :: Comments -> Bool
nullComments Comments{..} = null lineComments && null blockComments

instance NFData Comments

newtype RawBlockComment = RawBlockComment {getRawBlockComment :: String}
    deriving (Show, Eq, Ord)
    deriving newtype
        ( IsString
        , P.Stream
        , P.TraversableStream
        , P.VisualStream
        , Semigroup
        , Monoid
        , NFData
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
        , NFData
        )

instance Semigroup Comments where
    Comments ls bs <> Comments ls' bs' = Comments (ls <> ls') (bs <> bs')

instance Monoid Comments where
    mempty = Comments mempty mempty

isProperty :: Test -> Bool
isProperty Property {} = True
isProperty _           = False

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

type EvalId = Int

-- | Specify the test section to execute
data EvalParams = EvalParams
    { sections :: [Section]
    , module_  :: !TextDocumentIdentifier
    , evalId   :: !EvalId -- ^ unique group id; for test uses
    }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)
