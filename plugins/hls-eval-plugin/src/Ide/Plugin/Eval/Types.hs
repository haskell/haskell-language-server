{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wwarn #-}

module Ide.Plugin.Eval.Types
    ( Log(..),
      locate,
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
      GetEvalComments(..),
      IsEvaluating(..),
      nullComments)
where

import           Control.Arrow                   ((>>>))
import           Control.DeepSeq                 (deepseq)
import           Control.Lens
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.List                       (partition)
import           Data.List.NonEmpty              (NonEmpty)
import           Data.Map.Strict                 (Map)
import           Data.String                     (IsString (..))
import qualified Data.Text                       as T
import           Development.IDE                 (Range, RuleResult)
import qualified Development.IDE.Core.Shake      as Shake
import qualified Development.IDE.GHC.Compat.Core as Core
import           Development.IDE.Graph.Classes
import           GHC.Generics                    (Generic)
import           Ide.Logger
import           Ide.Plugin.Eval.GHC             (showDynFlags)
import           Ide.Plugin.Eval.Util
import           Language.LSP.Protocol.Types     (TextDocumentIdentifier,
                                                  TextEdit)
import qualified System.Time.Extra               as Extra
import qualified Text.Megaparsec                 as P

data Log
    = LogShake Shake.Log
    | LogCodeLensFp FilePath
    | LogCodeLensComments Comments
    | LogExecutionTime T.Text Extra.Seconds
    | LogTests !Int !Int !Int !Int
    | LogRunTestResults [T.Text]
    | LogRunTestEdits TextEdit
    | LogEvalFlags [String]
    | LogEvalPreSetDynFlags Core.DynFlags
    | LogEvalParsedFlags
        (Either
            Core.GhcException
            (Core.DynFlags, [Core.Located String], DynFlagsParsingWarnings))
    | LogEvalPostSetDynFlags Core.DynFlags
    | LogEvalStmtStart String
    | LogEvalStmtResult (Maybe [T.Text])
    | LogEvalImport String
    | LogEvalDeclaration String

instance Pretty Log where
    pretty = \case
        LogShake shakeLog -> pretty shakeLog
        LogCodeLensFp fp -> "fp" <+> pretty fp
        LogCodeLensComments comments -> "comments" <+> viaShow comments
        LogExecutionTime lbl duration -> pretty lbl <> ":" <+> pretty (Extra.showDuration duration)
        LogTests nTests nNonSetupSections nSetupSections nLenses -> "Tests" <+> fillSep
            [ pretty nTests
            , "tests in"
            , pretty nNonSetupSections
            , "sections"
            , pretty nSetupSections
            , "setups"
            , pretty nLenses
            , "lenses."
            ]
        LogRunTestResults results ->  "TEST RESULTS" <+> viaShow results
        LogRunTestEdits edits -> "TEST EDIT" <+> viaShow edits
        LogEvalFlags flags -> "{:SET" <+> pretty flags
        LogEvalPreSetDynFlags dynFlags -> "pre set" <+> pretty (showDynFlags dynFlags)
        LogEvalParsedFlags eans -> "parsed flags" <+> viaShow (eans
              <&> (_1 %~ showDynFlags >>> _3 %~ prettyWarnings))
        LogEvalPostSetDynFlags dynFlags -> "post set" <+> pretty (showDynFlags dynFlags)
        LogEvalStmtStart stmt -> "{STMT" <+> pretty stmt
        LogEvalStmtResult result -> "STMT}" <+> pretty result
        LogEvalImport stmt -> "{IMPORT" <+> pretty stmt
        LogEvalDeclaration stmt -> "{DECL" <+> pretty stmt

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

data IsEvaluating = IsEvaluating
    deriving (Eq, Show, Generic)
instance Hashable IsEvaluating
instance NFData   IsEvaluating

type instance RuleResult IsEvaluating = Bool

data GetEvalComments = GetEvalComments
    deriving (Eq, Show, Generic)
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
