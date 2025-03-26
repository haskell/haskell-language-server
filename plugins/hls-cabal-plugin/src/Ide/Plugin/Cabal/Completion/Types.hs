{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.Cabal.Completion.Types where

import           Control.DeepSeq                 (NFData)
import           Control.Lens                    ((^.))
import           Data.Hashable
import qualified Data.Text                       as T
import           Development.IDE                 as D
import qualified Distribution.Fields             as Syntax
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Parsec.Position    as Syntax
import           GHC.Generics
import qualified Language.LSP.Protocol.Lens      as JL

data Log
  = LogFileSplitError Position
  | -- | This should never occur since we extract the word to lookup from the same map we look it up in.
    LogUnknownKeyWordInContextError KeyWordName
  | -- | This should never occur since we extract the word to lookup from the same map we look it up in.
    LogUnknownStanzaNameInContextError StanzaName
  | LogFilePathCompleterIOError FilePath IOError
  | LogUseWithStaleFastNoResult
  | LogMapLookUpOfKnownKeyFailed T.Text
  | LogCompletionContext Context
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogFileSplitError pos -> "An error occurred when trying to separate the lines of the cabal file at position:" <+> pretty pos
    LogUnknownKeyWordInContextError kw ->
      "Lookup of key word failed for:" <+> viaShow kw
    LogUnknownStanzaNameInContextError sn ->
      "Lookup of stanza name failed for:" <+> viaShow sn
    LogFilePathCompleterIOError fp ioErr ->
      "When trying to complete the file path:" <+> pretty fp <+> "the following unexpected IO error occurred" <+> viaShow ioErr
    LogUseWithStaleFastNoResult -> "Package description couldn't be read"
    LogMapLookUpOfKnownKeyFailed key -> "Lookup of key in map failed even though it should exist" <+> pretty key
    LogCompletionContext ctx -> "Completion context is:" <+> pretty ctx

type instance RuleResult ParseCabalFile = PD.GenericPackageDescription

data ParseCabalFile = ParseCabalFile
  deriving (Eq, Show, Generic)

instance Hashable ParseCabalFile

instance NFData ParseCabalFile

type instance RuleResult ParseCabalFields = [Syntax.Field Syntax.Position]

data ParseCabalFields = ParseCabalFields
  deriving (Eq, Show, Generic)

instance Hashable ParseCabalFields

instance NFData ParseCabalFields

type instance RuleResult ParseCabalCommonSections = [Syntax.Field Syntax.Position]

data ParseCabalCommonSections = ParseCabalCommonSections
  deriving (Eq, Show, Generic)

instance Hashable ParseCabalCommonSections

instance NFData ParseCabalCommonSections

-- | The context a cursor can be in within a cabal file.
--
--  We can be in stanzas or the top level,
--  and additionally we can be in a context where we have already
--  written a keyword but no value for it yet
type Context = (StanzaContext, FieldContext)

-- | Context inside a cabal file.
--  Used to decide which keywords to suggest.
data StanzaContext
  = -- | Top level context in a cabal file such as 'author'
    TopLevel
  | -- | Nested context in a cabal file, such as 'library'.
    --
    -- Stanzas have their own fields which differ from top-level fields.
    -- Each stanza must be named, such as 'executable exe',
    -- except for the main library.
    Stanza !StanzaType !(Maybe StanzaName)
  deriving (Eq, Show, Read)

instance Pretty StanzaContext where
    pretty TopLevel      = "TopLevel"
    pretty (Stanza t ms) = "Stanza" <+> pretty t <+> (maybe mempty pretty ms)

-- | Keyword context in a cabal file.
--
--  Used to decide whether to suggest values or keywords.
data FieldContext
  = -- | Key word context, where a keyword
    -- occurs right before the current word
    -- to be completed
    KeyWord !KeyWordName
  | -- | Keyword context where no keyword occurs
    -- right before the current word to be completed
    None
  deriving (Eq, Show, Read)

instance Pretty FieldContext where
    pretty (KeyWord kw) = "KeyWord" <+> pretty kw
    pretty None         = "No Keyword"

type KeyWordName = T.Text

type StanzaName = T.Text

type StanzaType = T.Text

-- | Information regarding the current completion status
--
--  Example: @"dir1/fi@ having been written to the file
--  would correspond to:
--
--  @
--    completionPrefix = "dir1/fi"
--    isStringNotation = LeftSide
--    ...
--  @
--
--  We define this type instead of simply using
--  VFS.PosPrefixInfo since e.g. for filepaths we
--  need more than just the word before the
--  cursor (as can be seen above),
--  since we want to capture the whole filepath
--  before the cursor.
--
--  We also use this type to wrap all information
--  necessary to complete filepaths and other values
--  in a cabal file.
data CabalPrefixInfo = CabalPrefixInfo
  { -- | text prefix to complete
    completionPrefix         :: T.Text,
    -- | Did the completion happen in the context of a string notation,
    -- i.e. are there apostrophes around the item to be completed
    isStringNotation         :: Maybe Apostrophe,
    -- | the current position of the cursor in the file
    completionCursorPosition :: Position,
    -- | range where completion is to be inserted
    completionRange          :: Range,
    -- | directory of the handled cabal file
    completionWorkingDir     :: FilePath,
    -- | filename of the handled cabal file
    completionFileName       :: T.Text
  }
  deriving (Eq, Show)

-- | Where are the apostrophes around the item to be completed?
--
--  'Surrounded' means the item to complete already has the necessary apostrophes,
--  while 'LeftSide' means, a closing apostrophe has to be added after the completion item.
data Apostrophe = Surrounded | LeftSide
  deriving (Eq, Ord, Show)

-- | Wraps a completion in apostrophes where appropriate.
--
--  If a completion starts with an apostrophe we want to end it with an apostrophe.
--  If a completed filepath contains a space, it can only be written in the cabal
--  file if it is wrapped in apostrophes, thus we wrap it.
applyStringNotation :: Maybe Apostrophe -> T.Text -> T.Text
applyStringNotation (Just Surrounded) compl = compl
applyStringNotation (Just LeftSide) compl = compl <> "\""
applyStringNotation Nothing compl
  | Just _ <- T.find (== ' ') compl = "\"" <> compl <> "\""
  | otherwise = compl

-- | Convert an LSP 'Position' to a 'Syntax.Position'.
--
-- Cabal Positions start their indexing at 1 while LSP starts at 0.
-- This helper makes sure, the translation is done properly.
lspPositionToCabalPosition :: Position -> Syntax.Position
lspPositionToCabalPosition pos = Syntax.Position
  (fromIntegral (pos ^. JL.line) + 1)
  (fromIntegral (pos ^. JL.character) + 1)

-- | Convert an 'Syntax.Position' to a LSP 'Position'.
--
-- Cabal Positions start their indexing at 1 while LSP starts at 0.
-- This helper makes sure, the translation is done properly.
cabalPositionToLSPPosition :: Syntax.Position -> Position
cabalPositionToLSPPosition (Syntax.Position start end) = Position (toEnum start -1) (toEnum end -1)
