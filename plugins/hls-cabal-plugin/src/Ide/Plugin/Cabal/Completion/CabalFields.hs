{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Ide.Plugin.Cabal.Completion.CabalFields (findStanzaForColumn, findFieldSection, findTextWord, findFieldLine, getOptionalSectionName, getAnnotation, getFieldName, onelineSectionArgs, getFieldEndPosition, getSectionArgEndPosition, getNameEndPosition, getFieldLineEndPosition, getFieldLSPRange) where

import qualified Data.ByteString                   as BS
import           Data.List                         (find)
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty                as NE
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Distribution.Fields               as Syntax
import qualified Distribution.Parsec.Position      as Syntax
import           Ide.Plugin.Cabal.Completion.Types
import qualified Language.LSP.Protocol.Types       as LSPTypes

-- ----------------------------------------------------------------
-- Cabal-syntax utilities I don't really want to write myself
-- ----------------------------------------------------------------

-- | Determine the context of a cursor position within a stack of stanza contexts
--
-- If the cursor is indented more than one of the stanzas in the stack
-- the respective stanza is returned if this is never the case, the toplevel stanza
-- in the stack is returned.
findStanzaForColumn :: Int -> NonEmpty (Int, StanzaContext) -> (StanzaContext, FieldContext)
findStanzaForColumn col ctx = case NE.uncons ctx of
    ((_, stanza), Nothing) -> (stanza, None)
    ((indentation, stanza), Just res)
        | col < indentation -> findStanzaForColumn col res
        | otherwise -> (stanza, None)

-- | Determine the field the cursor is currently a part of.
--
-- The result is said field and its starting position
-- or Nothing if the passed list of fields is empty.

-- This only looks at the row of the cursor and not at the cursor's
-- position within the row.
--
-- TODO: we do not handle braces correctly. Add more tests!
findFieldSection :: Syntax.Position -> [Syntax.Field Syntax.Position] -> Maybe (Syntax.Field Syntax.Position)
findFieldSection _cursor [] = Nothing
findFieldSection _cursor [x] =
  -- Last field. We decide later, whether we are starting
  -- a new section.
  Just x
findFieldSection cursor (x:y:ys)
    | Syntax.positionRow (getAnnotation x) <= cursorLine && cursorLine < Syntax.positionRow (getAnnotation y)
    = Just x
    | otherwise = findFieldSection cursor (y:ys)
  where
    cursorLine = Syntax.positionRow cursor

-- | Determine the field line the cursor is currently a part of.
--
-- The result is said field line and its starting position
-- or Nothing if the passed list of fields is empty.

-- This function assumes that elements in a field's @FieldLine@ list
-- do not share the same row.
findFieldLine :: Syntax.Position -> [Syntax.Field Syntax.Position] -> Maybe (Syntax.FieldLine Syntax.Position)
findFieldLine _cursor [] = Nothing
findFieldLine cursor fields =
  case findFieldSection cursor fields of
    Nothing                          -> Nothing
    Just (Syntax.Field _ fieldLines) -> find filterLineFields fieldLines
    Just (Syntax.Section _ _ fields) -> findFieldLine cursor fields
  where
    cursorLine = Syntax.positionRow cursor
    -- In contrast to `Field` or `Section`, `FieldLine` must have the exact
    -- same line position as the cursor.
    filterLineFields (Syntax.FieldLine pos _) = Syntax.positionRow pos == cursorLine

-- | Determine the exact word at the current cursor position.
--
-- The result is said word or Nothing if the passed list is empty
-- or the cursor position is not next to, or on a word.
-- For this function, a word is a sequence of consecutive characters
-- that are not a space or column.

-- This function currently only considers words inside of a @FieldLine@.
findTextWord :: Syntax.Position -> [Syntax.Field Syntax.Position] -> Maybe T.Text
findTextWord _cursor [] = Nothing
findTextWord cursor fields =
  case findFieldLine cursor fields of
    Nothing -> Nothing
    Just (Syntax.FieldLine pos byteString) ->
      let decodedText  = T.decodeUtf8 byteString
          lineFieldCol = Syntax.positionCol pos
          lineFieldLen = T.length decodedText
          offset = cursorCol - lineFieldCol in
      -- Range check if cursor is inside or or next to found line.
      -- The latter comparison includes the length of the line as offset,
      -- which is done to also include cursors that are at the end of a line.
      --    e.g. "foo,bar|"
      --                 ^
      --               cursor
      --
      -- Having an offset which is outside of the line is possible because of `splitAt`.
      if offset >= 0 && lineFieldLen >= offset
        then
          let (lhs, rhs)  = T.splitAt offset decodedText
              strippedLhs = T.takeWhileEnd isAllowedChar lhs
              strippedRhs = T.takeWhile isAllowedChar rhs
              resultText  = T.concat [strippedLhs, strippedRhs] in
          -- It could be possible that the cursor was in-between separators, in this
          -- case the resulting text would be empty, which should result in `Nothing`.
          --    e.g. " foo ,| bar"
          --                ^
          --              cursor
          if not $ T.null resultText then Just resultText else Nothing
        else
          Nothing
  where
    cursorCol = Syntax.positionCol cursor
    separators = [',', ' ']
    isAllowedChar = (`notElem` separators)

type FieldName = T.Text

getAnnotation :: Syntax.Field ann -> ann
getAnnotation (Syntax.Field (Syntax.Name ann _) _)     = ann
getAnnotation (Syntax.Section (Syntax.Name ann _) _ _) = ann

getFieldName :: Syntax.Field ann -> FieldName
getFieldName (Syntax.Field (Syntax.Name _ fn) _)     = T.decodeUtf8 fn
getFieldName (Syntax.Section (Syntax.Name _ fn) _ _) = T.decodeUtf8 fn

-- | Returns the name of a section if it has a name.
--
-- This assumes that the given section args belong to named stanza
-- in which case the stanza name is returned.
getOptionalSectionName :: [Syntax.SectionArg ann] -> Maybe T.Text
getOptionalSectionName [] = Nothing
getOptionalSectionName (x:xs) = case x of
    Syntax.SecArgName _ name -> Just (T.decodeUtf8 name)
    _                        -> getOptionalSectionName xs


-- | Makes a single text line out of multiple
--   @SectionArg@s. Allows to display conditions,
--   flags, etc in one line, which is easier to read.
--
--   For example, @flag@ @(@ @pedantic@ @)@ will be joined in
--   one line, instead of four @SectionArg@s separately.
onelineSectionArgs :: [Syntax.SectionArg ann] -> T.Text
onelineSectionArgs sectionArgs = joinedName
  where
    joinedName = T.unwords $ map getName sectionArgs

    getName :: Syntax.SectionArg ann -> T.Text
    getName (Syntax.SecArgName _ identifier)  = T.decodeUtf8 identifier
    getName (Syntax.SecArgStr _ quotedString) = T.decodeUtf8 quotedString
    getName (Syntax.SecArgOther _ string)     = T.decodeUtf8 string


-- | Returns the end position of a provided field
getFieldEndPosition :: Syntax.Field Syntax.Position -> Syntax.Position
getFieldEndPosition (Syntax.Field name [])       = getNameEndPosition name
getFieldEndPosition (Syntax.Field _ (x:xs))      = getFieldLineEndPosition $ NE.last (x NE.:| xs)
getFieldEndPosition (Syntax.Section name [] [])  = getNameEndPosition name
getFieldEndPosition (Syntax.Section _ (x:xs) []) = getSectionArgEndPosition $ NE.last (x NE.:| xs)
getFieldEndPosition (Syntax.Section _ _ (x:xs))  = getFieldEndPosition $ NE.last (x NE.:| xs)

-- | Returns the end position of a provided section arg
getSectionArgEndPosition :: Syntax.SectionArg Syntax.Position -> Syntax.Position
getSectionArgEndPosition (Syntax.SecArgName (Syntax.Position row col) byteString)  = Syntax.Position row (col + BS.length byteString)
getSectionArgEndPosition (Syntax.SecArgStr (Syntax.Position row col) byteString)   = Syntax.Position row (col + BS.length byteString)
getSectionArgEndPosition (Syntax.SecArgOther (Syntax.Position row col) byteString) = Syntax.Position row (col + BS.length byteString)

-- | Returns the end position of a provided name
getNameEndPosition :: Syntax.Name Syntax.Position -> Syntax.Position
getNameEndPosition (Syntax.Name (Syntax.Position row col) byteString) = Syntax.Position row (col + BS.length byteString)

-- | Returns the end position of a provided field line
getFieldLineEndPosition :: Syntax.FieldLine Syntax.Position -> Syntax.Position
getFieldLineEndPosition (Syntax.FieldLine (Syntax.Position row col) byteString) = Syntax.Position row (col + BS.length byteString)

-- | Returns a LSP compatible range for a provided field
getFieldLSPRange :: Syntax.Field Syntax.Position -> LSPTypes.Range
getFieldLSPRange field = LSPTypes.Range startLSPPos endLSPPos
  where
    startLSPPos = cabalPositionToLSPPosition $ getAnnotation field
    endLSPPos   = cabalPositionToLSPPosition $ getFieldEndPosition field
