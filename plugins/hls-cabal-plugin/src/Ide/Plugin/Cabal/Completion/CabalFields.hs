module Ide.Plugin.Cabal.Completion.CabalFields (findStanzaForColumn, findFieldSection, getOptionalSectionName, getAnnotation, getFieldName) where

import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty                as NE
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Distribution.Fields               as Syntax
import qualified Distribution.Parsec.Position      as Syntax
import           Ide.Plugin.Cabal.Completion.Types

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

