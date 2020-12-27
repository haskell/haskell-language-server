-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Development.IDE.Core.PositionMapping
  ( PositionMapping(..)
  , PositionResult(..)
  , lowerRange
  , upperRange
  , positionResultToMaybe
  , fromCurrentPosition
  , toCurrentPosition
  , PositionDelta(..)
  , addDelta
  , mkDelta
  , toCurrentRange
  , fromCurrentRange
  , applyChange
  , zeroMapping
  -- toCurrent and fromCurrent are mainly exposed for testing
  , toCurrent
  , fromCurrent
  ) where

import Control.Monad
import qualified Data.Text as T
import Language.Haskell.LSP.Types
import Data.List

-- | Either an exact position, or the range of text that was substituted
data PositionResult a
  = PositionRange -- ^ Fields need to be non-strict otherwise bind is exponential
  { unsafeLowerRange :: a
  , unsafeUpperRange :: a }
  | PositionExact !a
  deriving (Eq,Ord,Show,Functor)

lowerRange :: PositionResult a -> a
lowerRange (PositionExact a) = a
lowerRange (PositionRange lower _) = lower

upperRange :: PositionResult a -> a
upperRange (PositionExact a) = a
upperRange (PositionRange _ upper) = upper

positionResultToMaybe :: PositionResult a -> Maybe a
positionResultToMaybe (PositionExact a) = Just a
positionResultToMaybe _ = Nothing

instance Applicative PositionResult where
  pure = PositionExact
  (PositionExact f) <*> a = fmap f a
  (PositionRange f g) <*> (PositionExact a) = PositionRange (f a) (g a)
  (PositionRange f g) <*> (PositionRange lower upper) = PositionRange (f lower) (g upper)

instance Monad PositionResult where
  (PositionExact a) >>= f = f a
  (PositionRange lower upper) >>= f = PositionRange lower' upper'
    where
      lower' = lowerRange $ f lower
      upper' = upperRange $ f upper

-- The position delta is the difference between two versions
data PositionDelta = PositionDelta
  { toDelta :: !(Position -> PositionResult Position)
  , fromDelta :: !(Position -> PositionResult Position)
  }

fromCurrentPosition :: PositionMapping -> Position -> Maybe Position
fromCurrentPosition (PositionMapping pm) = positionResultToMaybe . fromDelta pm

toCurrentPosition :: PositionMapping -> Position -> Maybe Position
toCurrentPosition (PositionMapping pm) = positionResultToMaybe . toDelta pm

-- A position mapping is the difference from the current version to
-- a specific version
newtype PositionMapping = PositionMapping PositionDelta

toCurrentRange :: PositionMapping -> Range -> Maybe Range
toCurrentRange mapping (Range a b) =
    Range <$> toCurrentPosition mapping a <*> toCurrentPosition mapping b

fromCurrentRange :: PositionMapping -> Range -> Maybe Range
fromCurrentRange mapping (Range a b) =
    Range <$> fromCurrentPosition mapping a <*> fromCurrentPosition mapping b

zeroMapping :: PositionMapping
zeroMapping = PositionMapping idDelta

-- | Compose two position mappings. Composes in the same way as function
-- composition (ie the second argument is applyed to the position first).
composeDelta :: PositionDelta
                -> PositionDelta
                -> PositionDelta
composeDelta (PositionDelta to1 from1) (PositionDelta to2 from2) =
  PositionDelta (to1 <=< to2)
                (from1 >=> from2)

idDelta :: PositionDelta
idDelta = PositionDelta pure pure

-- | Convert a set of changes into a delta from k  to k + 1
mkDelta :: [TextDocumentContentChangeEvent] -> PositionDelta
mkDelta cs = foldl' applyChange idDelta cs

-- | Add a new delta onto a Mapping k n to make a Mapping (k - 1) n
addDelta :: PositionDelta -> PositionMapping -> PositionMapping
addDelta delta (PositionMapping pm) = PositionMapping (composeDelta delta pm)

applyChange :: PositionDelta -> TextDocumentContentChangeEvent -> PositionDelta
applyChange PositionDelta{..} (TextDocumentContentChangeEvent (Just r) _ t) = PositionDelta
    { toDelta = toCurrent r t <=< toDelta
    , fromDelta = fromDelta <=< fromCurrent r t
    }
applyChange posMapping _ = posMapping

toCurrent :: Range -> T.Text -> Position -> PositionResult Position
toCurrent (Range start@(Position startLine startColumn) end@(Position endLine endColumn)) t (Position line column)
    | line < startLine || line == startLine && column < startColumn =
      -- Position is before the change and thereby unchanged.
      PositionExact $ Position line column
    | line > endLine || line == endLine && column >= endColumn =
      -- Position is after the change so increase line and column number
      -- as necessary.
      PositionExact $ newLine `seq` newColumn `seq` Position newLine newColumn
    | otherwise = PositionRange start end
    -- Position is in the region that was changed.
    where
        lineDiff = linesNew - linesOld
        linesNew = T.count "\n" t
        linesOld = endLine - startLine
        newEndColumn
          | linesNew == 0 = startColumn + T.length t
          | otherwise = T.length $ T.takeWhileEnd (/= '\n') t
        newColumn
          | line == endLine = column + newEndColumn - endColumn
          | otherwise = column
        newLine = line + lineDiff

fromCurrent :: Range -> T.Text -> Position -> PositionResult Position
fromCurrent (Range start@(Position startLine startColumn) end@(Position endLine endColumn)) t (Position line column)
    | line < startLine || line == startLine && column < startColumn =
      -- Position is before the change and thereby unchanged
      PositionExact $ Position line column
    | line > newEndLine || line == newEndLine && column >= newEndColumn =
      -- Position is after the change so increase line and column number
      -- as necessary.
      PositionExact $ newLine `seq` newColumn `seq` Position newLine newColumn
    | otherwise = PositionRange start end
    -- Position is in the region that was changed.
    where
        lineDiff = linesNew - linesOld
        linesNew = T.count "\n" t
        linesOld = endLine - startLine
        newEndLine = endLine + lineDiff
        newEndColumn
          | linesNew == 0 = startColumn + T.length t
          | otherwise = T.length $ T.takeWhileEnd (/= '\n') t
        newColumn
          | line == newEndLine = column - (newEndColumn - endColumn)
          | otherwise = column
        newLine = line - lineDiff
