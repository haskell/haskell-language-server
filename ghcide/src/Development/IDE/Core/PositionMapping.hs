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
  , addOldDelta
  , idDelta
  , composeDelta
  , mkDelta
  , toCurrentRange
  , fromCurrentRange
  , applyChange
  , zeroMapping
  , deltaFromDiff
  -- toCurrent and fromCurrent are mainly exposed for testing
  , toCurrent
  , fromCurrent
  ) where

import           Control.DeepSeq
import           Control.Lens                ((^.))
import           Control.Monad
import           Data.Algorithm.Diff
import           Data.Bifunctor
import           Data.List
import qualified Data.Text                   as T
import qualified Data.Vector.Unboxed         as V
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types (Position (Position),
                                              Range (Range),
                                              TextDocumentContentChangeEvent (TextDocumentContentChangeEvent),
                                              UInt, type (|?) (InL))

-- | Either an exact position, or the range of text that was substituted
data PositionResult a
  = PositionRange -- ^ Fields need to be non-strict otherwise bind is exponential
  { unsafeLowerRange :: a
  , unsafeUpperRange :: a }
  | PositionExact !a
  deriving (Eq,Ord,Show,Functor)

lowerRange :: PositionResult a -> a
lowerRange (PositionExact a)       = a
lowerRange (PositionRange lower _) = lower

upperRange :: PositionResult a -> a
upperRange (PositionExact a)       = a
upperRange (PositionRange _ upper) = upper

positionResultToMaybe :: PositionResult a -> Maybe a
positionResultToMaybe (PositionExact a) = Just a
positionResultToMaybe _                 = Nothing

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
  { toDelta   :: !(Position -> PositionResult Position)
  , fromDelta :: !(Position -> PositionResult Position)
  }

instance Show PositionDelta where
  show PositionDelta{} = "PositionDelta{..}"

instance NFData PositionDelta where
  rnf (PositionDelta a b) = a `seq` b `seq` ()

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
-- composition (ie the second argument is applied to the position first).
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

-- | addOldDelta
-- Add a old delta onto a Mapping k n to make a Mapping (k - 1) n
addOldDelta ::
    PositionDelta -- ^ delta from version k - 1 to version k
    -> PositionMapping -- ^ The input mapping is from version k to version n
    -> PositionMapping -- ^ The output mapping is from version k - 1 to version n
addOldDelta delta (PositionMapping pm) = PositionMapping (composeDelta pm delta)

-- TODO: We currently ignore the right hand side (if there is only text), as
-- that was what was done with lsp* 1.6 packages
applyChange :: PositionDelta -> TextDocumentContentChangeEvent -> PositionDelta
applyChange PositionDelta{..} (TextDocumentContentChangeEvent (InL x)) = PositionDelta
    { toDelta = toCurrent (x ^. L.range) (x ^. L.text) <=< toDelta
    , fromDelta = fromDelta <=< fromCurrent (x ^. L.range) (x ^. L.text)
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
        linesOld = fromIntegral endLine - fromIntegral startLine
        newEndColumn :: UInt
        newEndColumn
          | linesNew == 0 = fromIntegral $ fromIntegral startColumn + T.length t
          | otherwise = fromIntegral $ T.length $ T.takeWhileEnd (/= '\n') t
        newColumn :: UInt
        newColumn
          | line == endLine = fromIntegral $ (fromIntegral column + newEndColumn) - fromIntegral endColumn
          | otherwise = column
        newLine :: UInt
        newLine = fromIntegral $ fromIntegral line + lineDiff

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
        linesOld = fromIntegral endLine - fromIntegral startLine
        newEndLine :: UInt
        newEndLine = fromIntegral $ fromIntegral endLine + lineDiff
        newEndColumn :: UInt
        newEndColumn
          | linesNew == 0 = fromIntegral $ fromIntegral startColumn + T.length t
          | otherwise = fromIntegral $ T.length $ T.takeWhileEnd (/= '\n') t
        newColumn :: UInt
        newColumn
          | line == newEndLine = fromIntegral $ (fromIntegral column + fromIntegral endColumn) - newEndColumn
          | otherwise = column
        newLine :: UInt
        newLine = fromIntegral $ fromIntegral line - lineDiff

deltaFromDiff :: T.Text -> T.Text -> PositionDelta
deltaFromDiff (T.lines -> old) (T.lines -> new) =
    PositionDelta (lookupPos (fromIntegral lnew) o2nPrevs o2nNexts old2new) (lookupPos (fromIntegral lold) n2oPrevs n2oNexts new2old)
  where
    !lnew = length new
    !lold = length old

    diff = getDiff old new

    (V.fromList -> !old2new, V.fromList -> !new2old) = go diff 0 0

    -- Compute previous and next lines that mapped successfully
    !o2nPrevs = V.prescanl' f        (-1) old2new
    !o2nNexts = V.prescanr' (flip f) lnew old2new

    !n2oPrevs = V.prescanl' f        (-1) new2old
    !n2oNexts = V.prescanr' (flip f) lold new2old

    f :: Int -> Int -> Int
    f !a !b = if b == -1 then a else b

    lookupPos :: UInt -> V.Vector Int -> V.Vector Int -> V.Vector Int -> Position -> PositionResult Position
    lookupPos end prevs nexts xs (Position line col)
      | line >= fromIntegral (V.length xs) = PositionRange (Position end 0) (Position end 0)
      | otherwise           = case V.unsafeIndex xs (fromIntegral line) of
          -1 ->
            -- look for the previous and next lines that mapped successfully
            let !prev = 1 + V.unsafeIndex prevs (fromIntegral line)
                !next = V.unsafeIndex nexts (fromIntegral line)
              in PositionRange (Position (fromIntegral prev) 0) (Position (fromIntegral next) 0)
          line' -> PositionExact (Position (fromIntegral line') col)

    -- Construct a mapping between lines in the diff
    -- -1 for unsuccessful mapping
    go :: [Diff T.Text] -> Int -> Int -> ([Int], [Int])
    go [] _ _ = ([],[])
    go (Both _ _ : xs) !glold !glnew = bimap  (glnew :) (glold :) $ go xs (glold+1) (glnew+1)
    go (First _  : xs) !glold !glnew = first  (-1   :)          $ go xs (glold+1) glnew
    go (Second _ : xs) !glold !glnew = second          (-1   :) $ go xs glold     (glnew+1)
