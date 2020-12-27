-- | Position indexed streams of characters
module Development.IDE.Plugin.CodeAction.PositionIndexed
  ( PositionIndexed
  , PositionIndexedString
  , indexedByPosition
  , indexedByPositionStartingFrom
  , extendAllToIncludeCommaIfPossible
  , extendToIncludePreviousNewlineIfPossible
  , mergeRanges
  )
where

import           Data.Char
import           Data.List
import           Language.Haskell.LSP.Types

type PositionIndexed a = [(Position, a)]

type PositionIndexedString = PositionIndexed Char

-- | Add position indexing to a String.
--
--   > indexedByPositionStartingFrom (0,0) "hey\n ho" ≡
--   >   [ ((0,0),'h')
--   >   , ((0,1),'e')
--   >   , ((0,2),'y')
--   >   , ((0,3),'\n')
--   >   , ((1,0),' ')
--   >   , ((1,1),'h')
--   >   , ((1,2),'o')
--   >   ]
indexedByPositionStartingFrom :: Position -> String -> PositionIndexedString
indexedByPositionStartingFrom initialPos = unfoldr f . (initialPos, ) where
  f (_, []) = Nothing
  f (p@(Position l _), '\n' : rest) =
    Just ((p, '\n'), (Position (l + 1) 0, rest))
  f (p@(Position l c), x : rest) = Just ((p, x), (Position l (c + 1), rest))

-- | Add position indexing to a String.
--
--   > indexedByPosition = indexedByPositionStartingFrom (Position 0 0)
indexedByPosition :: String -> PositionIndexedString
indexedByPosition = indexedByPositionStartingFrom (Position 0 0)

-- | Returns a tuple (before, contents, after) if the range is present.
--   The range is present only if both its start and end positions are present
unconsRange
  :: Range
  -> PositionIndexed a
  -> Maybe (PositionIndexed a, PositionIndexed a, PositionIndexed a)
unconsRange Range {..} indexedString
  | (before, rest@(_ : _)) <- span ((/= _start) . fst) indexedString
  , (mid, after@(_ : _)) <- span ((/= _end) . fst) rest
  = Just (before, mid, after)
  | otherwise
  = Nothing

-- | Strips out all the positions included in the range.
--   Returns 'Nothing' if the start or end of the range are not included in the input.
stripRange :: Range -> PositionIndexed a -> Maybe (PositionIndexed a)
stripRange r s = case unconsRange r s of
  Just (b, _, a) -> Just (b ++ a)
  Nothing        -> Nothing

-- | Returns the smallest possible set of disjoint ranges that is equivalent to the input.
--   Assumes input ranges are sorted on the start positions.
mergeRanges :: [Range] -> [Range]
mergeRanges (r : r' : rest)
  |
    -- r' is contained in r
    _end r > _end r'   = mergeRanges (r : rest)
  |
    -- r and r' are overlapping
    _end r > _start r' = mergeRanges (r { _end = _end r' } : rest)

  | otherwise          = r : mergeRanges (r' : rest)
mergeRanges other = other

-- | Returns a sorted list of ranges with extended selections including preceding or trailing commas
--
-- @
--   a, |b|,  c  ===> a|, b|,  c
--   a,  b,  |c| ===> a,  b|,  c|
--   a, |b|, |c| ===> a|, b||, c|
-- @
extendAllToIncludeCommaIfPossible :: PositionIndexedString -> [Range] -> [Range]
extendAllToIncludeCommaIfPossible indexedString =
  mergeRanges . go indexedString . sortOn _start
 where
  go _ [] = []
  go input (r : rr)
    | r' : _ <- extendToIncludeCommaIfPossible input r
    , Just input' <- stripRange r' input
    = r' : go input' rr
    | otherwise
    = go input rr

extendToIncludeCommaIfPossible :: PositionIndexedString -> Range -> [Range]
extendToIncludeCommaIfPossible indexedString range
  | Just (before, _, after) <- unconsRange range indexedString
  , after' <- dropWhile (isSpace . snd) after
  , before' <- dropWhile (isSpace . snd) (reverse before)
  =
    -- a, |b|, c ===> a|, b|, c
    [ range { _start = start' } | (start', ',') : _ <- [before'] ]
    ++
    -- a, |b|, c ===> a, |b, |c
    [ range { _end = end' }
    | (_, ',') : rest <- [after']
    , (end', _) : _ <- pure $ dropWhile (isSpace . snd) rest
    ]
  | otherwise
  = [range]

extendToIncludePreviousNewlineIfPossible :: PositionIndexedString -> Range -> Range
extendToIncludePreviousNewlineIfPossible indexedString range
  | Just (before, _, _) <- unconsRange range indexedString
  , maybeFirstSpacePos <- lastSpacePos $ reverse before
  = case maybeFirstSpacePos of
      Nothing -> range
      Just pos -> range { _start = pos }
  | otherwise = range
  where
    lastSpacePos :: PositionIndexedString -> Maybe Position
    lastSpacePos [] = Nothing
    lastSpacePos ((pos, c):xs) =
      if not $ isSpace c
      then Nothing -- didn't find any space
      else case xs of
              (y:ys) | isSpace $ snd y -> lastSpacePos (y:ys)
              _ -> Just pos