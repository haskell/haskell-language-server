-- | Position indexed streams of characters
module Development.IDE.Plugin.CodeAction.PositionIndexed
  ( PositionIndexed
  , PositionIndexedString
  , indexedByPosition
  , indexedByPositionStartingFrom
  , extendAllToIncludeCommaIfPossible
  , extendToIncludePreviousNewlineIfPossible
  , mergeRanges
  , extendForHaddock
  )
where

import           Data.Char
import           Data.List                   as List
import           Data.Map                    as Map
import           Language.LSP.Protocol.Types (Position (Position),
                                              Range (Range, _end, _start))
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
  | _end r > _end r' =
      mergeRanges (r : rest)

  | isAdjacent r r' =
      mergeRanges (r { _end = _end r' } : rest)

  | otherwise =
      r : mergeRanges (r' : rest)

mergeRanges other = other


isAdjacent :: Range -> Range -> Bool
isAdjacent r r' =
  let Position l1 c1 = _end r
      Position l2 c2 = _start r'
  in
    -- Same line overlap
    (l1 == l2 && c1 >= c2)
    ||
    -- Direct next line (no blank line in between)
    (l2 == l1 + 1 && c2 == 0)
-- | Returns a sorted list of ranges with extended selections including preceding or trailing commas
--
-- @
--   a, |b|,  c  ===> a|, b|,  c
--   a,  b,  |c| ===> a,  b|,  c|
--   a, |b|, |c| ===> a|, b||, c|
-- @
--
-- If 'acceptNoComma' is enabled, additional ranges are returned
--
-- @
--   |a|       ===> |a|
--   |a|,  |b| ===> |a,|  |b|
-- @
extendAllToIncludeCommaIfPossible :: Bool -> PositionIndexedString -> [Range] -> [Range]
extendAllToIncludeCommaIfPossible acceptNoComma indexedString =
  mergeRanges . go indexedString . sortOn _start
 where
  go _ [] = []
  go input (r : rr)
    | r' : _ <- extendToIncludeCommaIfPossible acceptNoComma input r
    , Just input' <- stripRange r' input
    = r' : go input' rr
    | otherwise
    = go input rr

extendToIncludeCommaIfPossible :: Bool -> PositionIndexedString -> Range -> [Range]
extendToIncludeCommaIfPossible acceptNoComma indexedString range
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
    ++
    ([range | acceptNoComma])
  | otherwise
  = [range]

extendToIncludePreviousNewlineIfPossible :: PositionIndexedString -> Range -> Range
extendToIncludePreviousNewlineIfPossible indexedString range
  | Just (before, _, _) <- unconsRange range indexedString
  , maybeFirstSpacePos <- lastSpacePos $ reverse before
  = case maybeFirstSpacePos of
      Nothing  -> range
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
              _                        -> Just pos

extendForHaddock :: PositionIndexedString -> Range -> Range
extendForHaddock indexedContent range =
  let Position startLineUInt _ = _start range
      startLine = fromIntegral startLineUInt :: Int

      lineMap :: Map Int String
      lineMap = Map.fromListWith (++)
                  [ (lineNum, [ch])
                  | (Position lineUInt _, ch) <- indexedContent
                  , let lineNum = fromIntegral lineUInt :: Int
                  ]

      getLine :: Int -> String
      getLine i =
        let raw = Map.findWithDefault "" i lineMap
        in  reverse raw


      linesToConsume = countHaddockLines startLine getLine :: Int

  in  if linesToConsume == 0
      then range
      else
        let newStart = Position (fromIntegral (startLine - linesToConsume)) 0
        in range { _start = newStart }

countHaddockLines :: Int -> (Int -> String) -> Int
countHaddockLines sigLine getLine =
  let prevLineIdx = sigLine - 1
  in  if prevLineIdx < 0
      then 0 else
        let stripped = (getLine prevLineIdx)
        in  if isBlankLine stripped
            then 0

            else if isHaddockBlockEnd stripped
            then case scanBlockUp (prevLineIdx - 1) of
              Just openerLine -> sigLine - openerLine
              Nothing         -> 0

            else if isLineHaddock stripped
            then sigLine - prevLineIdx

            else if isCommentLine stripped
            then case scanLineCommentBlockUp (prevLineIdx - 1) of
              Just haddockLine -> sigLine - haddockLine
              Nothing          -> 0

            else 0
  where
    scanBlockUp :: Int -> Maybe Int
    scanBlockUp idx
      | idx < 0   = Nothing
      | otherwise =
          let stripped = dropWhile isSpace (getLine idx)
          in  if isBlankLine stripped
              then scanBlockUp (idx - 1)
              else if isHaddockBlockStart stripped
              then Just idx
              else if isPlainBlockStart stripped
              then Nothing
              else scanBlockUp (idx - 1)

    scanLineCommentBlockUp :: Int -> Maybe Int
    scanLineCommentBlockUp idx
      | idx < 0   = Nothing
      | otherwise =
          let stripped = dropWhile isSpace (getLine idx)
          in  if isBlankLine stripped
              then Nothing
              else if isLineHaddock stripped
              then Just idx
              else if isCommentLine stripped
              then scanLineCommentBlockUp (idx - 1)
              else Nothing

isHaddockBlockStart :: String -> Bool
isHaddockBlockStart s = "{- |" `isPrefixOf` s || "{-|" `isPrefixOf` s

isPlainBlockStart :: String -> Bool
isPlainBlockStart s = "{-" `isPrefixOf` s && not (isHaddockBlockStart s)

isHaddockBlockEnd :: String -> Bool
isHaddockBlockEnd s = "-}" `isPrefixOf` s

isLineHaddock :: String -> Bool
isLineHaddock s = "-- |" `isPrefixOf` s || "--^" `isPrefixOf` s

isCommentLine :: String -> Bool
isCommentLine s = "--" `isPrefixOf` s

isBlankLine :: String -> Bool
isBlankLine = all isSpace
