-- | Parallel versions of 'filter' and 'simpleFilter'

module Text.Fuzzy.Parallel
(   filter, filter',
    simpleFilter, simpleFilter',
    match, defChunkSize, defMaxResults,
    Scored(..)
) where

import           Control.Parallel.Strategies (evalList, parList, rseq, using)
import           Data.Bits                   ((.|.))
import           Data.Maybe                  (fromMaybe, mapMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.Array             as TA
import qualified Data.Text.Internal          as T
import           Prelude                     hiding (filter)

data Scored a = Scored {score :: !Int, original:: !a}
  deriving (Functor, Show)

-- | Returns the rendered output and the
-- matching score for a pattern and a text.
-- Two examples are given below:
--
-- >>> match "fnt" "infinite"
-- Just 3
--
-- >>> match "hsk" "Haskell"
-- Just 5
--
{-# INLINABLE match #-}
match :: T.Text    -- ^ Pattern in lowercase except for first character
      -> T.Text    -- ^ The text to search in.
      -> Maybe Int -- ^ The score
match (T.Text pArr pOff pLen) (T.Text sArr sOff sLen) = go 0 1 pOff sOff
  where
    pTotal = pOff + pLen
    sDelta = sOff + sLen - pTotal

    go !totalScore !currScore !currPOff !currSOff
      -- If pattern has been matched in full
      | currPOff >= pTotal
      = Just totalScore
      -- If there is not enough left to match the rest of the pattern, equivalent to
      -- (sOff + sLen - currSOff) < (pOff + pLen - currPOff)
      | currSOff > currPOff + sDelta
      = Nothing
      -- This is slightly broken for non-ASCII:
      -- 1. If code units, consisting a single pattern code point, are found as parts
      --    of different code points, it counts as a match. Unless you use a ton of emojis
      --    as identifiers, such false positives should not be be a big deal,
      --    and anyways HLS does not currently support such use cases, because it uses
      --    code point and UTF-16 code unit positions interchangeably.
      -- 2. Case conversions is not applied to non-ASCII code points, because one has
      --    to call T.toLower (not T.map toLower), reallocating the string in full, which
      --    is too much of performance penalty for fuzzy search. Again, anyway HLS does not
      --    attempt to do justice to Unicode: proper Unicode text matching requires
      --    `unicode-transforms` and friends.
      -- Altogether we sacrifice correctness for the sake of performance, which
      -- is a right trade-off for fuzzy search.
      | pByte <- TA.unsafeIndex pArr currPOff
      , sByte <- TA.unsafeIndex sArr currSOff
      -- First byte (currPOff == pOff) should match exactly, otherwise - up to case.
      , pByte == sByte || (currPOff /= pOff && pByte == toLowerAscii sByte)
      = let curr = currScore * 2 + 1 in
        go (totalScore + curr) curr (currPOff + 1) (currSOff + 1)
      | otherwise
      = go totalScore 0 currPOff (currSOff + 1)

    toLowerAscii w = if (w - 65) < 26 then w .|. 0x20 else w

-- | Sensible default value for chunk size to use when calling simple filter.
defChunkSize :: Int
defChunkSize = 1000

-- | Sensible default value for the number of max results to use when calling simple filter.
defMaxResults :: Int
defMaxResults = 10

-- | Return all elements of the list that have a fuzzy
-- match against the pattern. Runs with default settings where
-- nothing is added around the matches, as case insensitive.
--
-- >>> simpleFilter 1000 10 "vm" ["vim", "emacs", "virtual machine"]
-- [Scored {score = 4, original = "vim"},Scored {score = 4, original = "virtual machine"}]
{-# INLINABLE simpleFilter #-}
simpleFilter :: Int      -- ^ Chunk size. 1000 works well.
             -> Int      -- ^ Max. number of results wanted
             -> T.Text   -- ^ Pattern to look for.
             -> [T.Text] -- ^ List of texts to check.
             -> [Scored T.Text] -- ^ The ones that match.
simpleFilter chunk maxRes pat xs = filter chunk maxRes pat xs id


-- | The function to filter a list of values by fuzzy search on the text extracted from them,
-- using a custom matching function which determines how close words are.
filter' :: Int           -- ^ Chunk size. 1000 works well.
       -> Int           -- ^ Max. number of results wanted
       -> T.Text        -- ^ Pattern.
       -> [t]           -- ^ The list of values containing the text to search in.
       -> (t -> T.Text) -- ^ The function to extract the text from the container.
       -> (T.Text -> T.Text -> Maybe Int)
       -- ^ Custom scoring function to use for calculating how close words are
       -- When the function returns Nothing, this means the values are incomparable.
       -> [Scored t]    -- ^ The list of results, sorted, highest score first.
filter' chunkSize maxRes pat ts extract match' = partialSortByAscScore maxRes perfectScore (concat vss)
  where
      -- Preserve case for the first character, make all others lowercase
      pat' = case T.uncons pat of
        Just (c, rest) -> T.cons c (T.toLower rest)
        _              -> pat
      vss = map (mapMaybe (\t -> flip Scored t <$> match' pat' (extract t))) (chunkList chunkSize ts)
        `using` parList (evalList rseq)
      perfectScore = fromMaybe (error $ T.unpack pat) $ match' pat' pat'

-- | The function to filter a list of values by fuzzy search on the text extracted from them,
-- using a custom matching function which determines how close words are.
filter :: Int           -- ^ Chunk size. 1000 works well.
       -> Int           -- ^ Max. number of results wanted
       -> T.Text        -- ^ Pattern.
       -> [t]           -- ^ The list of values containing the text to search in.
       -> (t -> T.Text) -- ^ The function to extract the text from the container.
       -> [Scored t]    -- ^ The list of results, sorted, highest score first.
filter chunkSize maxRes pat ts extract =
  filter' chunkSize maxRes pat ts extract match

-- | Return all elements of the list that have a fuzzy match against the pattern,
-- the closeness of the match is determined using the custom scoring match function that is passed.
-- Runs with default settings where nothing is added around the matches, as case insensitive.
{-# INLINABLE simpleFilter' #-}
simpleFilter' :: Int      -- ^ Chunk size. 1000 works well.
             -> Int      -- ^ Max. number of results wanted
             -> T.Text   -- ^ Pattern to look for.
             -> [T.Text] -- ^ List of texts to check.
             -> (T.Text -> T.Text -> Maybe Int)
             -- ^ Custom scoring function to use for calculating how close words are
             -> [Scored T.Text] -- ^ The ones that match.
simpleFilter' chunk maxRes pat xs match' =
  filter' chunk maxRes pat xs id match'
--------------------------------------------------------------------------------

chunkList :: Int -> [a] -> [[a]]
chunkList chunkSize = go
  where
    go [] = []
    go xs = ys : go zs
      where
        (ys, zs) = splitAt chunkSize xs

-- | A stable partial sort ascending by score. O(N) best case, O(wanted*N) worst case
partialSortByAscScore
            :: Int  -- ^ Number of items needed
            -> Int  -- ^ Value of a perfect score
            -> [Scored t]
            -> [Scored t]
partialSortByAscScore wantedCount perfectScore orig = loop orig (SortState minBound perfectScore 0) [] where
  loop [] st@SortState{..} acc
    | foundCount == wantedCount = reverse acc
    | otherwise = if bestScoreSeen < scoreWanted
        then loop orig st{scoreWanted = bestScoreSeen, bestScoreSeen = minBound} acc
        else reverse acc
  loop (x : xs) st@SortState{..} acc
    | foundCount == wantedCount = reverse acc
    | score x == scoreWanted
    = loop xs st{foundCount = foundCount+1} (x:acc)
    | score x < scoreWanted && score x > bestScoreSeen
    = loop xs st{bestScoreSeen = score x} acc
    | otherwise
    = loop xs st acc

data SortState a = SortState
  { bestScoreSeen :: !Int
  , scoreWanted   :: !Int
  , foundCount    :: !Int
  }
  deriving Show
