-- | Parallel versions of 'filter' and 'simpleFilter'
module Text.Fuzzy.Parallel
(   filter,
    simpleFilter,
    Scored(..),
    -- reexports
    Fuzzy,
) where

import           Control.Monad.ST            (runST)
import           Control.Parallel.Strategies (Eval, Strategy, evalTraversable,
                                              parTraversable, rseq, using)
import           Data.Monoid.Textual         (TextualMonoid)
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V
-- need to use a stable sort
import           Data.Bifunctor              (second)
import           Data.Char                   (toLower)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Monoid.Textual         as T
import           Prelude                     hiding (filter)
import           Text.Fuzzy                  (Fuzzy (..))

data Scored a = Scored {score_ :: !Int, original:: !a}
  deriving (Functor,Show)

-- | Returns the rendered output and the
-- matching score for a pattern and a text.
-- Two examples are given below:
--
-- >>> match "fnt" "infinite" "" "" id True
-- Just ("infinite",3)
--
-- >>> match "hsk" ("Haskell",1995) "<" ">" fst False
-- Just ("<h>a<s><k>ell",5)
--
{-# INLINABLE match #-}

match :: (T.TextualMonoid s)
      => s        -- ^ Pattern in lowercase except for first character
      -> t        -- ^ The value containing the text to search in.
      -> s        -- ^ The text to add before each match.
      -> s        -- ^ The text to add after each match.
      -> (t -> s) -- ^ The function to extract the text from the container.
      -> Maybe (Fuzzy t s) -- ^ The original value, rendered string and score.
match pattern t pre post extract =
    if null pat then Just (Fuzzy t result totalScore) else Nothing
  where
    null :: (T.TextualMonoid s) => s -> Bool
    null = not . T.any (const True)

    s = extract t
    (totalScore, _currScore, result, pat, _) =
      T.foldl'
        undefined
        (\(tot, cur, res, pat, isFirst) c ->
            case T.splitCharacterPrefix pat of
              Nothing -> (tot, 0, res <> T.singleton c, pat, isFirst)
              Just (x, xs) ->
                -- the case of the first character has to match
                -- otherwise use lower case since the pattern is assumed lower
                let !c' = if isFirst then c else toLower c in
                if x == c' then
                  let cur' = cur * 2 + 1 in
                  (tot + cur', cur', res <> pre <> T.singleton c <> post, xs, False)
                else (tot, 0, res <> T.singleton c, pat, isFirst)
        ) ( 0
          , 1 -- matching at the start gives a bonus (cur = 1)
          , mempty, pattern, True) s

-- | The function to filter a list of values by fuzzy search on the text extracted from them.
filter :: (TextualMonoid s)
       => Int      -- ^ Chunk size. 1000 works well.
       -> Int      -- ^ Max. number of results wanted
       -> s        -- ^ Pattern.
       -> [t]      -- ^ The list of values containing the text to search in.
       -> s        -- ^ The text to add before each match.
       -> s        -- ^ The text to add after each match.
       -> (t -> s) -- ^ The function to extract the text from the container.
       -> [Scored t] -- ^ The list of results, sorted, highest score first.
filter chunkSize maxRes pattern ts pre post extract = runST $ do
  let v = V.mapMaybe id
             (V.map (\t -> match pattern' t pre post extract) (V.fromList ts)
             `using`
             parVectorChunk chunkSize (evalTraversable forceScore))
      perfectScore = score $ fromMaybe (error $ T.toString undefined pattern) $
        match pattern' pattern' "" "" id
  return $ partialSortByAscScore maxRes perfectScore v
  where
      -- Preserve case for the first character, make all others lowercase
      pattern' = case T.splitCharacterPrefix pattern of
          Just (c, rest) -> T.singleton c <> T.map toLower rest
          _              -> pattern

-- | Return all elements of the list that have a fuzzy
-- match against the pattern. Runs with default settings where
-- nothing is added around the matches, as case insensitive.
--
-- >>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
-- ["vim","virtual machine"]
{-# INLINABLE simpleFilter #-}
simpleFilter :: (TextualMonoid s)
             => Int -- ^ Chunk size. 1000 works well.
             -> Int -- ^ Max. number of results wanted
             -> s   -- ^ Pattern to look for.
             -> [s] -- ^ List of texts to check.
             -> [Scored s] -- ^ The ones that match.
simpleFilter chunk maxRes pattern xs =
  filter chunk maxRes pattern xs mempty mempty id

--------------------------------------------------------------------------------

-- | Evaluation that forces the 'score' field
forceScore :: TextualMonoid s => Fuzzy t s -> Eval(Fuzzy t s)
forceScore it@Fuzzy{score} = do
  score' <- rseq score
  return it{score = score'}

--------------------------------------------------------------------------------

-- | Divides a vector in chunks, applies the strategy in parallel to each chunk.
parVectorChunk :: Int -> Strategy a -> Vector a -> Eval (Vector a)
parVectorChunk chunkSize st v =
    V.concat <$> parTraversable (evalTraversable st) (chunkVector chunkSize v)

-- >>> chunkVector 3 (V.fromList [0..10])
-- >>> chunkVector 3 (V.fromList [0..11])
-- >>> chunkVector 3 (V.fromList [0..12])
-- [[0,1,2],[3,4,5],[6,7,8],[9,10]]
-- [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]
-- [[0,1,2],[3,4,5],[6,7,8],[9,10,11],[12]]
chunkVector :: Int -> Vector a -> [Vector a]
chunkVector chunkSize v = do
    let indices = chunkIndices chunkSize (0,V.length v)
    [V.slice l (h-l) v | (l,h) <- indices]

-- >>> chunkIndices 3 (0,9)
-- >>> chunkIndices 3 (0,10)
-- >>> chunkIndices 3 (0,11)
-- [(0,2),(3,5),(6,8)]
-- [(0,2),(3,5),(6,8),(9,9)]
-- [(0,2),(3,5),(6,8),(9,10)]
chunkIndices :: Int -> (Int,Int) -> [(Int,Int)]
chunkIndices chunkSize (from,to) =
  map (second pred) $
  pairwise $
  [from, from+chunkSize .. to-1] ++ [to]

pairwise :: [a] -> [(a,a)]
pairwise []       = []
pairwise [_]      = []
pairwise (x:y:xs) = (x,y) : pairwise (y:xs)

-- | A stable partial sort ascending by score. O(N) best case, O(wanted*N) worst case
partialSortByAscScore :: TextualMonoid s
            => Int  -- ^ Number of items needed
            -> Int  -- ^ Value of a perfect score
            -> Vector (Fuzzy t s)
            -> [Scored t]
partialSortByAscScore wantedCount perfectScore v = loop 0 (SortState minBound perfectScore 0) [] where
  l = V.length v
  loop index st@SortState{..} acc
    | foundCount == wantedCount = reverse acc
    | index == l
-- ProgressCancelledException
    = if bestScoreSeen < scoreWanted
        then loop 0 st{scoreWanted = bestScoreSeen, bestScoreSeen = minBound} acc
        else reverse acc
    | otherwise =
      case v!index of
        x | score x == scoreWanted
          -> loop (index+1) st{foundCount = foundCount+1} (toScored x:acc)
          | score x < scoreWanted && score x > bestScoreSeen
          -> loop (index+1) st{bestScoreSeen = score x} acc
          | otherwise
          -> loop (index+1) st acc

toScored :: TextualMonoid s => Fuzzy t s -> Scored t
toScored Fuzzy{..} = Scored score original

data SortState a = SortState
  { bestScoreSeen :: !Int
  , scoreWanted   :: !Int
  , foundCount    :: !Int
  }
  deriving Show
