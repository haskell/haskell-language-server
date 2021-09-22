-- | Parallel versions of 'filter' and 'simpleFilter'
module Text.Fuzzy.Parallel
(   filter,
    simpleFilter,
    -- reexports
    Fuzzy(..),
    match
) where

import           Control.Monad.ST            (runST)
import           Control.Parallel.Strategies (Eval, Strategy, evalTraversable,
                                              parTraversable, rseq, using)
import           Data.Function               (on)
import           Data.Monoid.Textual         (TextualMonoid)
import           Data.Ord                    (Down (Down))
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V
-- need to use a stable sort
import           Data.Bifunctor              (second)
import qualified Data.Vector.Algorithms.Tim  as VA
import           Prelude                     hiding (filter)
import           Text.Fuzzy                  (Fuzzy (..), match)

-- | The function to filter a list of values by fuzzy search on the text extracted from them.
--
-- >>> length $ filter 1000 200 "ML" (concat $ replicate 10000 [("Standard ML", 1990),("OCaml",1996),("Scala",2003)]) "<" ">" fst False
-- 200
filter :: (TextualMonoid s)
       => Int      -- ^ Chunk size. 1000 works well.
       -> s        -- ^ Pattern.
       -> [t]      -- ^ The list of values containing the text to search in.
       -> s        -- ^ The text to add before each match.
       -> s        -- ^ The text to add after each match.
       -> (t -> s) -- ^ The function to extract the text from the container.
       -> Bool     -- ^ Case sensitivity.
       -> [Fuzzy t s] -- ^ The list of results, sorted, highest score first.
filter chunkSize pattern ts pre post extract caseSen = runST $ do
  let v = (V.mapMaybe id
             (V.map (\t -> match pattern t pre post extract caseSen) (V.fromList ts)
             `using`
             parVectorChunk chunkSize (evalTraversable forceScore)))
  v' <- V.unsafeThaw v
  VA.sortBy (compare `on` (Down . score)) v'
  v'' <- V.unsafeFreeze v'
  return $ V.toList v''

-- | Return all elements of the list that have a fuzzy
-- match against the pattern. Runs with default settings where
-- nothing is added around the matches, as case insensitive.
--
-- >>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
-- ["vim","virtual machine"]
{-# INLINABLE simpleFilter #-}
simpleFilter :: (TextualMonoid s)
             => Int -- ^ Chunk size. 1000 works well.
             -> s   -- ^ Pattern to look for.
             -> [s] -- ^ List of texts to check.
             -> [s] -- ^ The ones that match.
simpleFilter chunk pattern xs =
  map original $ filter chunk pattern xs mempty mempty id False

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
    let indices = chunkIndices chunkSize (0,l)
        l = V.length v
    [V.fromListN (h-l+1) [v ! j | j <- [l .. h]]
            | (l,h) <- indices]

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
