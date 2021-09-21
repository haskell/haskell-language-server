-- | Parallel versions of 'filter' and 'simpleFilter'
module Text.Fuzzy.Parallel
(
    filter,
    simpleFilter,
    -- reexports
    Fuzzy(..),
    match
) where

import           Control.Monad.ST            (runST)
import           Control.Parallel.Strategies (Eval, Strategy, evalTraversable,
                                              parListChunk, parTraversable,
                                              rseq, using)
import           Data.Function               (on)
import           Data.List                   (sortOn)
import           Data.Maybe                  (catMaybes)
import           Data.Monoid.Textual         (TextualMonoid)
import           Data.Ord                    (Down (Down))
import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V
import qualified Data.Vector.Algorithms.Heap as VA
import           Prelude                     hiding (filter)
import           Text.Fuzzy                  (Fuzzy (..), match)

-- | The function to filter a list of values by fuzzy search on the text extracted from them.
--
-- >>> length $ filter 1000 200 "ML" (concat $ replicate 10000 [("Standard ML", 1990),("OCaml",1996),("Scala",2003)]) "<" ">" fst False
-- 200
filter :: (TextualMonoid s)
       => Int      -- ^ Chunk size. 1000 works well.
       -> Int      -- ^ Max results
       -> s        -- ^ Pattern.
       -> [t]      -- ^ The list of values containing the text to search in.
       -> s        -- ^ The text to add before each match.
       -> s        -- ^ The text to add after each match.
       -> (t -> s) -- ^ The function to extract the text from the container.
       -> Bool     -- ^ Case sensitivity.
       -> [Fuzzy t s] -- ^ The list of results, sorted, highest score first.
filter chunkSize maxRes pattern ts pre post extract caseSen = runST $ do
  let v = (V.catMaybes
             (V.map (\t -> match pattern t pre post extract caseSen) (V.fromList ts)
             `using`
             parVectorChunk chunkSize (evalTraversable forceScore)))
  v' <- V.unsafeThaw v
  VA.partialSortBy (compare `on` (Down . score)) v' maxRes
  v'' <- V.unsafeFreeze v'
  return $ take maxRes $ V.toList v''

-- | Return all elements of the list that have a fuzzy
-- match against the pattern. Runs with default settings where
-- nothing is added around the matches, as case insensitive.
--
-- >>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
-- ["vim","virtual machine"]
{-# INLINABLE simpleFilter #-}
simpleFilter :: (TextualMonoid s)
             => Int -- ^ Chunk size. 1000 works well.
             -> Int -- ^ Max results
             -> s   -- ^ Pattern to look for.
             -> [s] -- ^ List of texts to check.
             -> [s] -- ^ The ones that match.
simpleFilter chunk maxRes pattern xs =
  map original $ filter chunk maxRes pattern xs mempty mempty id False

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
    let indices = pairwise $ [0, chunkSize .. l-1] ++ [l]
        l = V.length v
    [V.fromListN (h-l) [v ! j | j <- [l .. h-1]]
            | (l,h) <- indices]

pairwise :: [a] -> [(a,a)]
pairwise []       = []
pairwise [_]      = []
pairwise (x:y:xs) = (x,y) : pairwise (y:xs)

-- | Evaluation that forces the 'score' field
forceScore :: TextualMonoid s => Fuzzy t s -> Eval(Fuzzy t s)
forceScore it@Fuzzy{score} = do
  score' <- rseq score
  return it{score = score'}
