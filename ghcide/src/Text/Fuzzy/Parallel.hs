-- | Parallel versions of 'filter' and 'simpleFilter'
module Text.Fuzzy.Parallel
(
    filter,
    simpleFilter,
    -- reexports
    Fuzzy(..),
    match
) where

import           Control.Parallel.Strategies (Eval, evalTraversable,
                                              parListChunk, rseq, using)
import           Data.List                   (sortOn)
import           Data.Maybe                  (catMaybes)
import           Data.Monoid.Textual         (TextualMonoid)
import           Data.Ord                    (Down (Down))
import           Prelude                     hiding (filter)
import           Text.Fuzzy                  (Fuzzy (..), match)

-- | Evaluation that forces the 'score' field
forceScore :: TextualMonoid s => Fuzzy t s -> Eval(Fuzzy t s)
forceScore it@Fuzzy{score} = do
  score' <- rseq score
  return it{score = score'}

-- | The function to filter a list of values by fuzzy search on the text extracted from them.
--
-- >>> filter "ML" [("Standard ML", 1990),("OCaml",1996),("Scala",2003)] "<" ">" fst False
-- [Fuzzy {original = ("Standard ML",1990), rendered = "standard <m><l>", score = 4},Fuzzy {original = ("OCaml",1996), rendered = "oca<m><l>", score = 4}]
{-# INLINABLE filter #-}
filter :: (TextualMonoid s)
       => Int      -- ^ Chunk size. 1000 works well.
       -> s        -- ^ Pattern.
       -> [t]      -- ^ The list of values containing the text to search in.
       -> s        -- ^ The text to add before each match.
       -> s        -- ^ The text to add after each match.
       -> (t -> s) -- ^ The function to extract the text from the container.
       -> Bool     -- ^ Case sensitivity.
       -> [Fuzzy t s] -- ^ The list of results, sorted, highest score first.
filter chunkSize pattern ts pre post extract caseSen =
  sortOn (Down . score)
         (catMaybes
             (map (\t -> match pattern t pre post extract caseSen) ts
             `using`
             parListChunk chunkSize (evalTraversable forceScore)))

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
