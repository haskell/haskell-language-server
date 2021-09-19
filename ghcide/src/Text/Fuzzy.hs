{-# LANGUAGE FlexibleContexts #-}

-- | Fuzzy string search in Haskell.
-- Uses 'TextualMonoid' to be able to run on different types of strings.
module Text.Fuzzy where

import           Prelude             hiding (filter)
import qualified Prelude             as P

import           Data.Char           (toLower)
import           Data.List           (sortOn)
import           Data.Maybe          (isJust, mapMaybe)
import           Data.Monoid         (mempty, (<>))
import           Data.Ord
import           Data.String
import           Data.Text           (Text)

import qualified Data.Monoid.Textual as T

-- | Included in the return type of @'match'@ and @'filter'@.
-- Contains the original value given, the rendered string
-- and the matching score.
data (T.TextualMonoid s) => Fuzzy t s =
  Fuzzy { original :: t
        , rendered :: s
        , score    :: Int
        } deriving (Show, Eq)

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
match :: (T.TextualMonoid s)
      => s        -- ^ Pattern.
      -> t        -- ^ The value containing the text to search in.
      -> s        -- ^ The text to add before each match.
      -> s        -- ^ The text to add after each match.
      -> (t -> s) -- ^ The function to extract the text from the container.
      -> Bool     -- ^ Case sensitivity.
      -> Maybe (Fuzzy t s) -- ^ The original value, rendered string and score.
match pattern t pre post extract caseSensitive =
    if null pat then Just (Fuzzy t result totalScore) else Nothing
  where
    null :: (T.TextualMonoid s) => s -> Bool
    null = not . T.any (const True)

    s = extract t
    (s', pattern') = let f = T.map toLower in
                     if caseSensitive then (s, pattern) else (f s, f pattern)

    (totalScore, currScore, result, pat) =
      T.foldl'
        undefined
        (\(tot, cur, res, pat) c ->
            case T.splitCharacterPrefix pat of
              Nothing -> (tot, 0, res <> T.singleton c, pat)
              Just (x, xs) ->
                if x == c then
                  let cur' = cur * 2 + 1 in
                  (tot + cur', cur', res <> pre <> T.singleton c <> post, xs)
                else (tot, 0, res <> T.singleton c, pat)
        ) (0, 0, mempty, pattern') s'

-- | The function to filter a list of values by fuzzy search on the text extracted from them.
--
-- >>> filter "ML" [("Standard ML", 1990),("OCaml",1996),("Scala",2003)] "<" ">" fst False
-- [Fuzzy {original = ("Standard ML",1990), rendered = "standard <m><l>", score = 4},Fuzzy {original = ("OCaml",1996), rendered = "oca<m><l>", score = 4}]
filter :: (T.TextualMonoid s)
       => s        -- ^ Pattern.
       -> [t]      -- ^ The list of values containing the text to search in.
       -> s        -- ^ The text to add before each match.
       -> s        -- ^ The text to add after each match.
       -> (t -> s) -- ^ The function to extract the text from the container.
       -> Bool     -- ^ Case sensitivity.
       -> [Fuzzy t s] -- ^ The list of results, sorted, highest score first.
filter pattern ts pre post extract caseSen =
  sortOn (Down . score)
         (mapMaybe (\t -> match pattern t pre post extract caseSen) ts)

filterText :: Text -> [Text] -> [Fuzzy Text Text]
filterText s t = filter s t "" "" id False

{-# SPECIALIZE simpleFilter :: Text -> [Text] -> [Fuzzy Text Text] #-}

-- | Return all elements of the list that have a fuzzy
-- match against the pattern. Runs with default settings where
-- nothing is added around the matches, as case insensitive.
--
-- >>> simpleFilter "vm" ["vim", "emacs", "virtual machine"]
-- ["vim","virtual machine"]
simpleFilter :: (T.TextualMonoid s)
             => s   -- ^ Pattern to look for.
             -> [s] -- ^ List of texts to check.
             -> [Fuzzy s s] -- ^ The ones that match.
simpleFilter pattern xs =
  filter pattern xs mempty mempty id False

-- | Returns false if the pattern and the text do not match at all.
-- Returns true otherwise.
--
-- >>> test "brd" "bread"
-- True
test :: (T.TextualMonoid s)
     => s -> s -> Bool
test p s = isJust (match p s mempty mempty id False)


{-# INLINABLE match #-}
{-# INLINABLE filter #-}
{-# INLINABLE simpleFilter #-}
