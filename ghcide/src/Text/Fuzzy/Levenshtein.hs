module Text.Fuzzy.Levenshtein where

import           Data.List           (sortOn)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Text.EditDistance
import           Text.Fuzzy.Parallel

-- | Sort the given list according to it's levenshtein distance relative to the
-- given string.
levenshteinScored :: Int -> Text -> [Text] -> [Scored Text]
levenshteinScored chunkSize needle haystack = do
  let levenshtein = levenshteinDistance $ defaultEditCosts {substitutionCosts=ConstantCost 2}
  sortOn score $
    matchPar chunkSize needle haystack id $
      \a b -> Just $ levenshtein (T.unpack a) (T.unpack b)
