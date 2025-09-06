module Text.Fuzzy.Levenshtein where

import           Data.Function       (fix)
import           Data.List           (sortOn)
import           Data.MemoTrie
import qualified Data.Text           as T
import qualified Data.Text.Array     as T
import           Data.Text.Internal  (Text (..))
import           Text.Fuzzy.Parallel

-- | Same caveats apply w.r.t. ASCII as in 'Text.Fuzzy.Parallel'.
-- Might be worth optimizing this at some point, but it's good enoughᵗᵐ for now
levenshtein :: Text -> Text -> Int
levenshtein a b | T.null a = T.length b
levenshtein a b | T.null b = T.length a
levenshtein (Text aBuf aOff aLen) (Text bBuf bOff bLen) = do
  let aTot = aOff + aLen
      bTot = bOff + bLen
      go' _ (!aIx, !bIx) | aIx >= aTot || bIx >= bTot = max (aTot - aIx) (bTot - bIx)
      go' f (!aIx, !bIx) | T.unsafeIndex aBuf aIx == T.unsafeIndex bBuf bIx = f (aIx + 1, bIx + 1)
      go' f (!aIx, !bIx) =
        minimum
          [ 2 + f (aIx + 1, bIx + 1), -- Give substitutions a heavier cost, so multiple typos cost more
            1 + f (aIx + 1, bIx),
            1 + f (aIx, bIx + 1)
          ]
      go = fix (memo . go')
  go (aOff, bOff)

-- | Sort the given list according to it's levenshtein distance relative to the
-- given string.
levenshteinScored :: Int -> Text -> [Text] -> [Scored Text]
levenshteinScored chunkSize needle haystack =
  sortOn score $
    matchPar chunkSize needle haystack id $
      \a b -> Just $ levenshtein a b
