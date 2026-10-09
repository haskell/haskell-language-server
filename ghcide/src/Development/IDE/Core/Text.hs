module Development.IDE.Core.Text
  ( takeLineRange
  , lineAt
  ) where

import           Data.Maybe                 (listToMaybe)
import           Data.Text                  (Text)
import           Data.Text.Utf16.Rope.Mixed (Rope)
import qualified Data.Text.Utf16.Rope.Mixed as Rope

-- | The lines of @rope@ over the 0-based inclusive line range @[from, to]@.
takeLineRange :: Word -> Word -> Rope -> [Text]
takeLineRange from to rope
  | to < from = []
  | otherwise = Rope.lines $ fst $ Rope.splitAtLine (to - from + 1) $ snd $ Rope.splitAtLine from rope

-- | The 0-based line @n@ of @rope@, if it has one.
lineAt :: Word -> Rope -> Maybe Text
lineAt n = listToMaybe . takeLineRange n n
