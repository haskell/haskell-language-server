module Development.IDE.Core.Text
  ( takeLineRange
  , lineAt
  ) where

import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Text                  (Text)
import           Data.Text.Rope             (posLine)
import           Data.Text.Utf16.Rope.Mixed (Rope)
import qualified Data.Text.Utf16.Rope.Mixed as Rope

-- | The lines of @rope@ over the 0-based inclusive line range @[from, to]@.
takeLineRange :: Word -> Word -> Rope -> [Text]
takeLineRange from to rope
  | to < from = []
  | otherwise = Rope.lines $ fst $ Rope.splitAtLine (to - from + 1) $ snd $ Rope.splitAtLine from rope

-- | The 0-based line @n@ of @rope@, if the document has one. Unlike
-- 'Rope.lines', LSP counts the empty line after a trailing newline, as well as
-- the single empty line in an empty document.
lineAt :: Word -> Rope -> Maybe Text
lineAt n rope
  | n > posLine (Rope.charLengthAsPosition rope) = Nothing
  | otherwise = Just $ fromMaybe mempty $ listToMaybe $ takeLineRange n n rope
