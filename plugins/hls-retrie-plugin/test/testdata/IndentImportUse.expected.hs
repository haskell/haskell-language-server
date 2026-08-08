module IndentImportUse where

  import CrossFileDef (e)
  import Data.Maybe (fromMaybe)
  -- the whole module body is indented so the import must sit at the
  -- correct column
  f :: Int
  f = fromMaybe 0 (Just 7)
