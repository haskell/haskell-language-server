module IndentImportUse where

  import CrossFileDef (e)
  -- the whole module body is indented so the import must sit at the
  -- correct column
  f :: Int
  f = e (Just 7)
