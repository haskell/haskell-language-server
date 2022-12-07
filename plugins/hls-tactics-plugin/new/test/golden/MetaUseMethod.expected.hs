{-# LANGUAGE MultiParamTypeClasses #-}

class Test where
  test :: Int

instance Test where
  test = 10


resolve :: Int
resolve = test

