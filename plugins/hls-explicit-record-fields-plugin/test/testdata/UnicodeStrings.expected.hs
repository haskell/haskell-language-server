{-# LANGUAGE Haskell2010 #-}

module UnicodeStrings where

data MyRec = MyRec
  { αβγa  :: String
  , count :: Int
  }

convertMe :: MyRec
convertMe = MyRec { αβγa = "αβγ", count = 42 }
