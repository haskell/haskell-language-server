{- | Won't panic on UNPACKs -}
module TUNPACK (THStatus(..)) where

type ByteString = String
type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

-- >>> "Yay! UNPACK pragma didn't do bad things!"
-- "Yay! UNPACK pragma didn't do bad things!"
