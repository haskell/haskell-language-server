{-# LANGUAGE StandaloneDeriving #-}
module ClassImportedDeriving where
-- deriving method source span of Show occurrence
data Foo = Foo deriving (Show)

-- standalone deriving method not in the same position
-- deriving instance Eq Foo

-- a :: Foo -> Foo -> Bool
-- a = (==)
