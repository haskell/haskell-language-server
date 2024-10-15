{-# LANGUAGE GADTs #-}

module GotoImplementation where

data AAA = AAA
instance Num AAA where
aaa :: Num x => x
aaa  = 1
aaa1 :: AAA = aaa

class BBB a where
    bbb :: a -> a
instance BBB AAA where
    bbb = const AAA
bbbb :: AAA
bbbb = bbb AAA

data DDD a where
    DDD1 :: Int -> DDD Int
    DDD2 :: String -> DDD String
ddd :: DDD a -> a
ddd d = case d of
    DDD1 a -> a + a
    DDD2 a -> a ++ a

