{-# LANGUAGE GADTs, GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
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

ccc :: Show a => a -> String
ccc d = show d

newtype Q k = Q k
    deriving newtype (Eq, Show)
ddd :: (Show k, Eq k) => k -> String
ddd k = if Q k == Q k then show k else ""
ddd1 = ddd (Q 0)

data GadtTest a where
    GadtTest :: Int -> GadtTest Int
printUsingEvidence :: Show a => GadtTest a -> String
printUsingEvidence (GadtTest i) = show i
