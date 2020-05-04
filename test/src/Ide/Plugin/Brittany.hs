module Ide.Plugin.Brittany where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Brittany" [
    someTruth
    ]

someTruth :: TestTree
someTruth = testCase "List comparison (different length)" $
      compare a b @?= GT
     where
         a :: [Int]
         a = [1,2,3]
         b :: [Int]
         b = [1,2]