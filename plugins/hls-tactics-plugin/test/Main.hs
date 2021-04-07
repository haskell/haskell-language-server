module Main where

import qualified Spec
import Test.Hls
import Test.Tasty.Hspec

main :: IO ()
main = testSpecs Spec.spec >>= defaultTestRunner . testGroup "tactics"
