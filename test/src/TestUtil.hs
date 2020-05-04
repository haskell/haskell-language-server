module TestUtil where

import Test.Tasty.HUnit ( (@?=), assertBool, Assertion)

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)
infixl 1 ===

assertTrue :: Assertion
assertTrue = assertBool "Success" True

assertFalse :: String -> Assertion
assertFalse msg = assertBool msg False