module TastyUtils (
    shouldSatisfy
   ,shouldNotSatisfy
) where

import Test.Tasty.HUnit

-- (===) :: (Eq a, Show a) => a -> a -> Assertion
-- (===) = (@?=)
-- infixl 1 ===

-- assertTrue :: Assertion
-- assertTrue = assertBool "Success" True

-- assertFalse :: String -> Assertion
-- assertFalse msg = assertBool msg False

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
v `shouldSatisfy` p = assertBool ("predicate failed on: " ++ show v) (p v)

shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
v `shouldNotSatisfy` p = assertBool ("predicate succeeded on: " ++ show v) ((not . p) v)