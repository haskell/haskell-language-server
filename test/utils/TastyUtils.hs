module TastyUtils (
      (===)
    , shouldBe
    , shouldNotBe
    , shouldSatisfy
    , shouldNotSatisfy
) where

import Test.Tasty.HUnit

infix 1 ===, `shouldBe`, `shouldNotBe`, `shouldSatisfy`, `shouldNotSatisfy`

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = actual @?= expected

shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> Assertion
actual `shouldNotBe` notExpected = assertBool ("not expected: " ++ show actual) (actual /= notExpected)

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
v `shouldSatisfy` p = assertBool ("predicate failed on: " ++ show v) (p v)

shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
v `shouldNotSatisfy` p = assertBool ("predicate succeeded on: " ++ show v) ((not . p) v)