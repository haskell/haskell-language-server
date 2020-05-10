module Test.Tasty.Expectations where

import Data.List
import Test.Tasty.HUnit

infix 1 ===, `shouldBe`, `shouldSatisfy`, `shouldNotBe`, `shouldNotSatisfy`
infix 1 `shouldContain`, `shouldMatchList`

(===) :: (Eq a, Show a) => a -> a -> Assertion
(===) = (@?=)

--was not ready to add a library until it was discussed, so
--converted these helper functions from https://github.com/hspec/hspec-expectations

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> Assertion
actual `shouldBe` expected = actual @?= expected

shouldNotBe :: (HasCallStack, Show a, Eq a) => a -> a -> Assertion
actual `shouldNotBe` notExpected = assertBool ("not expected: " ++ show actual) (actual /= notExpected)

shouldSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
v `shouldSatisfy` p = assertBool ("predicate failed on: " ++ show v) (p v)

shouldNotSatisfy :: (HasCallStack, Show a) => a -> (a -> Bool) -> Assertion
v `shouldNotSatisfy` p = assertBool ("predicate succeeded on: " ++ show v) ((not . p) v)

shouldContain :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Assertion
shouldContain = compareWith isInfixOf "does not contain"

shouldMatchList :: (HasCallStack, Show a, Eq a) => [a] -> [a] -> Assertion
xs `shouldMatchList` ys = maybe (return ()) assertFailure (matchList xs ys)

-- -----------------------------------------------------------------------

compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> String -> a -> a -> Assertion
compareWith comparator errorDesc result expected = assertBool errorMsg (comparator expected result)
    where
        errorMsg = show result ++ " " ++ errorDesc ++ " " ++ show expected

matchList :: (Show a, Eq a) => [a] -> [a] -> Maybe String
xs `matchList` ys
    | null extra && null missing = Nothing
    | otherwise = Just (err "")
    where
        extra   = xs \\ ys
        missing = ys \\ xs

        msgAndList msg zs = showString msg . showList zs . showString "\n"
        optMsgList msg zs = if null zs then id else msgAndList msg zs

        err :: ShowS
        err =
            showString "Actual list is not a permutation of expected list!\n"
            . msgAndList "  expected list contains:   " ys
            . msgAndList "  actual list contains:     " xs
            . optMsgList "  the missing elements are: " missing
            . optMsgList "  the extra elements are:   " extra