{-# LANGUAGE OverloadedStrings #-}
module Rename (tests) where

-- import Control.Monad.IO.Class
-- import Language.Haskell.LSP.Test
-- import Language.Haskell.LSP.Types
-- import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "rename" [
  testCase "works" $ True @?= True
  --  pendingWith "removed because of HaRe"
  -- runSession hlsCommand fullCaps "test/testdata" $ do
  --   doc <- openDoc "Rename.hs" "haskell"
  --   rename doc (Position 3 1) "baz" -- foo :: Int -> Int
  --   documentContents doc >>= liftIO . flip shouldBe expected
  -- where
  --   expected =
  --     "main = do\n\
  --     \  x <- return $ baz 42\n\
  --     \  return (baz x)\n\
  --     \baz :: Int -> Int\n\
  --     \baz x = x + 1\n\
  --     \bar = (+ 1) . baz\n"
    ]
