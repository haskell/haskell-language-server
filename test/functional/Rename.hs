{-# LANGUAGE OverloadedStrings #-}
module Rename (tests) where

import           Control.Monad.IO.Class     (liftIO)
import           Language.LSP.Test
import           Language.LSP.Types
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure (ignoreTestBecause)
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "rename" [
    ignoreTestBecause "no symbol renaming (yet!)" $
    testCase "works" $
        runSession hlsCommand fullCaps "test/testdata/rename" $ do
          doc <- openDoc "Rename.hs" "haskell"
          rename doc (Position 3 1) "baz" -- foo :: Int -> Int
          contents <- documentContents doc
          let expected =
                  "main = do\n\
                  \  x <- return $ baz 42\n\
                  \  return (baz x)\n\
                  \baz :: Int -> Int\n\
                  \baz x = x + 1\n\
                  \bar = (+ 1) . baz\n"
          liftIO $ contents @?= expected
    ]
