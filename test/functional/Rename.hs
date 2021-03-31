{-# LANGUAGE OverloadedStrings #-}
module Rename (tests) where

import           Test.Hls
import           Test.Hls.Command

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
