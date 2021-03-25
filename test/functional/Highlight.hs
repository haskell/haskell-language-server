{-# LANGUAGE OverloadedStrings #-}
module Highlight (tests) where

import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "highlight" [
    testCase "works" $ runSession (hlsCommand ++ " --test") fullCaps "test/testdata" $ do
        doc <- openDoc "Highlight.hs" "haskell"
        _ <- waitForDiagnosticsFrom doc
        highlights <- getHighlights doc (Position 2 2)
        liftIO $ do
            let hls =
                    [ DocumentHighlight (mkRange 2 0 2 3) (Just HkWrite)
                    , DocumentHighlight (mkRange 4 22 4 25) (Just HkRead)
                    , DocumentHighlight (mkRange 3 6 3 9) (Just HkRead)
                    , DocumentHighlight (mkRange 1 0 1 3) (Just HkRead)]
            mapM_ (\x -> x `elem` highlights @? "Contains highlight") hls
    ]
    where
        mkRange sl sc el ec = Range (Position sl sc) (Position el ec)
