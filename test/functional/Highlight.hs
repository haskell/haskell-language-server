{-# LANGUAGE OverloadedStrings #-}
module Highlight (tests) where

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit
import Test.Hspec.Expectations

tests :: TestTree
tests = testGroup "highlight" [
    testCase "works" $ runSession hieCommand fullCaps "test/testdata" $ do
        doc <- openDoc "Highlight.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
        highlights <- getHighlights doc (Position 2 2)
        liftIO $ do
            let hls =
                    [ DocumentHighlight (mkRange 2 0 2 3) (Just HkWrite)
                    , DocumentHighlight (mkRange 4 22 4 25) (Just HkRead)
                    , DocumentHighlight (mkRange 3 6 3 9) (Just HkRead)
                    , DocumentHighlight (mkRange 1 0 1 3) (Just HkRead)]
            mapM_ (\x -> highlights `shouldContain` [x]) hls
    ]
    where
        mkRange sl sc el ec = Range (Position sl sc) (Position el ec)
