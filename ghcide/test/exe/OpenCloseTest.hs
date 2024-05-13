
module OpenCloseTest (tests) where

import           Control.Applicative.Combinators
import           Control.Monad
import           Language.LSP.Protocol.Message
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Config                          (testWithDummyPluginEmpty)
import           Test.Hls                        (waitForProgressBegin,
                                                  waitForProgressDone)
import           Test.Tasty

tests :: TestTree
tests = testWithDummyPluginEmpty "open close" $ do
    doc <- createDoc "Testing.hs" "haskell" ""
    void (skipManyTill anyMessage $ message SMethod_WindowWorkDoneProgressCreate)
    waitForProgressBegin
    closeDoc doc
    waitForProgressDone
