
module OpenCloseTest (tests) where

import           Control.Applicative.Combinators
import           Control.Monad
import           Language.LSP.Protocol.Message
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Test.Tasty
import           TestUtils

tests :: TestTree
tests = testSession "open close" $ do
    doc <- createDoc "Testing.hs" "haskell" ""
    void (skipManyTill anyMessage $ message SMethod_WindowWorkDoneProgressCreate)
    waitForProgressBegin
    closeDoc doc
    waitForProgressDone
