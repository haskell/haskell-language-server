{-# LANGUAGE OverloadedStrings #-}
module Commands (tests) where

import Control.Lens hiding (List)
import Control.Monad.IO.Class
import qualified Data.Text as T
import Data.Char
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types as LSP
import Language.Haskell.LSP.Types.Lens as LSP
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils
import TastyUtils

tests :: TestTree
tests = testGroup "Commands" [
    testCase "are prefixed" $ runSession hieCommand fullCaps "test/testdata/" $ do
        ResponseMessage _ _ (Just res) Nothing <- initializeResponse
        let List cmds = res ^. LSP.capabilities . executeCommandProvider . _Just . commands
            f x = (T.length (T.takeWhile isNumber x) >= 1) && (T.count ":" x >= 2)
        liftIO $ do
            cmds `shouldSatisfy` all f
            cmds `shouldNotSatisfy` null
    ]
