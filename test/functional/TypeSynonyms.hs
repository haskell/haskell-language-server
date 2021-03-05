{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module TypeSynonyms where

import           Control.Applicative.Combinators
import           Control.Monad.IO.Class
import           Data.List                       (find)
import           Data.Text                       (Text)
import qualified Data.Text.IO                    as T
import           Language.LSP.Test
import           Language.LSP.Types
import           System.FilePath
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "typeSynonyms"
    [ goldenTest "Basic.hs" 5 6
    ]

goldenTest :: FilePath -> Int -> Int -> TestTree
goldenTest input line col =
    testCase input $ do
        runSession hlsCommand fullCaps spliceTestPath $ do
            doc <- openDoc input "haskell"
            _ <- waitForDiagnostics
            actions <- getCodeActions doc $ pointRange line col
            case find ((Just "Replace with type synonym" ==) . codeActionTitle) actions of
                Just (InR action) -> do
                    executeCodeAction action
                    _resp <- skipManyTill anyMessage (message SWorkspaceApplyEdit)
                    edited <- documentContents doc
                    let expected_name = spliceTestPath </> input <.> "expected"
                    expected <- liftIO $ T.readFile expected_name
                    liftIO $ edited @?= expected
                _ -> liftIO $ assertFailure "No CodeAction detected"


spliceTestPath :: FilePath
spliceTestPath = "test/testdata/typeSynonyms"

pointRange :: Int -> Int -> Range
pointRange
    (subtract 1 -> line)
    (subtract 1 -> col) =
        Range (Position line col) (Position line $ col + 1)

-- | Get the title of a code action.
codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle InL{}                               = Nothing
codeActionTitle (InR(CodeAction title _ _ _ _ _ _)) = Just title
