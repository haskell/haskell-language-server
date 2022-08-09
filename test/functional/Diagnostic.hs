{-# LANGUAGE OverloadedStrings #-}

module Diagnostic (tests) where

import           Control.Lens            hiding (List)
import qualified Language.LSP.Types.Lens as LSP
import           Test.Hls
import           Test.Hls.Command

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "diagnostics providers" [
        basicTests
        , warningTests
    ]

basicTests :: TestTree
basicTests = testGroup "Diagnostics work" [
    testCase "example plugin produces diagnostics" $
        runSession hlsCommandExamplePlugin fullCaps "test/testdata/diagnostics" $ do
            doc <- openDoc "Foo.hs" "haskell"
            diags <- waitForDiagnosticsFromSource doc "example2"
            reduceDiag <- liftIO $ inspectDiagnostic diags ["example2 diagnostic, hello world"]
            liftIO $ do
                length diags @?= 1
                reduceDiag ^. LSP.range @?= Range (Position 0 0) (Position 1 0)
                reduceDiag ^. LSP.severity @?= Just DsError
    ]

warningTests :: TestTree
warningTests = testGroup  "Warnings are warnings" [
    testCase "Overrides -Werror" $
        runSession hlsCommand fullCaps "test/testdata/wErrorTest" $ do
            doc <- openDoc "src/WError.hs" "haskell"
            [diag] <- waitForDiagnosticsFrom doc
            liftIO $ diag ^. LSP.severity @?= Just DsWarning
    ]

