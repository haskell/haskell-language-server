{-# LANGUAGE OverloadedStrings #-}

module Diagnostic (tests) where

import           Control.Lens               hiding (List)
import qualified Language.LSP.Protocol.Lens as L
import           Test.Hls
import           Test.Hls.Command

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "diagnostics providers" [ warningTests ]


warningTests :: TestTree
warningTests = testGroup  "Warnings are warnings" [
    testCase "Overrides -Werror" $
        runSession hlsCommand fullCaps "test/testdata/wErrorTest" $ do
            doc <- openDoc "src/WError.hs" "haskell"
            [diag] <- waitForDiagnosticsFrom doc
            liftIO $ diag ^. L.severity @?= Just DiagnosticSeverity_Warning
    ]

