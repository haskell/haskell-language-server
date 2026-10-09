{-# LANGUAGE OverloadedStrings #-}

module FunctionalBadProject (tests) where

import           Control.Lens
import qualified Data.Text                  as T
import qualified Language.LSP.Protocol.Lens as L
import           Test.Hls
import           Test.Hls.Command


tests :: TestTree
tests = testGroup "behaviour on malformed projects"
    [ testCase "Missing module diagnostic" $ do
        runSession hlsLspCommand fullLatestClientCaps "test/testdata/missingModuleTest/missingModule/" $ do
            doc <- openDoc "src/MyLib.hs" "haskell"
            [diag] <- waitForDiagnosticsFrom doc
            liftIO $ assertBool "missing module name" $ "MyLib" `T.isInfixOf` (diag ^. L.message)
            liftIO $ assertBool "module missing context" $ "may not be listed" `T.isInfixOf` (diag ^. L.message)
    , testCase "Missing module diagnostic - no matching prefix" $ do
        runSession hlsLspCommand fullLatestClientCaps "test/testdata/missingModuleTest/noPrefixMatch/" $ do
            doc <- openDoc "app/Other.hs" "haskell"
            [diag] <- waitForDiagnosticsFrom doc
            liftIO $ assertBool "missing module name" $
                "Other" `T.isInfixOf` (diag ^. L.message)
            liftIO $ assertBool  "hie-bios message" $
                "Cabal" `T.isInfixOf` (diag ^. L.message)
    ]
