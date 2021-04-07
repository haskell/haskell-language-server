{-# LANGUAGE OverloadedStrings #-}

module FunctionalLiquid (tests) where

import           Control.Lens            hiding (List)
import           Data.Aeson
import           Ide.Plugin.Config
import           Language.LSP.Types.Lens as LSP hiding (contents)
import           Test.Hls
import           Test.Hls.Command

-- ---------------------------------------------------------------------

tests :: TestTree
tests = testGroup "liquid haskell diagnostics" [
    ignoreTestBecause "no liquid haskell"
    $ testCase "liquid haskell generates diagnostics" $
        runSession hlsCommand codeActionSupportCaps "test/testdata" $ do
            doc <- openDoc "liquid/Evens.hs" "haskell"

            let config = def { liquidOn  = True, hlintOn = False }
            sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

            diags <- waitForDiagnosticsFromSource doc "liquid"
            d <- liftIO $ inspectDiagnostic diags ["Liquid Type Mismatch"]
            liftIO $ do
                length diags @?= 1
                d ^. range @?= Range (Position 8 0) (Position 8 11)
                d ^. severity @?= Just DsError
                d ^. code @?= Nothing
    ]
