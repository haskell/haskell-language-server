{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main (
    main,
) where

import           Control.Lens                  ((^.))
import           Control.Lens.Fold             ((^?))
import           Control.Monad                 (guard)
import qualified Data.ByteString               as BS
import           Data.Either                   (isRight)
import           Data.List.Extra               (nubOrdOn)
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as T
import qualified Ide.Plugin.CabalProject.Parse as Lib
import qualified Language.LSP.Protocol.Lens    as L
import           System.FilePath
import           Test.Hls
import           Utils

main :: IO ()
main = do
    defaultTestRunner $
        testGroup
            "Cabal Plugin Tests"
            [ unitTests
            , pluginTests
            ]

-- ------------------------------------------------------------------------
-- Unit Tests
-- ------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ cabalProjectParserUnitTests
        ]

cabalProjectParserUnitTests :: TestTree
cabalProjectParserUnitTests =
    testGroup
        "Parsing Cabal Project"
        [ testCase "Simple Parsing works" $ do
            (warnings, pm) <- Lib.parseCabalProjectFileContents (testDataDir </> "cabal.project")
            liftIO $ do
                null warnings @? "Found unexpected warnings"
                isRight pm @? "Failed to parse base cabal.project file"
        ]

-- ------------------------ ------------------------------------------------
-- Integration Tests
-- ------------------------------------------------------------------------

pluginTests :: TestTree
pluginTests =
    testGroup
        "Plugin Tests"
        [ testGroup
            "Diagnostics"
            [ runCabalProjectTestCaseSession "Publishes Diagnostics on Error" "invalid-cabal-project" $ do
                _ <- openDoc "cabal.project" "cabal-project"
                diags <- cabalProjectCaptureKick
                unexpectedErrorDiag <- liftIO $ inspectDiagnostic diags ["unexpected 'f'"]
                liftIO $ do
                    length diags @?= 1
                    unexpectedErrorDiag ^. L.range @?= Range (Position 2 6) (Position 3 0)
                    unexpectedErrorDiag ^. L.severity @?= Just DiagnosticSeverity_Error
            ,   runCabalProjectTestCaseSession "Publishes Diagnostics on misspelled packages as Warning" "warning-cabal-project" $ do
                _ <- openDoc "cabal.project" "cabal-project"
                diags <- cabalProjectCaptureKick
                stanzaWarningDiag <- liftIO $ inspectDiagnosticAny diags ["'\"package\"' is a stanza, not a field. Remove the trailing ':' to parse a stanza."]
                liftIO $ do
                    length diags @?= 1
                    stanzaWarningDiag ^. L.range @?= Range (Position 0 0) (Position 1 0)
                    stanzaWarningDiag ^. L.severity @?= Just DiagnosticSeverity_Warning
            , runCabalProjectTestCaseSession "Clears diagnostics" "invalid-cabal-project" $ do
                doc <- openDoc "cabal.project" "cabal-project"
                diags <- cabalProjectCaptureKick
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["unexpected 'f'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 2 6) (Position 3 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                _ <- applyEdit doc $ TextEdit (Range (Position 2 6) (Position 3 0)) " -foo"
                newDiags <- cabalProjectCaptureKick
                liftIO $ newDiags @?= []
            , runCabalProjectTestCaseSession "No Diagnostics in .hs files from valid cabal.project file" "simple-cabal-project" $ do
                hsDoc <- openDoc "A.hs" "haskell"
                expectNoMoreDiagnostics 1 hsDoc "typechecking"
                cabalDoc <- openDoc "cabal.project" "cabal-project"
                expectNoMoreDiagnostics 1 cabalDoc "parsing"
            ]
        ]
