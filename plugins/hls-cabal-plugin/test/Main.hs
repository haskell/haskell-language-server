{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main (
    main,
) where

import           Completer                       (completerTests)
import           Context                         (contextTests)
import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import qualified Data.ByteString                 as BS
import           Data.Either                     (isRight)
import qualified Data.Text                       as T
import qualified Data.Text                       as Text
import           Ide.Plugin.Cabal.LicenseSuggest (licenseErrorSuggestion)
import qualified Ide.Plugin.Cabal.Parse          as Lib
import qualified Language.LSP.Protocol.Lens      as L
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
            , completerTests
            , contextTests
            ]

-- ------------------------------------------------------------------------
-- Unit Tests
-- ------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
    testGroup
        "Unit Tests"
        [ cabalParserUnitTests
        , codeActionUnitTests
        ]

cabalParserUnitTests :: TestTree
cabalParserUnitTests =
    testGroup
        "Parsing Cabal"
        [ testCase "Simple Parsing works" $ do
            (warnings, pm) <- Lib.parseCabalFileContents =<< BS.readFile (testDataDir </> "simple.cabal")
            liftIO $ do
                null warnings @? "Found unexpected warnings"
                isRight pm @? "Failed to parse GenericPackageDescription"
        ]

codeActionUnitTests :: TestTree
codeActionUnitTests =
    testGroup
        "Code Action Tests"
        [ testCase "Unknown format" $ do
            -- the message has the wrong format
            licenseErrorSuggestion "Unknown license identifier: 'BSD3' Do you mean BSD-3-Clause?" @?= []
        , testCase "BSD-3-Clause" $ do
            take 2 (licenseErrorSuggestion "Unknown SPDX license identifier: 'BSD3' Do you mean BSD-3-Clause?")
                @?= [("BSD3", "BSD-3-Clause"), ("BSD3", "BSD-3-Clause-LBNL")]
        , testCase "MiT" $ do
            -- contains no suggestion
            take 2 (licenseErrorSuggestion "Unknown SPDX license identifier: 'MiT'")
                @?= [("MiT", "MIT"), ("MiT", "MIT-0")]
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
            [ runCabalTestCaseSession "Publishes Diagnostics on Error" "" $ do
                doc <- openDoc "invalid.cabal" "cabal"
                diags <- waitForDiagnosticsFromSource doc "cabal"
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
            , runCabalTestCaseSession "Clears diagnostics" "" $ do
                doc <- openDoc "invalid.cabal" "cabal"
                diags <- waitForDiagnosticsFrom doc
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                _ <- applyEdit doc $ TextEdit (Range (Position 3 20) (Position 4 0)) "BSD-3-Clause\n"
                newDiags <- waitForDiagnosticsFrom doc
                liftIO $ newDiags @?= []
            , runCabalTestCaseSession "No Diagnostics in .hs files from valid .cabal file" "simple-cabal" $ do
                hsDoc <- openDoc "A.hs" "haskell"
                expectNoMoreDiagnostics 1 hsDoc "typechecking"
                cabalDoc <- openDoc "simple-cabal.cabal" "cabal"
                expectNoMoreDiagnostics 1 cabalDoc "parsing"
            , runCabalTestCaseSession "Diagnostics in .hs files from invalid .cabal file" "simple-cabal" $ do
                    hsDoc <- openDoc "A.hs" "haskell"
                    expectNoMoreDiagnostics 1 hsDoc "typechecking"
                    cabalDoc <- openDoc "simple-cabal.cabal" "cabal"
                    expectNoMoreDiagnostics 1 cabalDoc "parsing"
                    let theRange = Range (Position 3 20) (Position 3 23)
                    -- Invalid license
                    changeDoc
                        cabalDoc
                        [ TextDocumentContentChangeEvent $
                            InL TextDocumentContentChangePartial
                                { _range = theRange
                                , _rangeLength = Nothing
                                , _text = "MIT3"
                                }
                        ]
                    cabalDiags <- waitForDiagnosticsFrom cabalDoc
                    unknownLicenseDiag <- liftIO $ inspectDiagnostic cabalDiags ["Unknown SPDX license identifier: 'MIT3'"]
                    expectNoMoreDiagnostics 1 hsDoc "typechecking"
                    liftIO $ do
                        length cabalDiags @?= 1
                        unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                        unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
            ]
        , testGroup
            "Code Actions"
            [ runCabalTestCaseSession "BSD-3" "" $ do
                doc <- openDoc "licenseCodeAction.cabal" "cabal"
                diags <- waitForDiagnosticsFromSource doc "cabal"
                reduceDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    reduceDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    reduceDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                [codeAction] <- getLicenseAction "BSD-3-Clause" <$> getCodeActions doc (Range (Position 3 24) (Position 4 0))
                executeCodeAction codeAction
                contents <- documentContents doc
                liftIO $
                    contents
                        @?= Text.unlines
                            [ "cabal-version:      3.0"
                            , "name:               licenseCodeAction"
                            , "version:            0.1.0.0"
                            , "license:            BSD-3-Clause"
                            , ""
                            , "library"
                            , "    build-depends:    base"
                            , "    default-language: Haskell2010"
                            ]
            , runCabalTestCaseSession "Apache-2.0" "" $ do
                doc <- openDoc "licenseCodeAction2.cabal" "cabal"
                diags <- waitForDiagnosticsFromSource doc "cabal"
                -- test if it supports typos in license name, here 'apahe'
                reduceDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'APAHE'"]
                liftIO $ do
                    length diags @?= 1
                    reduceDiag ^. L.range @?= Range (Position 3 25) (Position 4 0)
                    reduceDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                [codeAction] <- getLicenseAction "Apache-2.0" <$> getCodeActions doc (Range (Position 3 24) (Position 4 0))
                executeCodeAction codeAction
                contents <- documentContents doc
                liftIO $
                    contents
                        @?= Text.unlines
                            [ "cabal-version:      3.0"
                            , "name:               licenseCodeAction2"
                            , "version:            0.1.0.0"
                            , "license:            Apache-2.0"
                            , ""
                            , "library"
                            , "    build-depends:    base"
                            , "    default-language: Haskell2010"
                            ]
            ]
        ]
  where
    getLicenseAction :: T.Text -> [Command |? CodeAction] -> [CodeAction]
    getLicenseAction license codeActions = do
        InR action@CodeAction{_title} <- codeActions
        guard (_title == "Replace with " <> license)
        pure action
