{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main (
    main,
) where

import           CabalAdd                        (cabalAddTests)
import           Completer                       (completerTests)
import           Context                         (contextTests)
import           Control.Lens                    ((^.))
import           Control.Lens.Fold               ((^?))
import           Control.Monad                   (guard)
import qualified Data.ByteString                 as BS
import           Data.Either                     (isRight)
import           Data.List.Extra                 (nubOrdOn)
import qualified Data.Maybe                      as Maybe
import qualified Data.Text                       as T
import           Definition                      (gotoDefinitionTests)
import           Ide.Plugin.Cabal.LicenseSuggest (licenseErrorSuggestion)
import qualified Ide.Plugin.Cabal.Parse          as Lib
import qualified Language.LSP.Protocol.Lens      as L
import           Outline                         (outlineTests)
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
            , outlineTests
            , codeActionTests
            , gotoDefinitionTests
            , hoverTests
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
            licenseErrorSuggestion maxCompletions "Unknown license identifier: 'BSD3' Do you mean BSD-3-Clause?" @?= []
        , testCase "BSD-3-Clause" $ do
            take 2 (licenseErrorSuggestion maxCompletions "Unknown SPDX license identifier: 'BSD3' Do you mean BSD-3-Clause?")
                @?=
-- Cabal-syntax 3.12.0.0 added bunch of new licenses, so now more licenses match "BSD3" pattern
#if MIN_VERSION_Cabal_syntax(3,12,0)
                    [("BSD3", "BSD-4.3RENO"), ("BSD3", "BSD-3-Clause")]
#else
                    [("BSD3", "BSD-3-Clause"), ("BSD3", "BSD-3-Clause-LBNL")]
#endif
        , testCase "MiT" $ do
            -- contains no suggestion
            take 2 (licenseErrorSuggestion maxCompletions "Unknown SPDX license identifier: 'MiT'")
                @?= [("MiT", "MIT"), ("MiT", "MIT-0")]
        ]
  where
    maxCompletions = 100


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
                _ <- openDoc "invalid.cabal" "cabal"
                diags <- cabalCaptureKick
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
            , runCabalTestCaseSession "Clears diagnostics" "" $ do
                doc <- openDoc "invalid.cabal" "cabal"
                diags <- cabalCaptureKick
                unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
                liftIO $ do
                    length diags @?= 1
                    unknownLicenseDiag ^. L.range @?= Range (Position 3 24) (Position 4 0)
                    unknownLicenseDiag ^. L.severity @?= Just DiagnosticSeverity_Error
                _ <- applyEdit doc $ TextEdit (Range (Position 3 20) (Position 4 0)) "BSD-3-Clause\n"
                newDiags <- cabalCaptureKick
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
        ]
-- ----------------------------------------------------------------------------
-- Code Action Tests
-- ----------------------------------------------------------------------------

codeActionTests :: TestTree
codeActionTests = testGroup "Code Actions"
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
                @?= T.unlines
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
                @?= T.unlines
                    [ "cabal-version:      3.0"
                    , "name:               licenseCodeAction2"
                    , "version:            0.1.0.0"
                    , "license:            Apache-2.0"
                    , ""
                    , "library"
                    , "    build-depends:    base"
                    , "    default-language: Haskell2010"
                    ]
    , runCabalGoldenSession "Code Actions - Can fix field names" "code-actions" "FieldSuggestions" $ \doc -> do
        _ <- waitForDiagnosticsFrom doc
        cas <- Maybe.mapMaybe (^? _R) <$> getAllCodeActions doc
        -- Filter out the code actions we want to invoke.
        -- We only want to invoke Code Actions with certain titles, and
        -- we want to invoke them only once, not once for each cursor request.
        -- 'getAllCodeActions' iterates over each cursor position and requests code actions.
        let selectedCas = nubOrdOn (^. L.title) $ filter
                (\ca -> (ca ^. L.title) `elem`
                    [ "Replace with license"
                    , "Replace with build-type"
                    , "Replace with extra-doc-files"
                    , "Replace with ghc-options"
                    , "Replace with location"
                    , "Replace with default-language"
                    , "Replace with import"
                    , "Replace with build-depends"
                    , "Replace with main-is"
                    , "Replace with hs-source-dirs"
                    ]) cas
        mapM_ executeCodeAction selectedCas
        pure ()
    , cabalAddTests
    ]
  where
    getLicenseAction :: T.Text -> [Command |? CodeAction] -> [CodeAction]
    getLicenseAction license codeActions = do
        InR action@CodeAction{_title} <- codeActions
        guard (_title == "Replace with " <> license)
        pure action

-- ----------------------------------------------------------------------------
-- Hover Tests
-- ----------------------------------------------------------------------------

hoverTests :: TestTree
hoverTests = testGroup "Hover"
    [ hoverOnDependencyTests
    ]

hoverOnDependencyTests :: TestTree
hoverOnDependencyTests = testGroup "Hover Dependency"
    [ hoverContainsTest "base with separated version" "hover-deps.cabal" (Position 6 25) "[Documentation](https://hackage.haskell.org/package/base)"
    , hoverContainsTest "aeson with not separated version " "hover-deps.cabal" (Position 7 25) "[Documentation](https://hackage.haskell.org/package/aeson)"
    , hoverContainsTest "lens no version" "hover-deps.cabal" (Position 7 42) "[Documentation](https://hackage.haskell.org/package/lens)"

    , hoverIsNullTest "name has no documentation" "hover-deps.cabal" (Position 1 25)
    , hoverIsNullTest "exposed-modules has no documentation" "hover-deps.cabal" (Position 5 25)
    , hoverIsNullTest "hs-source-dirs has no documentation" "hover-deps.cabal" (Position 8 25)
    ]
    where
        hoverContainsTest :: TestName -> FilePath -> Position -> T.Text -> TestTree
        hoverContainsTest testName cabalFile pos containedText =
            runCabalTestCaseSession testName "hover" $ do
                doc <- openDoc cabalFile "cabal"
                h <- getHover doc pos
                case h of
                    Nothing -> liftIO $ assertFailure "No hover"
                    Just (Hover contents _) -> case contents of
                        InL (MarkupContent _ txt) -> do
                            liftIO
                            $ assertBool ("Failed to find `" <> T.unpack containedText <> "` in hover message: " <> T.unpack txt)
                            $ containedText `T.isInfixOf` txt
                        _ -> liftIO $ assertFailure "Unexpected content type"
                closeDoc doc

        hoverIsNullTest :: TestName -> FilePath -> Position -> TestTree
        hoverIsNullTest testName cabalFile pos =
            runCabalTestCaseSession testName "hover" $ do
                doc <- openDoc cabalFile "cabal"
                h <- getHover doc pos
                liftIO $ assertBool ("Found hover `" <> show h <> "`") $ Maybe.isNothing h
                closeDoc doc
