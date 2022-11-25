{-# LANGUAGE OverloadedStrings        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeOperators            #-}
module Main
  ( main
  ) where

import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import qualified Data.ByteString                 as BS
import           Data.Either                     (isRight)
import           Data.Function
import qualified Data.Text                       as Text
import           Development.IDE.Types.Logger
import           Ide.Plugin.Cabal
import           Ide.Plugin.Cabal.LicenseSuggest (licenseErrorSuggestion)
import qualified Ide.Plugin.Cabal.Parse          as Lib
import qualified Language.LSP.Types.Lens         as J
import           System.FilePath
import           Test.Hls


cabalPlugin :: Recorder (WithPriority Log) -> PluginDescriptor IdeState
cabalPlugin recorder = descriptor recorder "cabal"

main :: IO ()
main = do
  recorder <- initialiseRecorder True
  defaultTestRunner $
    testGroup "Cabal Plugin Tests"
      [ unitTests
      , pluginTests recorder
      ]

-- | @initialiseRecorder silent@
--
-- If @'silent' == True@, then don't log anything, otherwise
-- the recorder is the standard recorder of HLS. Useful for debugging.
initialiseRecorder :: Bool -> IO (Recorder (WithPriority Log))
initialiseRecorder True = pure mempty
initialiseRecorder False = do
  docWithPriorityRecorder <- makeDefaultStderrRecorder Nothing Debug

  let docWithFilteredPriorityRecorder =
        docWithPriorityRecorder
        & cfilter (\WithPriority{ priority } -> priority >= Debug)
  pure $ docWithFilteredPriorityRecorder
               & cmapWithPrio pretty

-- ------------------------------------------------------------------------
-- Unit Tests
-- ------------------------------------------------------------------------

unitTests :: TestTree
unitTests =
  testGroup "Unit Tests"
  [ cabalParserUnitTests,
    codeActionUnitTests
  ]

cabalParserUnitTests :: TestTree
cabalParserUnitTests = testGroup "Parsing Cabal"
  [ testCase "Simple Parsing works" $ do
      (warnings, pm) <- Lib.parseCabalFileContents =<< BS.readFile (testDataDir </> "simple.cabal")
      liftIO $ do
        null warnings @? "Found unexpected warnings"
        isRight pm @? "Failed to parse GenericPackageDescription"
  ]

codeActionUnitTests :: TestTree
codeActionUnitTests = testGroup "Code Action Tests"
  [ testCase "Unknown format" $ do
      -- the message has the wrong format
      licenseErrorSuggestion "Unknown license identifier: 'BSD3' Do you mean BSD-3-Clause?" @?= [],

    testCase "BSD-3-Clause" $ do
      take 2 (licenseErrorSuggestion "Unknown SPDX license identifier: 'BSD3' Do you mean BSD-3-Clause?")
        @?= [("BSD3","BSD-3-Clause"),("BSD3","BSD-3-Clause-LBNL")],

    testCase "MiT" $ do
      -- contains no suggestion
      take 2 (licenseErrorSuggestion "Unknown SPDX license identifier: 'MiT'")
        @?= [("MiT","MIT"),("MiT","MIT-0")]
  ]

-- ------------------------------------------------------------------------
-- Integration Tests
-- ------------------------------------------------------------------------

pluginTests :: Recorder (WithPriority Log) -> TestTree
pluginTests recorder = testGroup "Plugin Tests"
  [ testGroup "Diagnostics"
    [ runCabalTestCaseSession "Publishes Diagnostics on Error" recorder "" $ do
        doc <- openDoc "invalid.cabal" "cabal"
        diags <- waitForDiagnosticsFromSource doc "cabal"
        unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
        liftIO $ do
            length diags @?= 1
            unknownLicenseDiag ^. J.range @?= Range (Position 3 24) (Position 4 0)
            unknownLicenseDiag ^. J.severity @?= Just DsError
    , runCabalTestCaseSession "Clears diagnostics" recorder "" $ do
        doc <- openDoc "invalid.cabal" "cabal"
        diags <- waitForDiagnosticsFrom doc
        unknownLicenseDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
        liftIO $ do
            length diags @?= 1
            unknownLicenseDiag ^. J.range @?= Range (Position 3 24) (Position 4 0)
            unknownLicenseDiag ^. J.severity @?= Just DsError
        _ <- applyEdit doc $ TextEdit (Range (Position 3 20) (Position 4 0)) "BSD-3-Clause\n"
        newDiags <- waitForDiagnosticsFrom doc
        liftIO $ newDiags @?= []
    , runCabalTestCaseSession "No Diagnostics in .hs files from valid .cabal file" recorder "simple-cabal" $ do
        hsDoc <- openDoc "A.hs" "haskell"
        expectNoMoreDiagnostics 1 hsDoc "typechecking"
        cabalDoc <- openDoc "simple-cabal.cabal" "cabal"
        expectNoMoreDiagnostics 1 cabalDoc "parsing"
    , ignoreTestBecause "Testcase is flaky for certain GHC versions (e.g. 9.2.4). See #3333 for details." $ do
      runCabalTestCaseSession "Diagnostics in .hs files from invalid .cabal file" recorder "simple-cabal" $ do
        hsDoc <- openDoc "A.hs" "haskell"
        expectNoMoreDiagnostics 1 hsDoc "typechecking"
        cabalDoc <- openDoc "simple-cabal.cabal" "cabal"
        expectNoMoreDiagnostics 1 cabalDoc "parsing"
        let theRange = Range (Position 3 20) (Position 3 23)
        -- Invalid license
        changeDoc cabalDoc [TextDocumentContentChangeEvent (Just theRange) Nothing "MIT3"]
        cabalDiags <- waitForDiagnosticsFrom cabalDoc
        unknownLicenseDiag <- liftIO $ inspectDiagnostic cabalDiags ["Unknown SPDX license identifier: 'MIT3'"]
        expectNoMoreDiagnostics 1 hsDoc "typechecking"
        liftIO $ do
            length cabalDiags @?= 1
            unknownLicenseDiag ^. J.range @?= Range (Position 3 24) (Position 4 0)
            unknownLicenseDiag ^. J.severity @?= Just DsError
    ]
  , testGroup "Code Actions"
    [ runCabalTestCaseSession "BSD-3" recorder "" $ do
        doc <- openDoc "licenseCodeAction.cabal" "cabal"
        diags <- waitForDiagnosticsFromSource doc "cabal"
        reduceDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'BSD3'"]
        liftIO $ do
            length diags @?= 1
            reduceDiag ^. J.range @?= Range (Position 3 24) (Position 4 0)
            reduceDiag ^. J.severity @?= Just DsError
        [codeAction] <- getLicenseAction "BSD-3-Clause" <$> getCodeActions doc (Range (Position 3 24) (Position 4 0))
        executeCodeAction codeAction
        contents <- documentContents doc
        liftIO $ contents @?= Text.unlines
          [ "cabal-version:      3.0"
          , "name:               licenseCodeAction"
          , "version:            0.1.0.0"
          , "license:            BSD-3-Clause"
          , ""
          , "library"
          , "    build-depends:    base"
          , "    default-language: Haskell2010"
          ]
    , runCabalTestCaseSession "Apache-2.0" recorder "" $ do
        doc <- openDoc "licenseCodeAction2.cabal" "cabal"
        diags <- waitForDiagnosticsFromSource doc "cabal"
        -- test if it supports typos in license name, here 'apahe'
        reduceDiag <- liftIO $ inspectDiagnostic diags ["Unknown SPDX license identifier: 'APAHE'"]
        liftIO $ do
            length diags @?= 1
            reduceDiag ^. J.range @?= Range (Position 3 25) (Position 4 0)
            reduceDiag ^. J.severity @?= Just DsError
        [codeAction] <- getLicenseAction "Apache-2.0" <$> getCodeActions doc (Range (Position 3 24) (Position 4 0))
        executeCodeAction codeAction
        contents <- documentContents doc
        liftIO $ contents @?= Text.unlines
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
    getLicenseAction :: Text.Text -> [Command |? CodeAction] -> [CodeAction]
    getLicenseAction license codeActions = do
                  InR action@CodeAction{_title} <- codeActions
                  guard (_title=="Replace with " <> license)
                  pure action

-- ------------------------------------------------------------------------
-- Runner utils
-- ------------------------------------------------------------------------

runCabalTestCaseSession :: TestName -> Recorder (WithPriority Log) -> FilePath -> Session () -> TestTree
runCabalTestCaseSession title recorder subdir act = testCase title $ runCabalSession recorder subdir act

runCabalSession :: Recorder (WithPriority Log) -> FilePath -> Session a -> IO a
runCabalSession recorder subdir  =
    failIfSessionTimeout . runSessionWithServer (cabalPlugin recorder) (testDataDir </> subdir)

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
