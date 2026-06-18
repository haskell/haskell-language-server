{-# LANGUAGE OverloadedStrings #-}

module Definition (
    gotoDefinitionTests,
) where

import           Control.Lens                ((^.))
import           Data.List.Extra             (isSuffixOf)
import qualified Language.LSP.Protocol.Lens  as L
import qualified Language.LSP.Protocol.Types as LSP
import           System.FilePath
import           Test.Hls
import           Utils


gotoDefinitionTests :: TestTree
gotoDefinitionTests = testGroup "Goto Definition"
    [ gotoCommonSectionDefinitionTests
    , gotoModuleDefinitionTests
    ]

gotoModuleDefinitionTests :: TestTree
gotoModuleDefinitionTests = testGroup "Goto Module Definition"
    [ testGoToDefinitionLink "simple cabal test" "simple-cabal" "simple-cabal.cabal"
                             (Position 8 23) "A.hs"

    , testGoToDefinitionLink "library start of exposed-modules" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 6 22) ("src" </> "Library" </> "Lib.hs")
    , testGoToDefinitionLink "library middle of exposed-modules" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 6 29) ("src" </> "Library" </> "Lib.hs")
    , testGoToDefinitionLink "library end of exposed-modules" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 6 33) ("src" </> "Library" </> "Lib.hs")
    , testGoToDefinitionLink "library start of other-modules" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 9 22) ("src" </> "Library" </> "Other" </> "OtherLib.hs")
    , testGoToDefinitionLink "library end of other-modules" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 9 44) ("src" </> "Library" </> "Other" </> "OtherLib.hs")

    , testGoToDefinitionLink "executable other-modules" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 22 10) ("src" </> "exe" </> "Config.hs")

    , testGoToDefinitionLink "test-suite other-modules Config" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 31 10) ("src" </> "test"</> "Config.hs")
    , testGoToDefinitionLink "test-suite other-modules Library" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 34 10) ("src" </> "test" </> "Library.hs")

    , testGoToDefinitionLink "benchmark other-modules Config" ("goto-definition" </> "modules") "module-examples.cabal"
                             (Position 45 30) ("src" </> "bench" </> "Config.hs")

    , testGoToDefinitionLinkNoLocation "not existent module" ("goto-definition" </> "modules") "module-examples.cabal" (Position 48 25)
    , testGoToDefinitionLinkNoLocation "behind module" ("goto-definition" </> "modules") "module-examples.cabal" (Position 9 20)
    , testGoToDefinitionLinkNoLocation "after module" ("goto-definition" </> "modules") "module-examples.cabal" (Position 9 50)
    ]
    where
        getUriFromDefinition :: Show b => (Definition |? b) -> Uri
        getUriFromDefinition (InL (Definition (InL loc))) = loc^.L.uri
        getUriFromDefinition unk = error $ "Unexpected pattern '" ++ show unk ++ "' , expected '(InL (Definition (InL loc))'"

        testGoToDefinitionLink :: TestName -> FilePath -> FilePath -> Position -> FilePath -> TestTree
        testGoToDefinitionLink testName testDir cabalFile cursorPos expectedFilePath =
            runCabalTestCaseSession testName testDir $ do
                doc <- openDoc cabalFile "cabal"
                definitions <- getDefinitions doc cursorPos
                let uri = getUriFromDefinition definitions
                case uriToFilePath uri of
                    Nothing -> error $ "Not possible to convert Uri " <> show uri <> " to FilePath"
                    Just filePath -> do
                        let
                            isCorrectPath = expectedFilePath `isSuffixOf` filePath
                        liftIO $ isCorrectPath @? ("Absolute path expected to end on " <> filePath <>
                                                   " but " <> filePath <> " was given.")

        testGoToDefinitionLinkNoLocation :: TestName -> FilePath -> FilePath -> Position -> TestTree
        testGoToDefinitionLinkNoLocation testName testDir cabalFile cursorPos =
            runCabalTestCaseSession testName testDir $ do
                doc <- openDoc cabalFile "cabal"
                empty <- getDefinitions doc cursorPos
                liftIO $ empty @?= (InR $ InR LSP.Null)

gotoCommonSectionDefinitionTests :: TestTree
gotoCommonSectionDefinitionTests = testGroup "Goto Common Section Definition"
    [ positiveTest "middle of identifier"            (Position 27 16) (mkRange  6 0  7 22)
    , positiveTest "left of identifier"              (Position 30 12) (mkRange 10 0 17 40)
    , positiveTest "right of identifier"             (Position 33 22) (mkRange 20 0 23 34)
    , positiveTest "left of '-' in identifier"       (Position 36 20) (mkRange  6 0  7 22)
    , positiveTest "right of '-' in identifier"      (Position 39 19) (mkRange 10 0 17 40)
    , positiveTest "identifier in identifier list"   (Position 42 16) (mkRange 20 0 23 34)
    , positiveTest "left of ',' right of identifier" (Position 45 33) (mkRange 10 0 17 40)
    , positiveTest "right of ',' left of identifier" (Position 48 34) (mkRange  6 0  7 22)

    , negativeTest "right of ',' left of space"      (Position 51 23)
    , negativeTest "right of ':' left of space"      (Position 54 11)
    , negativeTest "not a definition"                (Position 57 8)
    , negativeTest "empty space"                     (Position 59 7)
    ]
    where
        getRangeFromDefinition :: Show b => (Definition |? b) -> Range
        getRangeFromDefinition (InL (Definition (InL loc))) = loc^.L.range
        getRangeFromDefinition unk = error $ "Unexpected pattern '" ++ show unk ++ "' , expected '(InL (Definition (InL loc))'"

        -- A positive test checks if the provided range is equal
        -- to the expected range from the definition in the test file.
        -- The test emulates a goto-definition request of an actual definition.
        positiveTest :: TestName -> Position -> Range -> TestTree
        positiveTest testName cursorPos expectedRange =
            runCabalTestCaseSession testName ("goto-definition" </> "common-section") $ do
                doc <- openDoc "simple-with-common.cabal" "cabal"
                definitions <- getDefinitions doc cursorPos
                let range = getRangeFromDefinition definitions
                liftIO $ range @?= expectedRange

        -- A negative test checks if the request failed and
        -- the provided result is empty, i.e. `InR $ InR Null`.
        -- The test emulates a goto-definition request of anything but an
        -- actual definition.
        negativeTest :: TestName -> Position -> TestTree
        negativeTest testName cursorPos =
            runCabalTestCaseSession testName ("goto-definition" </> "common-section") $ do
                doc <- openDoc "simple-with-common.cabal" "cabal"
                empty <- getDefinitions doc cursorPos
                liftIO $ empty @?= (InR $ InR LSP.Null)
