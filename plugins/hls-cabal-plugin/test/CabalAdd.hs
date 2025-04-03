{-# LANGUAGE OverloadedStrings #-}

module CabalAdd (
  cabalAddTests,
) where

import           Control.Lens                ((^.))
import           Control.Lens.Fold           ((^?))
import qualified Data.Maybe                  as Maybe
import qualified Data.Text                   as T
import qualified Data.Text.Internal.Search   as T
import           Distribution.Utils.Generic  (safeHead)
import           Ide.Plugin.Cabal.CabalAdd   (hiddenPackageSuggestion)
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types (Diagnostic (..), mkRange)
import           System.FilePath
import           Test.Hls                    (Session, TestTree, _R, anyMessage,
                                              assertEqual, documentContents,
                                              executeCodeAction,
                                              getAllCodeActions,
                                              getDocumentEdit, liftIO, openDoc,
                                              skipManyTill, testCase, testGroup,
                                              waitForDiagnosticsFrom, (@?=))
import           Utils

cabalAddTests :: TestTree
cabalAddTests =
  testGroup
    "CabalAdd Tests"
    [ runHaskellTestCaseSession "Code Actions - Can add hidden package to an executable" ("cabal-add-testdata" </> "cabal-add-exe")
        (generateAddDependencyTestSession "cabal-add-exe.cabal" ("src" </> "Main.hs") "split" [253])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a library" ("cabal-add-testdata" </> "cabal-add-lib")
        (generateAddDependencyTestSession "cabal-add-lib.cabal" ("src" </> "MyLib.hs") "split" [348])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a test" ("cabal-add-testdata" </> "cabal-add-tests")
        (generateAddDependencyTestSession "cabal-add-tests.cabal" ("test" </> "Main.hs") "split" [478])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a test with PackageImports" ("cabal-add-testdata" </> "cabal-add-tests")
        (generateAddDependencyTestSession "cabal-add-tests.cabal" ("test" </> "MainPackageImports.hs") "split" [731])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a benchmark" ("cabal-add-testdata" </> "cabal-add-bench")
        (generateAddDependencyTestSession "cabal-add-bench.cabal" ("bench" </> "Main.hs") "split" [403])

    , runHaskellTestCaseSession "Code Actions - Can add hidden package to an executable, multiple targets" ("cabal-add-testdata" </> "cabal-add-multitarget")
        (generateAddDependencyTestSession "cabal-add-multitarget.cabal" ("src" </> "Main.hs") "split" [269])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a library, multiple targets" ("cabal-add-testdata" </> "cabal-add-multitarget")
        (generateAddDependencyTestSession "cabal-add-multitarget.cabal" ("lib" </> "MyLib.hs") "split" [413])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to an internal library, multiple targets" ("cabal-add-testdata" </> "cabal-add-multitarget")
        (generateAddDependencyTestSession "cabal-add-multitarget.cabal" ("lib" </> "InternalLib.hs") "split" [413])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a test, multiple targets" ("cabal-add-testdata" </> "cabal-add-multitarget")
        (generateAddDependencyTestSession "cabal-add-multitarget.cabal" ("test" </> "Main.hs") "split" [655])
    , runHaskellTestCaseSession "Code Actions - Can add hidden package to a benchmark, multiple targets" ("cabal-add-testdata" </> "cabal-add-multitarget")
        (generateAddDependencyTestSession "cabal-add-multitarget.cabal" ("bench" </> "Main.hs") "split" [776])


    , runHaskellTestCaseSession "Code Actions - Guard against HPack" ("cabal-add-testdata" </> "cabal-add-packageYaml")
        (generatePackageYAMLTestSession ("src" </> "Main.hs"))

    , testHiddenPackageSuggestions "Check CabalAdd's parser, no version"
                                   [ "It is a member of the hidden package 'base'"
                                   , "It is a member of the hidden package 'Blammo-wai'"
                                   , "It is a member of the hidden package 'BlastHTTP'"
                                   , "It is a member of the hidden package 'CC-delcont-ref-tf'"
                                   , "It is a member of the hidden package '3d-graphics-examples'"
                                   , "It is a member of the hidden package 'AAI'"
                                   , "It is a member of the hidden package 'AWin32Console'"
                                   ]
                                   [ ("base", T.empty)
                                   , ("Blammo-wai", T.empty)
                                   , ("BlastHTTP", T.empty)
                                   , ("CC-delcont-ref-tf", T.empty)
                                   , ("3d-graphics-examples", T.empty)
                                   , ("AAI", T.empty)
                                   , ("AWin32Console", T.empty)
                                   ]
    , testHiddenPackageSuggestions "Check CabalAdd's parser, with version"
                                   [ "It is a member of the hidden package 'base-0.1.0.0'"
                                   , "It is a member of the hidden package 'Blammo-wai-0.11.0'"
                                   , "It is a member of the hidden package 'BlastHTTP-2.6.4.3'"
                                   , "It is a member of the hidden package 'CC-delcont-ref-tf-0.0.0.2'"
                                   , "It is a member of the hidden package '3d-graphics-examples-1.1.6'"
                                   , "It is a member of the hidden package 'AAI-0.1'"
                                   , "It is a member of the hidden package 'AWin32Console-1.19.1'"
                                   ]
                                   [ ("base","0.1.0.0")
                                   , ("Blammo-wai", "0.11.0")
                                   , ("BlastHTTP", "2.6.4.3")
                                   , ("CC-delcont-ref-tf", "0.0.0.2")
                                   , ("3d-graphics-examples", "1.1.6")
                                   , ("AAI", "0.1")
                                   , ("AWin32Console", "1.19.1")
                                   ]
    , testHiddenPackageSuggestions "Check CabalAdd's parser, no version, unicode comma"
                                   [ "It is a member of the hidden package \8216base\8217"
                                   , "It is a member of the hidden package \8216Blammo-wai\8217"
                                   , "It is a member of the hidden package \8216BlastHTTP\8217"
                                   , "It is a member of the hidden package \8216CC-delcont-ref-tf\8217"
                                   , "It is a member of the hidden package \8216AAI\8217"
                                   , "It is a member of the hidden package \8216AWin32Console\8217"
                                   ]
                                   [ ("base", T.empty)
                                   , ("Blammo-wai", T.empty)
                                   , ("BlastHTTP", T.empty)
                                   , ("CC-delcont-ref-tf", T.empty)
                                   , ("AAI", T.empty)
                                   , ("AWin32Console", T.empty)
                                   ]
    , testHiddenPackageSuggestions "Check CabalAdd's parser, with version, unicode comma"
                                   [ "It is a member of the hidden package \8216base-0.1.0.0\8217"
                                   , "It is a member of the hidden package \8216Blammo-wai-0.11.0\8217"
                                   , "It is a member of the hidden package \8216BlastHTTP-2.6.4.3\8217"
                                   , "It is a member of the hidden package \8216CC-delcont-ref-tf-0.0.0.2\8217"
                                   , "It is a member of the hidden package \8216AAI-0.1\8217"
                                   , "It is a member of the hidden package \8216AWin32Console-1.19.1\8217"
                                   ]
                                   [ ("base","0.1.0.0")
                                   , ("Blammo-wai", "0.11.0")
                                   , ("BlastHTTP", "2.6.4.3")
                                   , ("CC-delcont-ref-tf", "0.0.0.2")
                                   , ("AAI", "0.1")
                                   , ("AWin32Console", "1.19.1")
                                   ]
    , testHiddenPackageSuggestions "Check CabalAdd's parser, with version, unicode comma"
                                   [ "It is a member of the hidden package \8216\&3d-graphics-examples\8217"
                                   , "It is a member of the hidden package \8216\&3d-graphics-examples-1.1.6\8217"
                                   ]
                                   [ ("3d-graphics-examples", T.empty)
                                   , ("3d-graphics-examples", "1.1.6")
                                   ]
    , testHiddenPackageSuggestions "Check CabalAdd's parser, with version, with PackageImports"
                                   [ "(needs flag -package-id base-0.1.0.0)"
                                   , "(needs flag -package-id Blammo-wai-0.11.0)"
                                   , "(needs flag -package-id BlastHTTP-2.6.4.3)"
                                   , "(needs flag -package-id CC-delcont-ref-tf-0.0.0.2)"
                                   , "(needs flag -package-id 3d-graphics-examples-1.1.6)"
                                   , "(needs flag -package-id AAI-0.1)"
                                   , "(needs flag -package-id AWin32Console-1.19.1)"
                                   ]
                                   [ ("base","0.1.0.0")
                                   , ("Blammo-wai", "0.11.0")
                                   , ("BlastHTTP", "2.6.4.3")
                                   , ("CC-delcont-ref-tf", "0.0.0.2")
                                   , ("3d-graphics-examples", "1.1.6")
                                   , ("AAI", "0.1")
                                   , ("AWin32Console", "1.19.1")
                                   ]
    ]
 where
    generateAddDependencyTestSession :: FilePath -> FilePath -> T.Text -> [Int] -> Session ()
    generateAddDependencyTestSession cabalFile haskellFile dependency indicesRes = do
        hsdoc <- openDoc haskellFile "haskell"
        cabDoc <- openDoc cabalFile "cabal"
        _ <- waitForDiagnosticsFrom hsdoc
        cas <- Maybe.mapMaybe (^? _R) <$> getAllCodeActions hsdoc
        let selectedCas = filter (\ca -> "Add dependency" `T.isPrefixOf` (ca ^. L.title)) cas
        mapM_ executeCodeAction selectedCas
        _ <- skipManyTill anyMessage $ getDocumentEdit cabDoc -- Wait for the changes in cabal file
        contents <- documentContents cabDoc
        liftIO $ assertEqual (T.unpack dependency <> " isn't found in the cabal file") indicesRes (T.indices dependency contents)
    testHiddenPackageSuggestions :: String -> [T.Text] -> [(T.Text, T.Text)] -> TestTree
    testHiddenPackageSuggestions testTitle messages suggestions =
        let diags = map (\msg -> messageToDiagnostic msg ) messages
            suggestions' = map (safeHead . hiddenPackageSuggestion) diags
            assertions   = zipWith (@?=) suggestions' (map Just suggestions)
            testNames    = map (\(f, s) -> "Check if " ++ T.unpack f ++ (if s == "" then "" else "-") ++ T.unpack s ++ " was parsed correctly") suggestions
            test         = testGroup testTitle $ zipWith testCase testNames assertions
        in test
    messageToDiagnostic :: T.Text -> Diagnostic
    messageToDiagnostic msg = Diagnostic {
            _range    = mkRange 0 0 0 0
          , _severity = Nothing
          , _code     = Nothing
          , _source   = Nothing
          , _message  = msg
          , _relatedInformation = Nothing
          , _tags     = Nothing
          , _codeDescription = Nothing
          , _data_ = Nothing
        }


    generatePackageYAMLTestSession :: FilePath -> Session ()
    generatePackageYAMLTestSession haskellFile  = do
        hsdoc <- openDoc haskellFile "haskell"
        _ <- waitForDiagnosticsFrom hsdoc
        cas <- Maybe.mapMaybe (^? _R) <$> getAllCodeActions hsdoc
        let selectedCas = filter (\ca -> "Add dependency" `T.isPrefixOf` (ca ^. L.title)) cas
        liftIO $ assertEqual "PackageYAML" [] selectedCas
