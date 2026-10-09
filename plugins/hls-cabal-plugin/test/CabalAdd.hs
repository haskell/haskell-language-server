{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module CabalAdd (
    cabalAddDependencyTests,
    cabalAddModuleTests,
) where

import           Control.Lens                                  ((^.))
import           Control.Lens.Fold                             ((^?))
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Data.Text.Internal.Search                     as T
import           Distribution.ModuleName                       (fromString)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.Pretty                           as Pretty
import           Distribution.Types.Component
import           Distribution.Utils.Generic                    (safeHead)
import           Ide.Plugin.Cabal.CabalAdd.CodeAction          (hiddenPackageSuggestion)
import           Ide.Plugin.Cabal.Parse                        (parseCabalFileContents)
import qualified Language.LSP.Protocol.Lens                    as L
import qualified Language.LSP.Protocol.Types                   as J
import           System.FilePath
import           Test.Hls
import           Utils

cabalAddModuleTests :: TestTree
cabalAddModuleTests =
    testGroup
        "Add Module"
        [ runHaskellTestCaseSession "Add to benchmark" ("cabal-add-module" </> "library") $ do
            let compName = CBenchName "test1"
            pd <- generateAddDependencyTestSession "test.cabal" "Main.hs" compName
            checkModuleAddedTo pd "Main" compName
        , runHaskellTestCaseSession "Add to executable" ("cabal-add-module" </> "library") $ do
            let compName = CExeName "test"
            pd <- generateAddDependencyTestSession "test.cabal" "Main.hs" compName
            checkModuleAddedTo pd "Main" compName
        , runHaskellTestCaseSession "Add to test-suite" ("cabal-add-module" </> "library") $ do
            let compName = CTestName "test2"
            pd <- generateAddDependencyTestSession "test.cabal" "Main.hs" compName
            checkModuleAddedTo pd "Main" compName
        , runHaskellTestCaseSession "Add to library" ("cabal-add-module" </> "library") $ do
            let compName = CLibName $ LSubLibName "test3"
            pd <- generateAddDependencyTestSession "test.cabal" "Main.hs" compName
            checkModuleAddedTo pd "Main" compName
        , runHaskellTestCaseSession "Add to main library" ("cabal-add-module" </> "library") $ do
            let compName = CLibName LMainLibName
            pd <- generateAddDependencyTestSession "test.cabal" "Main.hs" compName
            checkModuleAddedTo pd "Main" compName
        ]
  where
    generateAddDependencyTestSession :: FilePath -> FilePath -> ComponentName -> Session PackageDescription
    generateAddDependencyTestSession cabalFile haskellFile compName = do
        haskellDoc <- openDoc haskellFile "haskell"
        cabalDoc <- openDoc cabalFile "cabal"
        _ <- waitForDiagnosticsFrom haskellDoc
        cas <- Maybe.mapMaybe (^? _R) <$> getAllCodeActions haskellDoc
        let selectedCas = filter (\ca -> (T.pack $ "Add to " <> Pretty.prettyShow compName <> " ") `T.isPrefixOf` (ca ^. L.title)) cas
        mapM_ executeCodeAction $ selectedCas
        _ <- skipManyTill anyMessage $ getDocumentEdit cabalDoc -- Wait for the changes in cabal file
        contents <- documentContents cabalDoc
        case parseCabalFileContents $ T.encodeUtf8 contents of
            (_, Right gpd) -> pure $ flattenPackageDescription gpd
            _ -> liftIO $ assertFailure "could not parse cabal file to gpd"

    -- | Verify that the given module was added to the desired component.
    -- Note that we do not care whether it was added to exposed-modules or other-modules of that component.
    checkModuleAddedTo :: PackageDescription -> String -> ComponentName -> Session ()
    checkModuleAddedTo pd modName compName = do
        let comp = getComponent pd compName
            compModules = case comp of
                CLib lib     -> explicitLibModules lib
                CFLib fLib   -> foreignLibModules fLib
                CExe exe     -> exeModules exe
                CTest test   -> testModules test
                CBench bench -> benchmarkModules bench
            testDescription = modName <> " was added to " <> showComponentName compName
        liftIO $ assertBool testDescription $ fromString modName `elem` compModules

cabalAddDependencyTests :: TestTree
cabalAddDependencyTests =
  testGroup
    "Add dependency"
    [ runHaskellTestCaseSession "Add to executable" ("cabal-add-testdata" </> "exe")
        (generateAddDependencyTestSession "exe.cabal" ("src" </> "Main.hs") "split" [253])
    , runHaskellTestCaseSession "Add to library" ("cabal-add-testdata" </> "lib")
        (generateAddDependencyTestSession "lib.cabal" ("src" </> "MyLib.hs") "split" [348])
    , runHaskellTestCaseSession "Add to testsuite" ("cabal-add-testdata" </> "tests")
        (generateAddDependencyTestSession "tests.cabal" ("test" </> "Main.hs") "split" [478])
    , runHaskellTestCaseSession "Add to testsuite with PackageImports" ("cabal-add-testdata" </> "tests")
        (generateAddDependencyTestSession "tests.cabal" ("test" </> "MainPackageImports.hs") "split" [731])
    , runHaskellTestCaseSession "Add to benchmark" ("cabal-add-testdata" </> "bench")
        (generateAddDependencyTestSession "bench.cabal" ("bench" </> "Main.hs") "split" [403])

    , runHaskellTestCaseSession "Add to executable, multiple targets" ("cabal-add-testdata" </> "multitarget")
        (generateAddDependencyTestSession "multitarget.cabal" ("src" </> "Main.hs") "split" [269])
    , runHaskellTestCaseSession "Add to library, multiple targets" ("cabal-add-testdata" </> "multitarget")
        (generateAddDependencyTestSession "multitarget.cabal" ("lib" </> "MyLib.hs") "split" [413])
    , runHaskellTestCaseSession "Add to internal library, multiple targets" ("cabal-add-testdata" </> "multitarget")
        (generateAddDependencyTestSession "multitarget.cabal" ("lib" </> "InternalLib.hs") "split" [413])
    , runHaskellTestCaseSession "Add to testsuite, multiple targets" ("cabal-add-testdata" </> "multitarget")
        (generateAddDependencyTestSession "multitarget.cabal" ("test" </> "Main.hs") "split" [655])
    , runHaskellTestCaseSession "Add to benchmark, multiple targets" ("cabal-add-testdata" </> "multitarget")
        (generateAddDependencyTestSession "multitarget.cabal" ("bench" </> "Main.hs") "split" [776])

    , runHaskellTestCaseSession "Guard against HPack" ("cabal-add-testdata" </> "packageYaml")
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
        let diags = map (\msg -> messageToDiagnostic msg) messages
            suggestions' = map (safeHead . hiddenPackageSuggestion) diags
            assertions   = zipWith (@?=) suggestions' (map Just suggestions)
            testNames    = map (\(f, s) -> "Check if " ++ T.unpack f ++ (if s == "" then "" else "-") ++ T.unpack s ++ " was parsed correctly") suggestions
            test         = testGroup testTitle $ zipWith testCase testNames assertions
        in test
    messageToDiagnostic :: T.Text -> Diagnostic
    messageToDiagnostic msg = Diagnostic {
            J._range    = mkRange 0 0 0 0
          , J._severity = Nothing
          , J._code     = Nothing
          , J._source   = Nothing
          , J._message  = msg
          , J._relatedInformation = Nothing
          , J._tags     = Nothing
          , J._codeDescription = Nothing
          , J._data_ = Nothing
        }

    generatePackageYAMLTestSession :: FilePath -> Session ()
    generatePackageYAMLTestSession haskellFile = do
        hsdoc <- openDoc haskellFile "haskell"
        _ <- waitForDiagnosticsFrom hsdoc
        cas <- Maybe.mapMaybe (^? _R) <$> getAllCodeActions hsdoc
        let selectedCas = filter (\ca -> "Add dependency" `T.isPrefixOf` (ca ^. L.title)) cas
        liftIO $ assertEqual "PackageYAML" [] selectedCas
