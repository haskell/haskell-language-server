{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}


module Completer where

import           Control.Lens                                   ((^.), (^?))
import           Control.Lens.Prism
import           Control.Monad                                  (forM_)
import qualified Data.ByteString                                as ByteString
import qualified Data.ByteString.Char8                          as BS8
import           Data.Maybe                                     (mapMaybe)
import qualified Data.Text                                      as T
import           Development.IDE.Plugin.Completions.Types       (cursorPos)
import qualified Development.IDE.Plugin.Completions.Types       as Ghcide
import qualified Distribution.Fields                            as Syntax
import           Distribution.PackageDescription                (GenericPackageDescription)
import           Distribution.PackageDescription.Parsec         (parseGenericPackageDescriptionMaybe)
import qualified Distribution.Parsec.Position                   as Syntax
import           Ide.Plugin.Cabal.Completion.Completer.FilePath
import           Ide.Plugin.Cabal.Completion.Completer.Module
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple   (importCompleter)
import           Ide.Plugin.Cabal.Completion.Completer.Types    (CompleterData (..),
                                                                 Matcher (..))
import           Ide.Plugin.Cabal.Completion.Completions
import           Ide.Plugin.Cabal.Completion.Types              (CabalPrefixInfo (..),
                                                                 StanzaName)
import qualified Language.LSP.Protocol.Lens                     as L
import           System.FilePath
import           Test.Hls
import           Test.Hls.FileSystem                            (file,
                                                                 mkVirtualFileTree,
                                                                 text)
import qualified Text.Fuzzy.Parallel                            as Fuzzy
import           Utils

completerTests :: TestTree
completerTests =
  testGroup
    "Completer Tests"
    [ basicCompleterTests,
      fileCompleterTests,
      filePathCompletionContextTests,
      directoryCompleterTests,
      completionHelperTests,
      filePathExposedModulesTests,
      exposedModuleCompleterTests,
      importCompleterTests,
      autogenFieldCompletionTests
    ]

basicCompleterTests :: TestTree
basicCompleterTests =
  testGroup
    "Basic Completer Tests"
    [ runCabalTestCaseSession "In stanza context - stanza should not be suggested" "" $ do
        doc <- openDoc "completer.cabal" "cabal"
        compls <- getCompletions doc (Position 11 7)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "does not suggest library" $ "library" `notElem` complTexts
        liftIO $ assertBool "suggests library keyword" $ "extra-libraries:" `elem` complTexts
    , runCabalTestCaseSession "In top level context - stanza should be suggested" "" $ do
        doc <- openDoc "completer.cabal" "cabal"
        compls <- getCompletions doc (Position 8 2)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "suggests benchmark" $ "benchmark" `elem` complTexts
    , runCabalTestCaseSession "In top level context - stanza should be suggested" "" $ do
        doc <- openDoc "completer.cabal" "cabal"
        compls <- getCompletions doc (Position 13 2)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "suggests common" $ "common" `elem` complTexts
    , runCabalTestCaseSession "Main-is completions should be relative to hs-source-dirs of same stanza" "filepath-completions" $ do
        doc <- openDoc "main-is.cabal" "cabal"
        compls <- getCompletions doc (Position 10 12)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "suggests f2" $ "f2.hs" `elem` complTexts
        liftIO $ assertBool "does not suggest" $ "Content.hs" `notElem` complTexts
    , parameterisedCursorTestM "extensions completion" libraryStanzaData
        [ \_ actual -> assertBool "suggests FieldSelectors" $ "FieldSelectors" `elem` actual
        , \_ actual -> assertBool "suggests OverloadedStrings" $ "OverloadedStrings" `elem` actual
        , \_ actual -> assertBool "suggests something" $ not . null $ actual
        , \_ actual -> assertBool "suggests NoLambdaCase" $ "NoLambdaCase" `elem` actual
        , \_ actual -> assertBool "suggests RecordWildCards" $ "RecordWildCards" `elem` actual
        ]
        $ \fileContent posPrefInfo -> do
            let vFileTree = mkVirtualFileTree "" $ [file "cabalFile.cabal" $ text fileContent]
            runCabalSessionVft vFileTree $ do
              doc <- openDoc "cabalFile.cabal" "cabal"
              compls <- getCompletions doc (cursorPos posPrefInfo)
              let complTexts = getTextEditTexts compls
              pure complTexts]
    where
      getTextEditTexts :: [CompletionItem] -> [T.Text]
      getTextEditTexts compls = mapMaybe (^? L.textEdit . _Just . _L . L.newText) compls

fileCompleterTests :: TestTree
fileCompleterTests =
  testGroup
    "File Completer Tests"
    [ testCase "Current Directory - no leading ./ by default" $ do
        completions <- completeFilePath "" filePathComplTestDir
        completions @?== [".hidden", "Content.hs", "dir1/", "dir2/", "textfile.txt", "main-is.cabal"],
      testCase "Current Directory - alternative writing" $ do
        completions <- completeFilePath "./" filePathComplTestDir
        completions @?== ["./.hidden", "./Content.hs", "./dir1/", "./dir2/", "./textfile.txt", "./main-is.cabal"],
      testCase "Current Directory - hidden file start" $ do
        completions <- completeFilePath "." filePathComplTestDir
        completions @?== ["Content.hs", ".hidden", "textfile.txt", "main-is.cabal"],
      testCase "Current Directory - incomplete directory path written" $ do
        completions <- completeFilePath "di" filePathComplTestDir
        completions @?== ["dir1/", "dir2/"],
      testCase "Current Directory - incomplete filepath written" $ do
        completions <- completeFilePath "te" filePathComplTestDir
        completions @?== ["Content.hs", "textfile.txt"],
      testCase "Subdirectory" $ do
        completions <- completeFilePath "dir1/" filePathComplTestDir
        completions @?== ["dir1/f1.txt", "dir1/f2.hs"],
      testCase "Subdirectory - incomplete filepath written" $ do
        completions <- completeFilePath "dir2/dir3/MA" filePathComplTestDir
        completions @?== ["dir2/dir3/MARKDOWN.md"],
      testCase "Nonexistent directory" $ do
        completions <- completeFilePath "dir2/dir4/" filePathComplTestDir
        completions @?== []
    ]
  where
    completeFilePath :: T.Text -> TestName -> IO [T.Text]
    completeFilePath written dirName = do
      completer <- filePathCompleter mempty $ mkCompleterData $ simpleCabalPrefixInfoFromFp written dirName
      pure $ fmap extract completer

filePathCompletionContextTests :: TestTree
filePathCompletionContextTests =
  testGroup
    "File Path Completion Context Tests"
    [ testCase "empty file - start" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "" 0 0)
        completionPrefix complContext @?= "",
      testCase "only whitespaces" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   " 0 3)
        completionPrefix complContext @?= "",
      testCase "simple filepath" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   src/" 0 7)
        completionPrefix complContext @?= "src/",
      testCase "simple filepath - starting apostrophe" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   \"src/" 0 8)
        completionPrefix complContext @?= "src/",
      testCase "simple filepath - starting apostrophe, already closed" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "   \"src/\"" 0 8)
        completionPrefix complContext @?= "src/",
      testCase "second filepath - starting apostrophe" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "fp.txt \"src/" 0 12)
        completionPrefix complContext @?= "src/",
      testCase "middle filepath - starting apostrophe" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "fp.txt \"src/ fp2.txt" 0 12)
        completionPrefix complContext @?= "src/",
      testCase "middle filepath - starting apostrophe, already closed" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "fp.t xt \"src\" fp2.txt" 0 12)
        completionPrefix complContext @?= "src",
      testCase "middle filepath - starting apostrophe, already closed" $ do
        let complContext = getCabalPrefixInfo "" (simplePosPrefixInfo "\"fp.txt\" \"src fp2.txt" 0 13)
        completionPrefix complContext @?= "src",
      testCase "Current Directory" $ do
        compls <-
          listFileCompletions
            mempty
            PathCompletionInfo
              { isStringNotationPath = Nothing,
                pathSegment = "",
                queryDirectory = "",
                workingDirectory = filePathComplTestDir
              }
        compls @?== [".hidden", "Content.hs", "dir1/", "dir2/", "textfile.txt", "main-is.cabal"],
      testCase "In directory" $ do
        compls <-
          listFileCompletions
            mempty
            PathCompletionInfo
              { isStringNotationPath = Nothing,
                pathSegment = "",
                queryDirectory = "dir1/",
                workingDirectory = filePathComplTestDir
              }
        compls @?== ["f1.txt", "f2.hs"]
    ]
  where
    simplePosPrefixInfo :: T.Text -> UInt -> UInt -> Ghcide.PosPrefixInfo
    simplePosPrefixInfo lineString linePos charPos =
      Ghcide.PosPrefixInfo
        { Ghcide.fullLine = lineString,
          Ghcide.prefixScope = "",
          Ghcide.prefixText = "",
          Ghcide.cursorPos = Position linePos charPos
        }

directoryCompleterTests :: TestTree
directoryCompleterTests =
  testGroup
    "Directory Completer Tests"
    [ testCase "Current Directory - no leading ./ by default" $ do
        completions <- completeDirectory "" filePathComplTestDir
        completions @?== ["dir1/", "dir2/"],
      testCase "Current Directory - alternative writing" $ do
        completions <- completeDirectory "./" filePathComplTestDir
        completions @?== ["./dir1/", "./dir2/"],
      testCase "Current Directory - incomplete directory path written" $ do
        completions <- completeDirectory "di" filePathComplTestDir
        completions @?== ["dir1/", "dir2/"],
      testCase "Current Directory - incomplete filepath written" $ do
        completions <- completeDirectory "te" filePathComplTestDir
        completions @?== [],
      testCase "Subdirectory - no more directories found" $ do
        completions <- completeDirectory "dir1/" filePathComplTestDir
        completions @?== [],
      testCase "Subdirectory - available subdirectory" $ do
        completions <- completeDirectory "dir2/" filePathComplTestDir
        completions @?== ["dir2/dir3/"],
      testCase "Nonexistent directory" $ do
        completions <- completeDirectory "dir2/dir4/" filePathComplTestDir
        completions @?== []
    ]
  where
    completeDirectory :: T.Text -> TestName -> IO [T.Text]
    completeDirectory written dirName = do
      completer <- directoryCompleter mempty $ mkCompleterData $ simpleCabalPrefixInfoFromFp written dirName
      pure $ fmap extract completer

completionHelperTests :: TestTree
completionHelperTests =
  testGroup
    "Completion Helper Tests"
    [ testCase "get FilePath - partly written file path" $ do
        getFilePathCursorPrefix "src/a" 0 5 @?= "src/a",
      testCase "get FilePath - ignores spaces" $ do
        getFilePathCursorPrefix "  src/a" 0 7 @?= "src/a",
      testCase "get FilePath - ignores spaces and keyword" $ do
        getFilePathCursorPrefix "license-file: src/a" 0 19 @?= "src/a",
      testCase "get FilePath - with apostrophe, ignores spaces and keyword" $ do
        getFilePathCursorPrefix "license-file: \"src/a" 0 20 @?= "src/a",
      testCase "get FilePath - ignores list of filepaths beforehand, space separated" $ do
        getFilePathCursorPrefix "  ./text.txt file.h" 0 19 @?= "file.h",
      testCase "get FilePath - ignores list of filepaths after, space separated" $ do
        getFilePathCursorPrefix "  ./text.t file.h" 0 10 @?= "./text.t",
      testCase "get FilePath - ignores list of filepaths and rest of filepath after, space separated" $ do
        getFilePathCursorPrefix "  ./text.t file.h" 0 6 @?= "./te",
      testCase "get FilePath - ignores list of filepaths beforehand, multiple space separated" $ do
        getFilePathCursorPrefix "  ./text.txt   file.h" 0 21 @?= "file.h",
      testCase "get FilePath - ignores list of filepaths beforehand, comma separated" $ do
        getFilePathCursorPrefix "  ./text.txt, file.h" 0 20 @?= "file.h",
      testCase "get FilePath - ignores list of filepaths beforehand, comma separated, many whitespaces" $ do
        getFilePathCursorPrefix "  ./text.txt,   file.h" 0 22 @?= "file.h",
      testCase "get FilePath - ignores list of filepaths beforehand, comma separated, no whitespace" $ do
        getFilePathCursorPrefix "  ./text.txt,file.h" 0 19 @?= "file.h",
      testCase "get FilePath - with apostrophes, ignores list of filepaths beforehand" $ do
        getFilePathCursorPrefix "  \"./text.txt\" \"file.h" 0 23 @?= "file.h",
      testCase "get FilePath - ignores list of filepaths with apostrophe beforehand" $ do
        getFilePathCursorPrefix "  \"./text.txt\" file.h" 0 22 @?= "file.h"
    ]
  where
    getFilePathCursorPrefix :: T.Text -> UInt -> UInt -> T.Text
    getFilePathCursorPrefix lineString linePos charPos =
      completionPrefix . getCabalPrefixInfo "" $
        Ghcide.PosPrefixInfo
          { Ghcide.fullLine = lineString,
            Ghcide.prefixScope = "",
            Ghcide.prefixText = "",
            Ghcide.cursorPos = Position linePos charPos
          }

filePathExposedModulesTests :: TestTree
filePathExposedModulesTests =
  testGroup
    "Filepaths for Exposed Modules Tests"
    [ testCase "Root dir" $ do
        exposed <- callFilePathsForExposedModules ["./"]
        exposed @?== ["Dir1.", "File1"],
      testCase "Nested path" $ do
        exposed <- callFilePathsForExposedModules ["./Dir1/Dir2/"]
        exposed @?== ["File2"],
      testCase "Nested empty dir" $ do
        exposed <- callFilePathsForExposedModules ["./Dir1/Dir2/Dir4"]
        exposed @?== [],
      testCase "Two dirs" $ do
        exposed <- callFilePathsForExposedModules ["./Dir1/", "Dir1/Dir3/Dir4/"]
        exposed @?== ["Dir2.", "Dir3.", "File3"]
    ]
  where
    callFilePathsForExposedModules :: [FilePath] -> IO [T.Text]
    callFilePathsForExposedModules srcDirs = do
      let prefInfo = simpleCabalPrefixInfoFromFp "" exposedTestDir
      filePathsForExposedModules mempty srcDirs prefInfo $ Matcher $ Fuzzy.simpleFilter Fuzzy.defChunkSize Fuzzy.defMaxResults

exposedModuleCompleterTests :: TestTree
exposedModuleCompleterTests =
  testGroup
    "Exposed Modules Completer Tests"
    [ testCase "Top level single source dir, library" $ do
        completions <- callModulesCompleter Nothing sourceDirsExtractionLibrary ""
        completions @?== ["Dir2.", "Dir3."],
      testCase "Top level single source dir, benchmark, with prefix" $ do
        completions <- callModulesCompleter (Just "benchie") sourceDirsExtractionBenchmark "Fi"
        completions @?== ["File1"],
      testCase "Top level single source dir, named executable" $ do
        completions <- callModulesCompleter (Just "executie") sourceDirsExtractionExecutable ""
        completions @?== ["File1", "Dir1.", "Dir2.", "Dir3."],
      testCase "Top level single source dir, named executable" $ do
        completions <- callModulesCompleter (Just "exe-not-so-cutie") sourceDirsExtractionExecutable ""
        completions @?== ["File2", "Dir4."],
      testCase "Top level single source dir, nonexistent name" $ do
        completions <- callModulesCompleter (Just "exe-the-beste") sourceDirsExtractionExecutable ""
        completions @?== [],
      testCase "Top level single source dir, testsuite, with prefix" $ do
        completions <- callModulesCompleter (Just "suitor") sourceDirsExtractionTestSuite "3"
        completions @?== ["File3"],
      testCase "Name nothing but not library" $ do
        completions <- callModulesCompleter Nothing sourceDirsExtractionTestSuite "3"
        completions @?== []
    ]
  where
    callModulesCompleter :: Maybe StanzaName -> (Maybe StanzaName -> GenericPackageDescription -> [FilePath]) -> T.Text -> IO [T.Text]
    callModulesCompleter sName func prefix = do
      let cData = simpleCompleterData sName testDataDir prefix
      completer <- modulesCompleter func mempty cData
      pure $ fmap extract completer

-- TODO: These tests are a bit barebones at the moment,
-- since we do not take cursorposition into account at this point.
importCompleterTests :: TestTree
importCompleterTests =
  testGroup
    "Import Completer Tests"
    [ testCase "All above common sections are suggested" $ do
      completions <- callImportCompleter
      ("defaults" `elem` completions) @? "defaults contained"
      ("test-defaults" `elem` completions) @? "test-defaults contained"
    -- TODO: Only common sections defined before the current stanza may be imported
    , testCase "Common sections occuring below are not suggested" $ do
      completions <- callImportCompleter
      ("notForLib" `elem` completions) @? "notForLib contained, this needs to be fixed"
    , testCase "All common sections are suggested when curser is below them" $ do
      completions <- callImportCompleter
      completions @?== ["defaults", "notForLib" ,"test-defaults"]
    ]
    where
      callImportCompleter :: IO [T.Text]
      callImportCompleter = do
        let cData' = simpleCompleterData Nothing testDataDir ""
        let cabalCommonSections = [makeCommonSection 13 0 "defaults", makeCommonSection 18 0 "test-defaults", makeCommonSection 27 0 "notForLib"]
        let cData = cData' {getCabalCommonSections = pure $ Just cabalCommonSections}
        completer <- importCompleter mempty cData
        pure $ fmap extract completer
      makeCommonSection :: Int -> Int -> String -> Syntax.Field Syntax.Position
      makeCommonSection row col name =
        Syntax.Section
          (Syntax.Name (Syntax.Position row col) "common")
          [Syntax.SecArgName (Syntax.Position row (col + 7)) (BS8.pack name)]
          []

autogenFieldCompletionTests :: TestTree
autogenFieldCompletionTests =
  testGroup "Autogen Field Completer Tests"
    [ testAutogenField "library"         "completion/autogen-completion.cabal" (Position 6 9)  ["autogen-modules:", "autogen-includes:"]
    , testAutogenField "executable"      "completion/autogen-completion.cabal" (Position 11 9) ["autogen-modules:", "autogen-includes:"]
    , testAutogenField "test-suite"      "completion/autogen-completion.cabal" (Position 16 9) ["autogen-modules:", "autogen-includes:"]
    , testAutogenField "benchmark"       "completion/autogen-completion.cabal" (Position 21 9) ["autogen-modules:", "autogen-includes:"]
    , testAutogenField "common"          "completion/autogen-completion.cabal" (Position 24 9) ["autogen-modules:", "autogen-includes:"]
    ]

  where
    testAutogenField :: String -> FilePath -> Position -> [T.Text] -> TestTree
    testAutogenField section file pos expected = runCabalTestCaseSession ("autogen-modules completion in " <> section) "" $ do
      doc <- openDoc file "cabal"
      items <- getCompletions doc pos
      let labels = map (^. L.label) items
      liftIO $ forM_ expected $ \expect ->
        assertBool (T.unpack expect <> " not found in " <> section) $
          any (expect `T.isInfixOf`) labels

simpleCompleterData :: Maybe StanzaName -> FilePath -> T.Text -> CompleterData
simpleCompleterData sName dir pref = do
  CompleterData
    { cabalPrefixInfo = simpleExposedCabalPrefixInfo pref dir,
      getLatestGPD = do
        cabalContents <- ByteString.readFile $ testDataDir </> "exposed.cabal"
        pure $ parseGenericPackageDescriptionMaybe cabalContents,
      getCabalCommonSections = undefined,
      stanzaName = sName,
      matcher = Matcher $ Fuzzy.simpleFilter Fuzzy.defChunkSize Fuzzy.defMaxResults
    }

mkCompleterData :: CabalPrefixInfo -> CompleterData
mkCompleterData prefInfo =
  CompleterData
    { getLatestGPD = undefined,
      getCabalCommonSections = undefined,
      cabalPrefixInfo = prefInfo,
      stanzaName = Nothing,
      matcher = Matcher $ Fuzzy.simpleFilter Fuzzy.defChunkSize Fuzzy.defMaxResults
    }

exposedTestDir :: FilePath
exposedTestDir = addTrailingPathSeparator $ testDataDir </> "src-modules"

simpleExposedCabalPrefixInfo :: T.Text -> FilePath -> CabalPrefixInfo
simpleExposedCabalPrefixInfo prefix fp =
  CabalPrefixInfo
    { completionPrefix = prefix,
      isStringNotation = Nothing,
      completionCursorPosition = Position 0 0,
      completionRange = Range (Position 0 0) (Position 0 0),
      completionWorkingDir = fp,
      completionFileName = "exposed.cabal"
    }

extract :: CompletionItem -> T.Text
extract item = case item ^. L.textEdit of
  Just (InL v) -> v ^. L.newText
  _            -> error ""

-- ------------------------------------------------------------------------
-- Test Data
-- ------------------------------------------------------------------------

libraryStanzaData :: T.Text
libraryStanzaData = [__i|
    cabal-version:      3.0
    name:               simple-cabal
    common mylib
      default-extensions: Field
                               ^
    library
        default-extensions: Ov
                              ^
    test-suite mysuite
        default-extensions:
                            ^
    executable myexe
        default-extensions: NoLam
                                 ^
    benchmark mybench
        other-extensions: RecordW
                                 ^
|]
