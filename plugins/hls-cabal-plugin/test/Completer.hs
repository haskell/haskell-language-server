{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Completer where

import           Control.Lens                                   ((^.))
import qualified Data.ByteString                                as ByteString
import qualified Data.Text                                      as T
import           Distribution.PackageDescription.Parsec         (parseGenericPackageDescriptionMaybe)
import           Ide.Plugin.Cabal.Completion.Completer.FilePath
import           Ide.Plugin.Cabal.Completion.Completer.Module
import           Ide.Plugin.Cabal.Completion.Completer.Types    (CompleterData (..))
import           Ide.Plugin.Cabal.Completion.Completions
import           Ide.Plugin.Cabal.Completion.Types
import           Ide.Plugin.Cabal.Parse                         (GenericPackageDescription)
import qualified Language.LSP.Protocol.Lens                     as L
import qualified Language.LSP.VFS                               as VFS
import           System.FilePath
import           Test.Hls
import           Utils

completerTests :: TestTree
completerTests =
  testGroup
    "Completer Tests"
    [ fileCompleterTests,
      filePathCompletionContextTests,
      directoryCompleterTests,
      completionHelperTests,
      filePathExposedModulesTests,
      exposedModuleCompleterTests
    ]

fileCompleterTests :: TestTree
fileCompleterTests =
  testGroup
    "File Completer Tests"
    [ testCase "Current Directory" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "" testDir
        completions @?== ["./.hidden", "./Content.hs", "./dir1/", "./dir2/", "./textfile.txt"],
      testCase "Current Directory - alternative writing" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "./" testDir
        completions @?== ["./.hidden", "./Content.hs", "./dir1/", "./dir2/", "./textfile.txt"],
      testCase "Current Directory - hidden file start" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "." testDir
        completions @?== ["./Content.hs", "./.hidden", "./textfile.txt"],
      testCase "Current Directory - incomplete directory path written" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "di" testDir
        completions @?== ["./dir1/", "./dir2/"],
      testCase "Current Directory - incomplete filepath written" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "te" testDir
        completions @?== ["./Content.hs", "./textfile.txt"],
      testCase "Subdirectory" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "dir1/" testDir
        completions @?== ["dir1/f1.txt", "dir1/f2.hs"],
      testCase "Subdirectory - incomplete filepath written" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "dir2/dir3/MA" testDir
        completions @?== ["dir2/dir3/MARKDOWN.md"],
      testCase "Nonexistent directory" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeFilePath "dir2/dir4/" testDir
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
        testDir <- getFilePathComplTestDir
        compls <-
          listFileCompletions
            mempty
            PathCompletionInfo
              { isStringNotationPath = Nothing,
                pathSegment = "",
                queryDirectory = "",
                workingDirectory = testDir
              }
        compls @?== [".hidden", "Content.hs", "dir1/", "dir2/", "textfile.txt"],
      testCase "In directory" $ do
        testDir <- getFilePathComplTestDir
        compls <-
          listFileCompletions
            mempty
            PathCompletionInfo
              { isStringNotationPath = Nothing,
                pathSegment = "",
                queryDirectory = "dir1/",
                workingDirectory = testDir
              }
        compls @?== ["f1.txt", "f2.hs"]
    ]
  where
    simplePosPrefixInfo :: T.Text -> UInt -> UInt -> VFS.PosPrefixInfo
    simplePosPrefixInfo lineString linePos charPos =
      VFS.PosPrefixInfo
        { VFS.fullLine = lineString,
          VFS.prefixModule = "",
          VFS.prefixText = "",
          VFS.cursorPos = Position linePos charPos
        }

directoryCompleterTests :: TestTree
directoryCompleterTests =
  testGroup
    "Directory Completer Tests"
    [ testCase "Current Directory" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "" testDir
        completions @?== ["./dir1/", "./dir2/"],
      testCase "Current Directory - alternative writing" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "./" testDir
        completions @?== ["./dir1/", "./dir2/"],
      testCase "Current Directory - incomplete directory path written" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "di" testDir
        completions @?== ["./dir1/", "./dir2/"],
      testCase "Current Directory - incomplete filepath written" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "te" testDir
        completions @?== [],
      testCase "Subdirectory - no more directories found" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "dir1/" testDir
        completions @?== [],
      testCase "Subdirectory - available subdirectory" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "dir2/" testDir
        completions @?== ["dir2/dir3/"],
      testCase "Nonexistent directory" $ do
        testDir <- getFilePathComplTestDir
        completions <- completeDirectory "dir2/dir4/" testDir
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
        VFS.PosPrefixInfo
          { VFS.fullLine = lineString,
            VFS.prefixModule = "",
            VFS.prefixText = "",
            VFS.cursorPos = Position linePos charPos
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
      cwd <- getExposedTestDir
      let prefInfo = simpleCabalPrefixInfoFromFp "" cwd
      filePathsForExposedModules mempty srcDirs prefInfo

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
    simpleCompleterData :: Maybe StanzaName -> FilePath -> T.Text -> CompleterData
    simpleCompleterData sName dir pref = do
      CompleterData
        { cabalPrefixInfo = simpleExposedCabalPrefixInfo pref dir,
          getLatestGPD = do
            testDir <- getTestDir
            cabalContents <- ByteString.readFile $ testDir </> "exposed.cabal"
            pure $ parseGenericPackageDescriptionMaybe cabalContents,
          stanzaName = sName
        }
    callModulesCompleter :: Maybe StanzaName -> (Maybe StanzaName -> GenericPackageDescription -> [FilePath]) -> T.Text -> IO [T.Text]
    callModulesCompleter sName func prefix = do
      cwd <- getTestDir
      let cData = simpleCompleterData sName cwd prefix
      completer <- modulesCompleter func mempty cData
      pure $ fmap extract completer

mkCompleterData :: CabalPrefixInfo -> CompleterData
mkCompleterData prefInfo = CompleterData {getLatestGPD = undefined, cabalPrefixInfo = prefInfo, stanzaName = Nothing}

getExposedTestDir :: IO FilePath
getExposedTestDir = do
  testDir <- getTestDir
  pure $ addTrailingPathSeparator $ testDir </> "src-modules"

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
