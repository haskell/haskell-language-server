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
import qualified Development.IDE.Plugin.Completions.Types       as Ghcide
import qualified Distribution.Fields                            as Syntax
import           Distribution.PackageDescription                (GenericPackageDescription)
import           Distribution.PackageDescription.Parsec         (parseGenericPackageDescriptionMaybe)
import qualified Distribution.Parsec.Position                   as Syntax
import           Ide.Plugin.Cabal.Completion.Completer.FilePath
import           Ide.Plugin.Cabal.Completion.Completer.Module
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple   (importCompleter)
import           Ide.Plugin.Cabal.Completion.Completer.Types    (CompleterData (..))
import           Ide.Plugin.Cabal.Completion.Types              (CabalPrefixInfo (..),
                                                                 StanzaName)
import           Ide.Plugin.CabalProject.Completion.Completions
import qualified Language.LSP.Protocol.Lens                     as L
import           System.FilePath
import           Test.Hls
import           Utils

completerTests :: TestTree
completerTests =
  testGroup
    "Completer Tests"
    [ basicCompleterTests,
      fileCompleterTests,
      filePathCompletionContextTests
    --   directoryCompleterTests,
    --   completionHelperTests,
    --   filePathExposedModulesTests,
    --   exposedModuleCompleterTests,
    --   importCompleterTests,
    --   autogenFieldCompletionTests
    ]

basicCompleterTests :: TestTree
basicCompleterTests =
  testGroup
    "Basic Completer Tests"
    [ runCabalProjectTestCaseSession "In stanza context - stanza should not be suggested" "" $ do
        doc <- openDoc "cabal.completer.project" "cabal-project"
        compls <- getCompletions doc (Position 1 4)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "does not suggest packages" $ "packages" `notElem` complTexts
        liftIO $ assertBool "suggests program-prefix keyword" $ "program-prefix:" `elem` complTexts
    , runCabalProjectTestCaseSession "In top level context - stanza should be suggested" "" $ do
        doc <- openDoc "cabal.completer.project" "cabal-project"
        compls <- getCompletions doc (Position 5 2)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "suggests package" $ "package" `elem` complTexts
    , runCabalProjectTestCaseSession "In top level context - stanza should be suggested" "" $ do
        doc <- openDoc "cabal.completer.project" "cabal-project"
        compls <- getCompletions doc (Position 3 2)
        let complTexts = getTextEditTexts compls
        liftIO $ assertBool "suggests program-options" $ "program-options" `elem` complTexts
    ]
    where
      getTextEditTexts :: [CompletionItem] -> [T.Text]
      getTextEditTexts compls = mapMaybe (^? L.textEdit . _Just . _L . L.newText) compls

fileCompleterTests :: TestTree
fileCompleterTests =
  testGroup
    "File Completer Tests"
    [ testCase "Current Directory - no leading ./ by default" $ do
        completions <- completeFilePath "" filePathComplTestDir
        completions @?== ["Content.hs", "dir1/", "dir2/", "textfile.txt", "test.cabal", "cabal.project"],
      testCase "Current Directory - alternative writing" $ do
        completions <- completeFilePath "./" filePathComplTestDir
        completions @?== ["./Content.hs", "./dir1/", "./dir2/", "./textfile.txt", "./test.cabal", "./cabal.project"],
      testCase "Current Directory - hidden file start" $ do
        completions <- completeFilePath "." filePathComplTestDir
        completions @?== ["Content.hs", "textfile.txt", "test.cabal", "cabal.project"],
      testCase "Current Directory - incomplete directory path written" $ do
        completions <- completeFilePath "di" filePathComplTestDir
        completions @?== ["dir1/", "dir2/"],
      testCase "Current Directory - incomplete filepath written" $ do
        completions <- completeFilePath "te" filePathComplTestDir
        completions @?== ["Content.hs", "textfile.txt", "test.cabal"],
      testCase "Subdirectory" $ do
        completions <- completeFilePath "dir1/" filePathComplTestDir
        completions @?== ["dir1/f1.txt", "dir1/f2.hs"],
    --   testCase "Subdirectory - incomplete filepath written" $ do
    --     completions <- completeFilePath "dir2/dir3/MA" filePathComplTestDir
    --     completions @?== ["dir2/dir3/MARKDOWN.md"],
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
        compls @?== ["Content.hs", "dir1/", "dir2/", "textfile.txt", "test.cabal", "cabal.project"],
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

mkCompleterData :: CabalPrefixInfo -> CompleterData
mkCompleterData prefInfo = CompleterData {getLatestGPD = undefined, cabalPrefixInfo = prefInfo, stanzaName = Nothing}

extract :: CompletionItem -> T.Text
extract item = case item ^. L.textEdit of
  Just (InL v) -> v ^. L.newText
  _            -> error ""
