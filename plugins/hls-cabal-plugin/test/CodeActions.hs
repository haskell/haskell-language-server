{-# LANGUAGE OverloadedStrings #-}

module CodeActions where

import           Control.Lens                 ((^.))
import           Control.Monad.Trans.Except   (runExceptT)
import qualified Data.ByteString              as BS
import qualified Data.Map                     as M
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as Encoding
import qualified Data.Text.IO                 as T
import           Ide.Plugin.Cabal.CodeActions
import qualified Language.LSP.Protocol.Lens   as JL
import qualified Language.LSP.Protocol.Types  as J
import qualified Language.LSP.Protocol.Types  as LSP
import           System.FilePath              (normalise, (</>))
import           Test.Hls                     (CodeAction, TestTree, TextEdit,
                                               assertFailure, goldenGitDiff,
                                               testCase, testGroup, (@?=))
import qualified Text.Cabal.Parser            as CP
import qualified Text.Cabal.Types             as CP (myPretty)
import           Utils

moduleCodeActionTests :: TestTree
moduleCodeActionTests =
  testGroup
    "Module Code Action Tests"
    [ collectModuleInsertionOptionsTests
    ]

collectModuleInsertionOptionsTests :: TestTree
collectModuleInsertionOptionsTests  =
  testGroup
    "Collect Module Insertion Options Tests"
    [ testCase "empty file" $ do
        codeActions <- callCollectModuleInsertionOptions [""]
        codeActions @?= [],
      goldenGitDiff "library - empty exposed" (testDataDir </> "add-mods" </> "lib.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "lib.cabal" "./src/NewModule.hs"
         let te = getTEWithLabel "library" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      goldenGitDiff "test suite - existing modules in same line with commas" (testDataDir </> "add-mods" </> "test-suite.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "test-suite.cabal" "./src/NewModule.hs"
         let te = getTEWithLabel "test-suite testiebestie" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      goldenGitDiff "library - multiline modules" (testDataDir </> "add-mods" </> "lib-multiline.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "lib-multiline.cabal" "./src/NewModule.hs"
         let te = getTEWithLabel "library" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      goldenGitDiff "executable - multiline" (testDataDir </> "add-mods" </> "executable-multiline.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "executable-multiline.cabal" "test/preprocessor/NewModule.hs"
         let te = getTEWithLabel "executable exeName" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      goldenGitDiff "lib - multiline - first element" (testDataDir </> "add-mods" </> "lib-first-multiline.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "lib-first-multiline.cabal" "./src/NewModule.hs"
         let te = getTEWithLabel "library" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      goldenGitDiff "real file - lib" (testDataDir </> "add-mods" </> "real-cabal-lib.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "real-cabal.cabal" "./src/NewModule.hs"
         let te = getTEWithLabel "library" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      goldenGitDiff "real file - lib" (testDataDir </> "add-mods" </> "real-cabal-test-suite.golden.cabal") $ do
         (codeActions, contents) <- callCollectModuleInsertionOptionsFromFp "./real-cabal.cabal" "./test/NewModule.hs"
         let te = getTEWithLabel "test-suite tests" codeActions
         let newContents = LSP.applyTextEdit te contents
         pure $ BS.fromStrict $ Encoding.encodeUtf8 newContents,
      testCase "empty source dirs - lowercase dir" $ do
         (codeActions, _) <- callCollectModuleInsertionOptionsFromFp "./empty-source-dirs.cabal" "./test/NewModule.hs"
         -- no code actions are found, since the test directory is lowercase
         codeActions @?= []
    ]
  where
    callCollectModuleInsertionOptionsFromFp :: FilePath -> FilePath -> IO ([J.CodeAction], T.Text)
    callCollectModuleInsertionOptionsFromFp relCabalFp relModulePath = do
      let cabalFp = normalise $ testDataDir </> "add-mods" </> relCabalFp
          moduleFp = normalise $ testDataDir </> "add-mods" </> relModulePath
      contents <- T.readFile cabalFp
      let eitherAST = CP.parseCabalFile cabalFp contents
      case eitherAST of
        Right ast -> do
            Right options <- runExceptT $ collectModuleInsertionOptions mempty cabalFp (LSP.filePathToUri moduleFp) ast
            pure (options, contents)
        Left _ -> assertFailure $ "failed to parse cabal file to ast" <> show (CP.myPretty eitherAST)

    callCollectModuleInsertionOptions :: [T.Text] -> IO [J.CodeAction]
    callCollectModuleInsertionOptions ls = do
      let eitherAST = CP.parseCabalFile "./myFile.cabal" $ T.unlines ls
      case eitherAST of
        Right ast -> do
            Right options <- runExceptT $ collectModuleInsertionOptions mempty "./myFile.cabal" (LSP.filePathToUri $ testDataDir </> "add-mods" </> "src" </> "NewModule.hs") ast
            pure options
        _ -> assertFailure "failed to parse cabal file to ast"

    getTEWithLabel :: T.Text -> [CodeAction] -> TextEdit
    getTEWithLabel label codeActions = head $ head $ M.elems $ fromJust (fromJust (extractCa ^. JL.edit) ^. JL.changes)
      where
        extractCa = head $ filter (\x -> label `T.isInfixOf` (x ^. JL.title)) codeActions
