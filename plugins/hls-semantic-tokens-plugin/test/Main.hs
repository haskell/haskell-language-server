{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow                      (Arrow ((***)), (&&&),
                                                     (+++))
import           Control.Lens                       hiding (use, (<.>))
import           Control.Monad                      (forM)
import           Data.Bifunctor
import qualified Data.ByteString                    as BS
import           Data.Data
import           Data.Default
import           Data.Functor                       (void)
import qualified Data.List                          as List
import           Data.Map                           as Map hiding (map)
import           Data.Maybe                         (fromJust)
import qualified Data.Maybe
import qualified Data.Set                           as Set
import           Data.String                        (fromString)
import           Data.Text                          hiding (length, map,
                                                     unlines)
import           Development.IDE                    (getFileContents, runAction)
import           Development.IDE.Core.Rules         (Log)
import           Development.IDE.Plugin.Test        (WaitForIdeRuleResult (..))
import           Ide.Plugin.Error                   (getNormalizedFilePathE)
import           Ide.Plugin.SemanticTokens
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens         as L
import           Language.LSP.Protocol.Types        (SemanticTokens (..),
                                                     SemanticTokensParams (..))
import qualified Language.LSP.Test                  as Test
import           System.Environment.Blank
import           System.FilePath
import qualified Test.Hls                           (PluginTestDescriptor,
                                                     mkPluginTestDescriptor',
                                                     runSessionWithServerInTmpDir,
                                                     waitForAction)
import           Test.Hls
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.Util                      (withCanonicalTempDir)

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

semanticTokensPlugin :: Test.Hls.PluginTestDescriptor Log
semanticTokensPlugin = Test.Hls.mkPluginTestDescriptor Ide.Plugin.SemanticTokens.descriptor "SemanticTokens"

mkSemanticTokensParams :: TextDocumentIdentifier -> SemanticTokensParams
mkSemanticTokensParams = SemanticTokensParams Nothing Nothing

runSessionWithServerInDir file x =
  Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProject file) $ do
    doc <- openDoc file "haskell"
    res <- waitForAction "TypeCheck" doc
    x doc

runSessionWithServerInDirAndGetSemantic file x =
  Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProject file) $ do
    doc <- openDoc file "haskell"
    res <- waitForAction "TypeCheck" doc
    res <- Test.getSemanticTokens doc
    x res doc

runSessionWithServerInDirAndGetSemanticsFile file x =
  do
    let path = "." </> testDataDir </> file
    content <- liftIO $ readFile path
    Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProject file) $ do
      doc <- openDoc file "haskell"
      res <- waitForAction "TypeCheck" doc
      res <- Test.getSemanticTokens doc
      x res doc content

goldenWithHaskellAndCapsOutPut config plugin title tree path desc act =
  goldenGitDiff title (FS.vftOriginalRoot tree </> path <.> desc) $
    runSessionWithServerInTmpDir config plugin tree $
      fromString <$> do
        doc <- openDoc (path <.> "hs") "haskell"
        void waitForBuildQueue
        r <- act doc
        return r

goldenWithSemanticTokens :: TestName -> FilePath -> TestTree
goldenWithSemanticTokens title path =
  goldenWithHaskellAndCapsOutPut
    def
    semanticTokensPlugin
    title
    (mkFs $ FS.directProject (path <.> "hs"))
    path
    "expected"
    docSemanticTokensString

docSemanticTokensString :: TextDocumentIdentifier -> Session String
docSemanticTokensString doc = do
  res <- Test.getSemanticTokens doc
  content <- unpack <$> documentContents doc
  let expect = []
  case res ^? _L of
    Just tokens -> do
      either (error . show) (return . unlines . map show) $ recoverSemanticTokens content tokens
    _noTokens -> error "No tokens found"

semanticTokensImportedTests :: TestTree
semanticTokensImportedTests =
  testGroup
    "imported test"
    [ goldenWithSemanticTokens "type class" "TClass"
    ]

semanticTokensClassTests :: TestTree
semanticTokensClassTests =
  testGroup
    "type class"
    [ goldenWithSemanticTokens "golden type class" "TClass",
      goldenWithSemanticTokens "imported class method InstanceClassMethodBind" "TInstanceClassMethodBind",
      goldenWithSemanticTokens "imported class method TInstanceClassMethodUse" "TInstanceClassMethodUse",
      goldenWithSemanticTokens "imported deriving" "TClassImportedDeriving"
    ]

semanticTokensValuePatternTests :: TestTree
semanticTokensValuePatternTests =
  testGroup
    "value and patterns "
    [ goldenWithSemanticTokens "value bind" "TValBind",
      goldenWithSemanticTokens "pattern match" "TPatternMatch",
      goldenWithSemanticTokens "pattern bind" "TPatternbind"
    ]

semanticTokensTests :: TestTree
semanticTokensTests =
  testGroup
    "other semantic Token test"
    [ testCase "module import test" $ do
        let filePath1 = "./test/testdata/TModuleA.hs"
        let filePath2 = "./test/testdata/TModuleB.hs"
        content1 <- liftIO $ Prelude.readFile filePath1
        content2 <- liftIO $ Prelude.readFile filePath2

        let file1 = "TModuleA.hs"
        let file2 = "TModuleB.hs"
        let expect =
              [ SemanticTokenOriginal {tokenType = TVariable, loc = Loc {line = 5, startChar = 1, len = 2}, name = "go"},
                SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 5, startChar = 6, len = 4}, name = "Game"}
              ]
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1, file2]) $ do
          doc1 <- openDoc file1 "haskell"
          doc2 <- openDoc file2 "haskell"
          check1 <- waitForAction "TypeCheck" doc1
          check2 <- waitForAction "TypeCheck" doc2
          case check2 of
            Right (WaitForIdeRuleResult x) -> return ()
            Left y                         -> error "TypeCheck2 failed"

          res2 <- Test.getSemanticTokens doc2
          case res2 ^? _L of
            Just tokens -> do
              either
                (error . show)
                (\xs -> liftIO $ xs @?= expect)
                $ recoverSemanticTokens content2 tokens
              return ()
            _ -> error "No tokens found"
          liftIO $ 1 @?= 1,
      goldenWithSemanticTokens "mixed constancy test result generated from one ghc version" "T1",
      goldenWithSemanticTokens "pattern bind" "TPatternSyn",
      goldenWithSemanticTokens "type family" "TTypefamily"
    ]

semanticTokensDataTypeTests =
  testGroup
    "get semantic Tokens"
    [ goldenWithSemanticTokens "simple datatype" "TDataType",
      goldenWithSemanticTokens "record" "TRecord",
      goldenWithSemanticTokens "datatype import" "TDatatypeImported",
      goldenWithSemanticTokens "datatype family" "TDataFamily",
      goldenWithSemanticTokens "GADT" "TGADT"
    ]

semanticTokensFunctionTests =
  testGroup
    "get semantic of functions"
    [ goldenWithSemanticTokens "functions" "TFunction",
      goldenWithSemanticTokens "local functions" "TFunctionLocal",
      goldenWithSemanticTokens "function in let binding" "TFunctionLet"
    ]

main :: IO ()
main =
  defaultTestRunner $
    testGroup
      "Semantic tokens"
      [ semanticTokensTests,
        semanticTokensClassTests,
        semanticTokensDataTypeTests,
        semanticTokensValuePatternTests,
        semanticTokensFunctionTests
      ]
