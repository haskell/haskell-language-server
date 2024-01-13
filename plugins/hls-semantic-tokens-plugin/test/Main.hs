{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Arrow                      (Arrow ((***)), (&&&),
                                                     (+++))
import           Control.Lens                       hiding (use, (<.>))
import           Control.Monad                      (forM)
import           Control.Monad.IO.Class             (liftIO)
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
import qualified Data.Text.Utf16.Rope               as Rope
import           Development.IDE                    (getFileContents, runAction,
                                                     toNormalizedUri)
import           Development.IDE.Core.Rules         (Log)
import           Development.IDE.Core.Shake         (getVirtualFile)
import           Development.IDE.Plugin.Test        (WaitForIdeRuleResult (..))
import           Development.IDE.Test               (waitForBuildQueue)
import           Ide.Plugin.Error                   (getNormalizedFilePathE)
import           Ide.Plugin.SemanticTokens
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens         as L
import           Language.LSP.Protocol.Types        (SemanticTokens (..),
                                                     SemanticTokensParams (..),
                                                     _L, type (|?) (..))
import qualified Language.LSP.Server                as Lsp
import           Language.LSP.Test                  (Session (..), openDoc)
import qualified Language.LSP.Test                  as Test
import           Language.LSP.VFS                   (VirtualFile (..))
import           System.Environment.Blank
import           System.FilePath
import           Test.Hls                           (PluginTestDescriptor,
                                                     Session (..), TestName,
                                                     TestTree,
                                                     TextDocumentIdentifier,
                                                     defaultTestRunner,
                                                     documentContents,
                                                     goldenGitDiff,
                                                     mkPluginTestDescriptor,
                                                     mkPluginTestDescriptor',
                                                     runSessionWithServerInTmpDir,
                                                     testCase, testGroup,
                                                     waitForAction, (@?=))
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.Util                      (withCanonicalTempDir)

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

semanticTokensPlugin :: Test.Hls.PluginTestDescriptor SemanticLog
semanticTokensPlugin = Test.Hls.mkPluginTestDescriptor enabledSemanticDescriptor "SemanticTokens"
  where
    enabledSemanticDescriptor recorder plId =
      let semanticDescriptor = Ide.Plugin.SemanticTokens.descriptor recorder plId
       in semanticDescriptor
            { pluginConfigDescriptor =
                (pluginConfigDescriptor semanticDescriptor)
                  { configInitialGenericConfig =
                      (configInitialGenericConfig (pluginConfigDescriptor semanticDescriptor))
                        { plcGlobalOn = True
                        }
                  }
            }

mkSemanticTokensParams :: TextDocumentIdentifier -> SemanticTokensParams
mkSemanticTokensParams = SemanticTokensParams Nothing Nothing

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
  textContent <- documentContents doc
  let vfs = VirtualFile 0 0 (Rope.fromText textContent)
  let expect = []
  case res ^? _L of
    Just tokens -> do
      either (error . show) (return . unlines . map show) $ recoverSemanticTokens vfs tokens
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

        let file1 = "TModuleA.hs"
        let file2 = "TModuleB.hs"
        let expect =
              [ SemanticTokenOriginal TVariable (Loc 5 1 2) "go",
                SemanticTokenOriginal TDataCon (Loc 5 6 4) "Game"
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
          textContent2 <- documentContents doc2
          let vfs = VirtualFile 0 0 (Rope.fromText textContent2)
          case res2 ^? _L of
            Just tokens -> do
              either
                (error . show)
                (\xs -> liftIO $ xs @?= expect)
                $ recoverSemanticTokens vfs tokens
              return ()
            _ -> error "No tokens found"
          liftIO $ 1 @?= 1,
      goldenWithSemanticTokens "mixed constancy test result generated from one ghc version" "T1",
      goldenWithSemanticTokens "pattern bind" "TPatternSyn",
      goldenWithSemanticTokens "type family" "TTypefamily",
      goldenWithSemanticTokens "TUnicodeSyntax" "TUnicodeSyntax"
    ]

semanticTokensDataTypeTests =
  testGroup
    "get semantic Tokens"
    [ goldenWithSemanticTokens "simple datatype" "TDataType",
      goldenWithSemanticTokens "record" "TRecord",
      goldenWithSemanticTokens "record" "TRecordDuplicateRecordFields",
      goldenWithSemanticTokens "datatype import" "TDatatypeImported",
      goldenWithSemanticTokens "datatype family" "TDataFamily",
      goldenWithSemanticTokens "GADT" "TGADT"
    ]

semanticTokensFunctionTests =
  testGroup
    "get semantic of functions"
    [ goldenWithSemanticTokens "functions" "TFunction",
      goldenWithSemanticTokens "local functions" "TFunctionLocal",
      goldenWithSemanticTokens "function in let binding" "TFunctionLet",
      goldenWithSemanticTokens "negative case non-function with constraint" "TNoneFunctionWithConstraint"
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
