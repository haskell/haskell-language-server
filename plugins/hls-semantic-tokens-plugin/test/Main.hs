{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Lens                       ((^?))
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson                         (KeyValue (..), Value (..),
                                                     object)
import           Data.Default
import           Data.Functor                       (void)
import           Data.Map                           as Map hiding (map)
import           Data.String                        (fromString)
import           Data.Text                          hiding (length, map,
                                                     unlines)
import qualified Data.Text                          as Text
import qualified Data.Text.Utf16.Rope               as Rope
import           Development.IDE.Plugin.Test        (WaitForIdeRuleResult (..))
import           Development.IDE.Test               (waitForBuildQueue)
import           Ide.Plugin.SemanticTokens
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import           Language.LSP.Protocol.Types        (SemanticTokenTypes (..),
                                                     SemanticTokensParams (..),
                                                     _L)
import           Language.LSP.Test                  (Session (..),
                                                     SessionConfig (ignoreConfigurationRequests),
                                                     openDoc)
import qualified Language.LSP.Test                  as Test
import           Language.LSP.VFS                   (VirtualFile (..))
import           System.FilePath
import qualified Test.Hls                           as Test
import           Test.Hls                           (PluginTestDescriptor,
                                                     TestName, TestTree,
                                                     TextDocumentIdentifier,
                                                     defaultTestRunner,
                                                     documentContents, fullCaps,
                                                     goldenGitDiff,
                                                     mkPluginTestDescriptor,
                                                     pluginTestRecorder,
                                                     runSessionWithServerInTmpDir,
                                                     runSessionWithServerInTmpDir',
                                                     testCase, testGroup,
                                                     waitForAction, (@?=))
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.FileSystem                (file, text)

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

goldenWithHaskellAndCapsOutPut config plugin title tree path desc act =
  goldenGitDiff title (FS.vftOriginalRoot tree </> path <.> desc) $
    runSessionWithServerInTmpDir config plugin tree $
      fromString <$> do
        doc <- openDoc (path <.> "hs") "haskell"
        void waitForBuildQueue
        act doc

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
  xs  <- map fromLspTokenTypeStrict <$> docLspSemanticTokensString doc
  return $ unlines . map show $ xs

docLspSemanticTokensString :: TextDocumentIdentifier -> Session [SemanticTokenOriginal Language.LSP.Protocol.Types.SemanticTokenTypes]
docLspSemanticTokensString doc = do
  res <- Test.getSemanticTokens doc
  textContent <- documentContents doc
  let vfs = VirtualFile 0 0 (Rope.fromText textContent)
  let expect = []
  case res ^? Language.LSP.Protocol.Types._L of
    Just tokens -> do
      either (error . show) pure $ recoverLspSemanticTokens vfs tokens
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

mkSemanticConfig :: Value -> Config
mkSemanticConfig setting = def{plugins = Map.insert "SemanticTokens" conf (plugins def)}
    where
      conf = def{plcConfig = (\(Object obj) -> obj) setting }

modifySemantic :: Value -> Session ()
modifySemantic setting = Test.setHlsConfig $ mkSemanticConfig setting


directFile :: FilePath -> Text -> [FS.FileTree]
directFile fp content =
  [ FS.directCradle [Text.pack fp]
  , file fp (text content)
  ]

semanticTokensConfigTest :: TestTree
semanticTokensConfigTest = testGroup "semantic token config test" [
        testCase "function to variable" $ do
            let content = Text.unlines ["module Hello where", "go _ = 1"]
            let fs = mkFs $ directFile "Hello.hs" content
            let funcVar = object [ "tokenMapping" .= object ["function" .= var] ]
                var :: String
                var = "variable"
            do
                recorder <- pluginTestRecorder
                Test.Hls.runSessionWithServerInTmpDir' (semanticTokensPlugin recorder)
                    (mkSemanticConfig funcVar)
                    def {ignoreConfigurationRequests = False}
                    fullCaps
                    fs $ do
                    -- modifySemantic funcVar
                    void waitForBuildQueue
                    doc <- openDoc "Hello.hs" "haskell"
                    void waitForBuildQueue
                    result1 <- docLspSemanticTokensString doc
                    liftIO $ unlines (map show result1) @?= "2:1-3 SemanticTokenTypes_Variable \"go\"\n"
    ]

semanticTokensTests :: TestTree
semanticTokensTests =
  testGroup
    "other semantic Token test"
    [ testCase "module import test" $ do
        let file1 = "TModuleA.hs"
        let file2 = "TModuleB.hs"
        let expect =
              [ SemanticTokenOriginal TVariable (Loc 5 1 2) "go",
                SemanticTokenOriginal TDataCon (Loc 5 6 4) "Game"
              ]
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1, file2]) $ do
          doc1 <- openDoc file1 "haskell"
          doc2 <- openDoc file2 "haskell"
          _check1 <- waitForAction "TypeCheck" doc1
          check2 <- waitForAction "TypeCheck" doc2
          case check2 of
            Right (WaitForIdeRuleResult _) -> return ()
            Left _                         -> error "TypeCheck2 failed"

          textContent2 <- documentContents doc2
          let vfs = VirtualFile 0 0 (Rope.fromText textContent2)
          res2 <- Test.getSemanticTokens doc2
          case res2 ^? Language.LSP.Protocol.Types._L of
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

semanticTokensDataTypeTests :: TestTree
semanticTokensDataTypeTests =
  testGroup
    "get semantic Tokens"
    [ goldenWithSemanticTokens "simple datatype" "TDataType",
      goldenWithSemanticTokens "record" "TRecord",
      goldenWithSemanticTokens "datatype import" "TDatatypeImported",
      goldenWithSemanticTokens "datatype family" "TDataFamily",
      goldenWithSemanticTokens "GADT" "TGADT"
    ]

semanticTokensFunctionTests :: TestTree
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
        semanticTokensFunctionTests,
        semanticTokensConfigTest
      ]
