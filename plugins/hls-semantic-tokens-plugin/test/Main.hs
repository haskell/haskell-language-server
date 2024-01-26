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

goldenWithSemanticTokensWithDefaultConfig :: TestName -> FilePath -> TestTree
goldenWithSemanticTokensWithDefaultConfig title path =
  goldenWithHaskellAndCapsOutPut
    def
    semanticTokensPlugin
    title
    (mkFs $ FS.directProject (path <.> "hs"))
    path
    "expected"
    (docSemanticTokensString def)

docSemanticTokensString :: SemanticTokensConfig-> TextDocumentIdentifier -> Session String
docSemanticTokensString cf doc = do
  xs  <- map (lspTokenHsToken cf) <$> docLspSemanticTokensString doc
  return $ unlines . map show $ xs

docLspSemanticTokensString :: TextDocumentIdentifier -> Session [SemanticTokenOriginal Language.LSP.Protocol.Types.SemanticTokenTypes]
docLspSemanticTokensString doc = do
  res <- Test.getSemanticTokens doc
  textContent <- documentContents doc
  let vfs = VirtualFile 0 0 (Rope.fromText textContent)
  case res ^? Language.LSP.Protocol.Types._L of
    Just tokens -> do
      either (error . show) pure $ recoverLspSemanticTokens vfs tokens
    _noTokens -> error "No tokens found"

semanticTokensClassTests :: TestTree
semanticTokensClassTests =
  testGroup
    "type class"
    [ goldenWithSemanticTokensWithDefaultConfig "golden type class" "TClass",
      goldenWithSemanticTokensWithDefaultConfig "imported class method InstanceClassMethodBind" "TInstanceClassMethodBind",
      goldenWithSemanticTokensWithDefaultConfig "imported class method TInstanceClassMethodUse" "TInstanceClassMethodUse",
      goldenWithSemanticTokensWithDefaultConfig "imported deriving" "TClassImportedDeriving"
    ]

semanticTokensValuePatternTests :: TestTree
semanticTokensValuePatternTests =
  testGroup
    "value and patterns "
    [ goldenWithSemanticTokensWithDefaultConfig "value bind" "TValBind",
      goldenWithSemanticTokensWithDefaultConfig "pattern match" "TPatternMatch",
      goldenWithSemanticTokensWithDefaultConfig "pattern bind" "TPatternbind"
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
            let funcVar = object ["functionToken" .= var]
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
        let file1 = "TModula𐐀bA.hs"
        let file2 = "TModuleB.hs"
        let expect =
              [
                SemanticTokenOriginal TModule (Loc 3 8 8) "TModuleA",
                SemanticTokenOriginal TVariable (Loc 5 1 2) "go",
                SemanticTokenOriginal TDataConstructor (Loc 5 6 4) "Game"
              ]
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1, file2]) $ do
          doc1 <- openDoc file1 "haskell"
          doc2 <- openDoc file2 "haskell"
          check1 <- waitForAction "TypeCheck" doc1
          check2 <- waitForAction "TypeCheck" doc2
          case check1 of
            Right (WaitForIdeRuleResult _) -> return ()
            Left _                         -> error "TypeCheck1 failed"
          case check2 of
            Right (WaitForIdeRuleResult _) -> return ()
            Left _                         -> error "TypeCheck2 failed"



          textContent2 <- documentContents doc2
          let vfs = VirtualFile 0 0 (Rope.fromText textContent2)
          res2 <- Test.getSemanticTokens doc2
          result <- docSemanticTokensString def doc2
          let expect = unlines [
                    "3:8-18 TModule \"TModula\\66560bA\""
                    , "4:18-28 TModule \"TModula\\66560bA\""
                    , "6:1-3 TVariable \"go\""
                    , "6:6-10 TDataConstructor \"Game\""
                    , "8:1-5 TVariable \"a\\66560bb\""
                    , "8:8-19 TModule \"TModula\\66560bA.\""
                    , "8:19-22 TRecordField \"a\\66560b\""
                    , "8:23-25 TVariable \"go\""
                ]
          liftIO $ result @?= expect,
      goldenWithSemanticTokensWithDefaultConfig "mixed constancy test result generated from one ghc version" "T1",
      goldenWithSemanticTokensWithDefaultConfig "pattern bind" "TPatternSynonym",
      goldenWithSemanticTokensWithDefaultConfig "type family" "TTypefamily",
      goldenWithSemanticTokensWithDefaultConfig "TUnicodeSyntax" "TUnicodeSyntax",
      goldenWithSemanticTokensWithDefaultConfig "TQualifiedName" "TQualifiedName",
      goldenWithSemanticTokensWithDefaultConfig "TDoc" "TDoc"
    ]

semanticTokensDataTypeTests :: TestTree
semanticTokensDataTypeTests =
  testGroup
    "get semantic Tokens"
    [ goldenWithSemanticTokensWithDefaultConfig "simple datatype" "TDataType",
      goldenWithSemanticTokensWithDefaultConfig "record" "TRecord",
      goldenWithSemanticTokensWithDefaultConfig "record With DuplicateRecordFields" "TRecordDuplicateRecordFields",
      goldenWithSemanticTokensWithDefaultConfig "datatype import" "TDatatypeImported",
      goldenWithSemanticTokensWithDefaultConfig "datatype family" "TDataFamily",
      goldenWithSemanticTokensWithDefaultConfig "GADT" "TGADT"
    ]

semanticTokensFunctionTests :: TestTree
semanticTokensFunctionTests =
  testGroup
    "get semantic of functions"
    [ goldenWithSemanticTokensWithDefaultConfig "functions" "TFunction",
      goldenWithSemanticTokensWithDefaultConfig "local functions" "TFunctionLocal",
      goldenWithSemanticTokensWithDefaultConfig "functions under type synonym" "TFunctionUnderTypeSynonym",
      goldenWithSemanticTokensWithDefaultConfig "function in let binding" "TFunctionLet",
      goldenWithSemanticTokensWithDefaultConfig "negative case non-function with constraint" "TNoneFunctionWithConstraint"
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
