{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

import           Control.Lens                       ((^.), (^?))
import           Data.Aeson                         (KeyValue (..), Object)
import qualified Data.Aeson.KeyMap                  as KV
import           Data.Default
import           Data.Functor                       (void)
import           Data.Map.Strict                    as Map hiding (map)
import           Data.String                        (fromString)
import           Data.Text                          hiding (length, map,
                                                     unlines)
import qualified Data.Text                          as Text
import qualified Data.Text.Utf16.Rope.Mixed         as Rope
import           Development.IDE                    (Pretty)
import           Development.IDE.Plugin.Test        (WaitForIdeRuleResult (..))
import           Ide.Plugin.SemanticTokens
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens         as L
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Test                  as Test
import           Language.LSP.VFS                   (VirtualFile (..))
import           System.FilePath
import           Test.Hls
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.FileSystem                (file, text)

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-semantic-tokens-plugin" </> "test" </> "testdata"

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

goldenWithHaskellAndCapsOutPut :: (Pretty b) => Config -> PluginTestDescriptor b -> TestName -> FS.VirtualFileTree -> FilePath -> String -> (TextDocumentIdentifier -> Session String) -> TestTree
goldenWithHaskellAndCapsOutPut config plugin title tree path desc act =
  goldenGitDiff title (FS.vftOriginalRoot tree </> path <.> desc) $
    fromString
      <$> ( runSessionWithServerInTmpDir config plugin tree $
              do
                doc <- openDoc (path <.> "hs") "haskell"
                void waitForBuildQueue
                act doc
          )

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

docSemanticTokensString :: SemanticTokensConfig -> TextDocumentIdentifier -> Session String
docSemanticTokensString cf doc = do
  xs <- map (lspTokenHsToken cf) <$> docLspSemanticTokensString doc
  return $ unlines . map show $ xs

docLspSemanticTokensString :: (HasCallStack) => TextDocumentIdentifier -> Session [SemanticTokenOriginal Language.LSP.Protocol.Types.SemanticTokenTypes]
docLspSemanticTokensString doc = do
  res <- Test.getSemanticTokens doc
  textContent <- documentContents doc
  let vfs = VirtualFile 0 0 (Rope.fromText textContent)
  case res ^? Language.LSP.Protocol.Types._L of
    Just tokens -> do
      either (error . show) pure $ recoverLspSemanticTokens vfs tokens
    _noTokens -> error "No tokens found"

-- | Pass a param and return the response from `semanticTokensFull`
-- getSemanticTokensFullDelta :: TextDocumentIdentifier -> Session _
getSemanticTokensFullDelta :: TextDocumentIdentifier -> Text -> Session (SemanticTokens |? (SemanticTokensDelta |? Null))
getSemanticTokensFullDelta doc lastResultId = do
  let params = SemanticTokensDeltaParams Nothing Nothing doc lastResultId
  rsp <- request SMethod_TextDocumentSemanticTokensFullDelta params
  case rsp ^. L.result of
    Right x -> return x
    _       -> error "No tokens found"

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

mkSemanticConfig :: Object -> Config
mkSemanticConfig setting = def {plugins = Map.insert "SemanticTokens" conf (plugins def)}
  where
    conf = def {plcConfig = setting}

directFile :: FilePath -> Text -> [FS.FileTree]
directFile fp content =
  [ FS.directCradle [Text.pack fp],
    file fp (text content)
  ]

semanticTokensConfigTest :: TestTree
semanticTokensConfigTest =
  testGroup
    "semantic token config test"
    [ testCase "function to variable" $ do
        let content = Text.unlines ["module Hello where", "go _ = 1"]
        let fs = mkFs $ directFile "Hello.hs" content
        let funcVar = KV.fromList ["functionToken" .= var]
            var :: String
            var = "variable"
        Test.Hls.runSessionWithTestConfig def
          { testPluginDescriptor = semanticTokensPlugin
          , testConfigSession = def
            { ignoreConfigurationRequests = False
            }
          , testConfigCaps = fullLatestClientCaps
          , testDirLocation = Right fs
          , testLspConfig = mkSemanticConfig funcVar
          }
        $ const $ do
            -- modifySemantic funcVar
            void waitForBuildQueue
            doc <- openDoc "Hello.hs" "haskell"
            void waitForBuildQueue
            result1 <- docLspSemanticTokensString doc
            liftIO $ unlines (map show result1) @?= "2:1-3 SemanticTokenTypes_Variable \"go\"\n"
    ]

semanticTokensFullDeltaTests :: TestTree
semanticTokensFullDeltaTests =
  testGroup "semanticTokensFullDeltaTests"
    [ testCase "null delta since unchanged" $ do
        let file1 = "TModuleA.hs"
        let expectDelta = InR (InL (SemanticTokensDelta (Just "1") []))
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1]) $ do
          doc1 <- openDoc file1 "haskell"
          _ <- waitForAction "TypeCheck" doc1
          _ <- Test.getSemanticTokens doc1
          delta <- getSemanticTokensFullDelta doc1 "0"
          liftIO $ delta @?= expectDelta,
      testCase "add tokens" $ do
        let file1 = "TModuleA.hs"
        let expectDelta = InR (InL (SemanticTokensDelta (Just "1") [SemanticTokensEdit 20 0 (Just [2, 0, 3, 8, 0])]))
        --                                                                                         r c l t m
        --                                      where r = row, c = column, l = length, t = token, m = modifier
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1]) $ do
          doc1 <- openDoc file1 "haskell"
          _ <- waitForAction "TypeCheck" doc1
          _ <- Test.getSemanticTokens doc1
          -- open the file and append a line to it
          let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                { _range = Range (Position 4 0) (Position 4 6)
                , _rangeLength = Nothing
                , _text = "foo = 1"
                }
          changeDoc doc1 [change]
          _ <- waitForAction "TypeCheck" doc1
          delta <- getSemanticTokensFullDelta doc1 "0"
          liftIO $ delta @?= expectDelta,
      testCase "remove tokens" $ do
        let file1 = "TModuleA.hs"
        let expectDelta = InR (InL (SemanticTokensDelta (Just "1") [SemanticTokensEdit 0 20 (Just [])]))
        -- delete all tokens
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1]) $ do
          doc1 <- openDoc file1 "haskell"
          _ <- waitForAction "TypeCheck" doc1
          _ <- Test.getSemanticTokens doc1
          -- open the file and append a line to it
          let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
                { _range = Range (Position 2 0) (Position 2 28)
                , _rangeLength = Nothing
                , _text = Text.replicate 28 " "
                }
          changeDoc doc1 [change]
          _ <- waitForAction "TypeCheck" doc1
          delta <- getSemanticTokensFullDelta doc1 "0"
          liftIO $ delta @?= expectDelta
    ]

semanticTokensTests :: TestTree
semanticTokensTests =
  testGroup "other semantic Token test" $
    [ testCase "module import test" $ do
        let file1 = "TModuleA.hs"
        let file2 = "TModuleB.hs"
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

          result <- docSemanticTokensString def doc2
          let expect =
                unlines
                  [ "3:8-16 TModule \"TModuleA\"",
                    "4:18-26 TModule \"TModuleA\"",
                    "6:1-3 TVariable \"go\"",
                    "6:6-10 TDataConstructor \"Game\"",
                    "8:1-5 TVariable \"a\\66560bb\"",
                    "8:8-17 TModule \"TModuleA.\"",
                    "8:17-20 TRecordField \"a\\66560b\"",
                    "8:21-23 TVariable \"go\""
                  ]
          liftIO $ result @?= expect,
      goldenWithSemanticTokensWithDefaultConfig "mixed constancy test result generated from one ghc version" "T1",
      goldenWithSemanticTokensWithDefaultConfig "pattern bind" "TPatternSynonym",
      goldenWithSemanticTokensWithDefaultConfig "type family" "TTypefamily",
      goldenWithSemanticTokensWithDefaultConfig "TUnicodeSyntax" "TUnicodeSyntax",
      goldenWithSemanticTokensWithDefaultConfig "TQualifiedName" "TQualifiedName"
    ]
      -- not supported in ghc92
      ++ [goldenWithSemanticTokensWithDefaultConfig "TDoc" "TDoc" | ghcVersion > GHC92]

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
      goldenWithSemanticTokensWithDefaultConfig "negative case non-function with constraint" "TNoneFunctionWithConstraint",
      goldenWithSemanticTokensWithDefaultConfig "TOperator" "TOperator"
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
        semanticTokensConfigTest,
        semanticTokensFullDeltaTests
      ]
