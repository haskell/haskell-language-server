{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

import           Control.Lens                       ((^.), (^?))
import           Data.Aeson                         (KeyValue (..), Object)
import qualified Data.Aeson.KeyMap                  as KV
import           Data.Default
import           Data.Functor                       (void)
import qualified Data.List                          as T
import           Data.Map.Strict                    as Map hiding (map)
import           Data.String                        (fromString)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Utf16.Rope.Mixed         as Rope
import           Data.Version                       (Version (..))
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
import           System.Info                        (compilerVersion)
import           Test.Hls
import qualified Test.Hls.FileSystem                as FS
import           Test.Hls.FileSystem                (file, text)

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-semantic-tokens-plugin" </> "test" </> "testdata" </> testVersionDir

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

-- if 9_10 and after we change the directory to the testdata/before_9_10 directory
-- if 9_10 and after we change the directory to the testdata/after_9_10 directory

testVersionDir :: FilePath
testVersionDir
      | compilerVersion >= Version [9, 10] [] = "after_9_10"
      | otherwise = "before_9_10"

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
            liftIO $ unlines (map show result1) @?=
                T.unlines ( [ "1:1-7 SemanticTokenTypes_Keyword \"module\"" ]
                        ++ ["1:8-13 SemanticTokenTypes_Namespace \"Hello\"" | compilerVersion >= Version [9, 10] []]
                        ++ [ "1:14-19 SemanticTokenTypes_Keyword \"where\""
                           , "2:1-3 SemanticTokenTypes_Variable \"go\""
                           , "2:6-7 SemanticTokenTypes_Keyword \"=\""
                           , "2:8-9 SemanticTokenTypes_Number \"1\"" ])
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
        let expectDelta
                | compilerVersion >= Version [9, 10] [] = InR (InL (SemanticTokensDelta (Just "1") [SemanticTokensEdit {_start = 60, _deleteCount = 0, _data_ = Just [2,0,3,8,0,0,4,1,15,0,0,2,1,19,0]}]))
                | otherwise = InR (InL (SemanticTokensDelta {_resultId = Just "1", _edits = [SemanticTokensEdit {_start = 55, _deleteCount = 0, _data_ = Just [2,0,3,8,0,0,4,1,15,0,0,2,1,19,0]}]}))
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
        let expectDelta
                | compilerVersion >= Version [9, 10] [] = InR (InL (SemanticTokensDelta {_resultId = Just "1", _edits = [SemanticTokensEdit {_start = 21, _deleteCount = 12, _data_ = Just []},SemanticTokensEdit {_start = 34, _deleteCount = 3, _data_ = Just []},SemanticTokensEdit {_start = 41, _deleteCount = 0, _data_ = Just [7]},SemanticTokensEdit {_start = 42, _deleteCount = 2, _data_ = Just [15]},SemanticTokensEdit {_start = 46, _deleteCount = 1, _data_ = Just [5]},SemanticTokensEdit {_start = 51, _deleteCount = 6, _data_ = Just [6]}]}))
                | otherwise =  InR (InL (SemanticTokensDelta {_resultId = Just "1", _edits = [SemanticTokensEdit {_start = 16, _deleteCount = 12, _data_ = Just []},SemanticTokensEdit {_start = 29, _deleteCount = 3, _data_ = Just []},SemanticTokensEdit {_start = 36, _deleteCount = 0, _data_ = Just [7]},SemanticTokensEdit {_start = 37, _deleteCount = 2, _data_ = Just [15]},SemanticTokensEdit {_start = 41, _deleteCount = 1, _data_ = Just [5]},SemanticTokensEdit {_start = 46, _deleteCount = 6, _data_ = Just [6]}]}))
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
  testGroup "other semantic Token test"
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
                  (
                     [ "[1:1-7 HsSyntacticTokenType TKeyword \"module\"]" ]
                    -- > 9.10 have module name in the token
                     ++ ["[1:8-16 HsSemanticTokenType TModule \"TModuleB\"]" | compilerVersion >= Version [9, 10] []]
                     ++ [ "[1:17-22 HsSyntacticTokenType TKeyword \"where\"]"
                     , "[3:1-7 HsSyntacticTokenType TKeyword \"import\"]"
                     , "[3:8-16 HsSemanticTokenType TModule \"TModuleA\"]"
                     , "[4:1-7 HsSyntacticTokenType TKeyword \"import\"]"
                     , "[4:8-17 HsSyntacticTokenType TKeyword \"qualified\"]"
                     , "[4:18-26 HsSemanticTokenType TModule \"TModuleA\"]"
                     , "[6:1-3 HsSemanticTokenType TVariable \"go\"]"
                     , "[6:4-5 HsSyntacticTokenType TKeyword \"=\"]"
                     , "[6:6-10 HsSemanticTokenType TDataConstructor \"Game\"]"
                     , "[6:11-12 HsSyntacticTokenType TNumberLit \"1\"]"
                     , "[8:1-5 HsSemanticTokenType TVariable \"a\\66560bb\"]"
                     , "[8:5-6 HsSyntacticTokenType TKeyword \" \"]"
                     , "[8:8-17 HsSemanticTokenType TModule \"TModuleA.\"]"
                     , "[8:17-20 HsSyntacticTokenType TRecordSelector \"a\\66560b\",8:17-20 HsSemanticTokenType TRecordField \"a\\66560b\"]"
                     , "[8:21-23 HsSemanticTokenType TVariable \"go\"]"
                     ]
                    )
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
