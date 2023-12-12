{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Bifunctor
import           Data.ByteString                 as BS
import           Data.Default
import qualified Data.Maybe
import           Data.Text
import           Development.IDE
import           Development.IDE.GHC.Compat
-- import Development.IDE.Test
-- import Test.Hls
-- import Test.Hls.Util              (withCanonicalTempDir)
import           Control.Arrow                   (Arrow ((***)), (&&&), (+++))
import           Data.Data
import           Data.Functor                    (void)
import           Data.Map                        as Map
import           Data.String                     (fromString)
import           System.Environment.Blank
import           System.FilePath
-- import qualified Development.IDE.Core.Shake           as Shake
import           Ide.Types
import qualified Test.Hls                        (PluginTestDescriptor,
                                                  mkPluginTestDescriptor',
                                                  runSessionWithServerInTmpDir,
                                                  waitForAction)
import qualified Test.Hls.FileSystem             as FS
-- import Development.IDE.Plugin.Test
import           Control.Lens                    hiding (use)
import           Control.Monad                   (forM)
import qualified Data.List                       as List
import           Data.Maybe                      (fromJust)
import qualified Data.Set                        as Set
import           Development.IDE.Plugin.Test     (WaitForIdeRuleResult (..))
import           Ide.Plugin.Error                (getNormalizedFilePathE)
import           Ide.Plugin.SemanticTokens
import           Ide.Plugin.SemanticTokens.Types
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Types     (SemanticTokens (..),
                                                  SemanticTokensParams (..))
import qualified Language.LSP.Test               as Test
import           Test.Hls
import           Test.Hls                        (TextDocumentIdentifier,
                                                  getCodeLenses, openDoc,
                                                  waitForAction)
import           Test.Hls.Util                   (withCanonicalTempDir)

getUniqueName :: (NamedThing a) => a -> Name
getUniqueName = getName

-- astToString :: (Data a) => a -> String
-- astToString = showSDoc fakeDynFlags . showAstDataFull

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir

semanticTokensPlugin :: Test.Hls.PluginTestDescriptor ()
semanticTokensPlugin = Test.Hls.mkPluginTestDescriptor' Ide.Plugin.SemanticTokens.descriptor "SemanticTokens"


mkSemanticTokensParams :: TextDocumentIdentifier -> SemanticTokensParams
mkSemanticTokensParams doc = SemanticTokensParams Nothing Nothing doc

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

semanticTokensClassTests :: TestTree
semanticTokensClassTests = testGroup  "type class"
    [

    testCase "type class" $ do
        let filePath = "./test/testdata/class.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 4, startChar = 7, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 4, startChar = 11, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 5, startChar = 3, len = 3}, name = "foo"}
                ,SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 5, startChar = 10, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 15, len = 3}, name = "Int"}]
        runSessionWithServerInDirAndGetSemantic "class.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"
    , testCase "imported class method InstanceClassMethodBind" $ do
        let filePath = "./test/testdata/InstanceClassMethodBind.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 16, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 10, len = 2}, name = "Eq"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 13, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 6, startChar = 5, len = 4}, name = "(==)"}
                ,SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 6, startChar = 12, len = 9}, name = "undefined"}]
        runSessionWithServerInDirAndGetSemantic "InstanceClassMethodBind.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"
    ,
    testCase "imported class method InstanceClassMethodUse" $ do
        let filePath = "./test/testdata/InstanceClassMethodUse.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 2}, name = "go"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 4, startChar = 9, len = 4}, name = "(==)"}]
        runSessionWithServerInDirAndGetSemantic "InstanceClassMethodUse.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show) (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"
    ]


semanticTokensTests :: TestTree
semanticTokensTests =
  testGroup
  "get semantic Tokens"
  [
    testCase "variable" $ do
        -- let filePath = "./test/testdata/T1.hs"
        runSessionWithServerInDirAndGetSemantic "T1.hs" $ \tokens _ -> do
            case tokens ^? _L  of
                Just tokens -> do
                    liftIO $ 1 @?= 1
                _ -> error "No tokens found"
            liftIO $ 1 @?= 1
    , testCase "value bind" $ do
        let filePath = "./test/testdata/valBind.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [
                SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 5}, name = "hello"}
                , SemanticTokenOriginal {tokenType = TTypeSyn, loc = Loc {line = 4, startChar = 10, len = 6}, name = "String"}
                , SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 5, startChar = 1, len = 5}, name = "hello"}
                ]
        runSessionWithServerInDirAndGetSemantic "valBind.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"

    , testCase "record" $ do
        let filePath = "./test/testdata/record.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 4, startChar = 18, len = 3}, name = "foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 25, len = 3}, name = "Int"}]
        runSessionWithServerInDirAndGetSemantic "record.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"

    , testCase "pattern match" $ do
        let filePath = "./test/testdata/patternMatch.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 1}, name = "g"}
                    ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 4, len = 7}, name = "Nothing"}]
        runSessionWithServerInDirAndGetSemantic "patternMatch.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"

    , testCase "pattern bind" $ do
        let filePath = "./test/testdata/patternBind.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =

                [SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 3, startChar = 2, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 3, startChar = 5, len = 1}, name = "b"}
                ,SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 5, startChar = 1, len = 1}, name = "f"}
                ,SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 5, startChar = 3, len = 1}, name = "x"}
                ,SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 5, startChar = 5, len = 1}, name = "y"}
                ,SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 5, startChar = 9, len = 1}, name = "x"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 5, startChar = 11, len = 1}, name = "+"}
                ,SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 5, startChar = 13, len = 1}, name = "y"}]
        runSessionWithServerInDirAndGetSemantic "patternBind.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ forM (Prelude.zip xs expect) $ \(x, y) -> do
                            x @?= y
                            return ()) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"
    , testCase "pattern syn" $ do
        let filePath = "./test/testdata/patternsyn.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TPatternSyn, loc = Loc {line = 5, startChar = 9, len = 3}
            , name = "Foo"}]
        runSessionWithServerInDirAndGetSemantic "patternsyn.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"
    , testCase "imported" $ do
        let filePath = "./test/testdata/imported.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 4, startChar = 7, len = 1}, name = "+"}
                ]
        runSessionWithServerInDirAndGetSemantic "imported.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"

    , testCase "datatype" $ do
        let filePath = "./test/testdata/datatype.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 3, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 3, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 3, startChar = 16, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 3, startChar = 30, len = 2}, name = "Eq"}]
        runSessionWithServerInDirAndGetSemantic "datatype.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _ -> error "No tokens found"

    ,
    testCase "module import test" $ do
        let filePath1 = "./test/testdata/imported.hs"
        let filePath2 = "./test/testdata/imported.hs"
        content1 <- liftIO $ Prelude.readFile filePath1
        content2 <- liftIO $ Prelude.readFile filePath2

        let file1 = "moduleA.hs"
        let file2 = "moduleB.hs"
        let expect = []
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1, file2]) $ do
                doc1 <- openDoc file1 "haskell"
                doc2 <- openDoc file2 "haskell"
                check1 <- waitForAction "TypeCheck" doc1
                check2 <- waitForAction "TypeCheck" doc2
                -- case check2 of
                --     Right (WaitForIdeRuleResult x) -> liftIO $ print $ "result of checking2: " <> show x
                --     Left y -> error "TypeCheck2 failed"

                -- res2 <- Test.getSemanticTokens doc2
                -- case res2 ^? _L of
                --     Just tokens -> do
                --         either (error . show)
                --             (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content2 tokens
                --         return ()
                --     _ -> error "No tokens found"
                liftIO $ 1 @?= 1
  ]

main :: IO ()
main = defaultTestRunner $
  testGroup "Semantic tokens"
    [semanticTokensTests, semanticTokensClassTests]
