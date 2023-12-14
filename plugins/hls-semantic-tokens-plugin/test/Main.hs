{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}



import           Control.Arrow                      (Arrow ((***)), (&&&),
                                                     (+++))
import           Control.Lens                       hiding (use)
import           Control.Monad                      (forM)
import           Data.Bifunctor
import qualified Data.ByteString                    as BS
import           Data.Data
import           Data.Default
import           Data.Functor                       (void)
import qualified Data.List                          as List
import           Data.Map                           as Map
import           Data.Maybe                         (fromJust)
import qualified Data.Maybe
import qualified Data.Set                           as Set
import           Data.String                        (fromString)
import           Data.Text
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

semanticTokensPlugin :: Test.Hls.PluginTestDescriptor ()
semanticTokensPlugin = Test.Hls.mkPluginTestDescriptor' Ide.Plugin.SemanticTokens.descriptor "SemanticTokens"


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
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProject file) $ do
            let path = "." </> testDataDir </> file
            content <- liftIO $ readFile path
            doc <- openDoc file "haskell"
            res <- waitForAction "TypeCheck" doc
            res <- Test.getSemanticTokens doc
            x res doc content

semanticTokensImportedTests :: TestTree
semanticTokensImportedTests = testGroup "imported test"
    [
        testCase "type class" $ do
            let expect =
                      [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 6, len = 3}, name = "Foo"}
                      , SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 12, len = 3}, name = "Foo"}
                      , SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 4, startChar = 26, len = 4}, name = "Show"}
                      , SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 6, startChar = 1, len = 2}, name = "eq"}
                      , SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 6, startChar = 8, len = 2}, name = "Eq"}
                      , SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 6, startChar = 11, len = 1}, name = "a"}
                      , SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 6, startChar = 17, len = 1}, name = "a"}
                      , SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 6, startChar = 22, len = 4}, name = "Bool"}
                      , SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 7, startChar = 1, len = 2}, name = "eq"}
                      , SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 7, startChar = 4, len = 1}, name = "b"}
                      , SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 7, startChar = 8, len = 1}, name = "b"}
                      , SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 7, startChar = 10, len = 2}, name = "=="}
                      , SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 7, startChar = 13, len = 1}, name = "b"}]


            runSessionWithServerInDirAndGetSemanticsFile "TClass.hs" $ \res doc content -> do
                -- content <- waitForAction "getFileContents" doc
                case res ^? _L of
                    Just tokens -> do
                        either (error . show)
                            (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                        return ()
                    _notokens -> error "No tokens found"



    ]

semanticTokensClassTests :: TestTree
semanticTokensClassTests = testGroup  "type class"
    [ testCase "type class" $ do
        let filePath = "./test/testdata/TClass.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 4, startChar = 7, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 4, startChar = 11, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 5, startChar = 3, len = 3}, name = "foo"}
                ,SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 5, startChar = 10, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 15, len = 3}, name = "Int"}]
        runSessionWithServerInDirAndGetSemantic "TClass.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notoken -> error "No tokens found"
    , testCase "imported class method InstanceClassMethodBind" $ do
        let filePath = "./test/testdata/TInstanceClassMethodBind.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 16, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 5, startChar = 10, len = 2}, name = "Eq"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 13, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 6, startChar = 5, len = 4}, name = "(==)"}
                ,SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 6, startChar = 12, len = 9}, name = "undefined"}]
        runSessionWithServerInDirAndGetSemantic "TInstanceClassMethodBind.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"
    ,
    testCase "imported class method TInstanceClassMethodUse" $ do
        let filePath = "./test/testdata/TInstanceClassMethodUse.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 2}, name = "go"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 4, startChar = 9, len = 4}, name = "(==)"}]
        runSessionWithServerInDirAndGetSemantic "TInstanceClassMethodUse.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show) (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"

    , testCase "imported deriving" $ do
        let filePath = "./test/testdata/TClassImportedDeriving.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 4, startChar = 26, len = 4}, name = "Show"}]
        runSessionWithServerInDirAndGetSemantic "TClassImportedDeriving.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"
    ]

semanticTokensValuePatternTests :: TestTree
semanticTokensValuePatternTests =
  testGroup
  "value and patterns "
  [
    testCase "value bind" $ do
        let filePath = "./test/testdata/TValBind.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 5}, name = "hello"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 10, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 5, startChar = 1, len = 5}, name = "hello"}
                ,SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 5, startChar = 9, len = 6}, name = "length"}]
        runSessionWithServerInDirAndGetSemantic "TValBind.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _noTokens -> error "No tokens found"
    , testCase "pattern match" $ do
        let filePath = "./test/testdata/TPatternMatch.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 4, startChar = 1, len = 1}, name = "g"}
                    ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 4, len = 7}, name = "Nothing"}]
        runSessionWithServerInDirAndGetSemantic "TPatternMatch.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _noTokens -> error "No tokens found"
    , testCase "pattern bind" $ do
        let filePath = "./test/testdata/TPatternBind.hs"
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
        runSessionWithServerInDirAndGetSemantic "TPatternBind.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ forM (Prelude.zip xs expect) $ \(x, y) -> do
                            x @?= y
                            return ()) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"
  ]

semanticTokensTests :: TestTree
semanticTokensTests =
  testGroup
  "other semantic Token test"
  [
    testCase "mixed constancy test" $ do
        let filePath = "./test/testdata/T1.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 6, startChar = 18, len = 3}, name = "Set"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 6, startChar = 23, len = 6}, name = "insert"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 9, startChar = 6, len = 3}, name = "Foo"},SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 9, startChar = 12, len = 3}, name = "Foo"},SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 9, startChar = 18, len = 3}, name = "foo"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 9, startChar = 25, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 11, startChar = 7, len = 3}, name = "Boo"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 11, startChar = 11, len = 1}, name = "a"},SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 12, startChar = 3, len = 3}, name = "boo"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 12, startChar = 10, len = 1}, name = "a"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 12, startChar = 15, len = 1}, name = "a"},SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 14, startChar = 10, len = 3}, name = "Boo"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 14, startChar = 14, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 15, startChar = 5, len = 3}, name = "boo"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 15, startChar = 9, len = 1}, name = "x"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 15, startChar = 13, len = 1}, name = "x"},SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 15, startChar = 15, len = 1}, name = "+"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 17, startChar = 6, len = 2}, name = "Dd"},SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 17, startChar = 11, len = 2}, name = "Dd"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 17, startChar = 14, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TPatternSyn, loc = Loc {line = 19, startChar = 9, len = 3}, name = "One"},SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 19, startChar = 15, len = 3}, name = "Foo"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 21, startChar = 1, len = 3}, name = "ggg"},SemanticTokenOriginal {tokenType = TPatternSyn, loc = Loc {line = 21, startChar = 7, len = 3}, name = "One"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 23, startChar = 6, len = 3}, name = "Doo"},SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 23, startChar = 12, len = 3}, name = "Doo"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 23, startChar = 16, len = 11}, name = "Prelude.Int"},SemanticTokenOriginal {tokenType = TTypeSyn, loc = Loc {line = 24, startChar = 6, len = 4}, name = "Bar1"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 24, startChar = 13, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TTypeSyn, loc = Loc {line = 25, startChar = 6, len = 4}, name = "Bar2"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 25, startChar = 13, len = 3}, name = "Doo"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 27, startChar = 1, len = 2}, name = "bb"},SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 27, startChar = 8, len = 3}, name = "Boo"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 27, startChar = 12, len = 1}, name = "a"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 27, startChar = 18, len = 1}, name = "a"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 27, startChar = 23, len = 1}, name = "a"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 28, startChar = 1, len = 2}, name = "bb"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 28, startChar = 4, len = 1}, name = "x"},SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 28, startChar = 9, len = 3}, name = "boo"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 28, startChar = 13, len = 1}, name = "x"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 29, startChar = 1, len = 2}, name = "aa"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 29, startChar = 7, len = 4}, name = "cool"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 29, startChar = 15, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 29, startChar = 22, len = 4}, name = "cool"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 30, startChar = 1, len = 2}, name = "aa"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 30, startChar = 4, len = 1}, name = "x"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 30, startChar = 9, len = 1}, name = "c"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 30, startChar = 14, len = 2}, name = "aa"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 30, startChar = 17, len = 1}, name = "x"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 30, startChar = 19, len = 1}, name = "c"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 31, startChar = 12, len = 2}, name = "xx"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 31, startChar = 16, len = 2}, name = "yy"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 32, startChar = 11, len = 2}, name = "dd"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 34, startChar = 2, len = 2}, name = "zz"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 34, startChar = 6, len = 2}, name = "kk"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 35, startChar = 1, len = 2}, name = "cc"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 35, startChar = 7, len = 3}, name = "Foo"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 35, startChar = 15, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 35, startChar = 20, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 35, startChar = 28, len = 3}, name = "Int"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 36, startChar = 1, len = 2}, name = "cc"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 36, startChar = 4, len = 1}, name = "f"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 36, startChar = 7, len = 2}, name = "gg"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 36, startChar = 11, len = 2}, name = "vv"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 37, startChar = 10, len = 2}, name = "gg"},SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 38, startChar = 14, len = 3}, name = "foo"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 38, startChar = 18, len = 1}, name = "$"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 38, startChar = 20, len = 1}, name = "f"},SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 38, startChar = 24, len = 3}, name = "foo"},SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 39, startChar = 14, len = 3}, name = "foo"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 39, startChar = 18, len = 1}, name = "$"},SemanticTokenOriginal {tokenType = TPatternBind, loc = Loc {line = 39, startChar = 20, len = 1}, name = "f"},SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 39, startChar = 24, len = 3}, name = "foo"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 41, startChar = 1, len = 2}, name = "go"},SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 41, startChar = 6, len = 3}, name = "foo"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 42, startChar = 1, len = 3}, name = "add"},SemanticTokenOriginal {tokenType = TClassMethod, loc = Loc {line = 42, startChar = 7, len = 11}, name = "(Prelude.+)"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 47, startChar = 1, len = 4}, name = "main"},SemanticTokenOriginal {tokenType = TNothing, loc = Loc {line = 47, startChar = 9, len = 2}, name = "IO"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 48, startChar = 1, len = 4}, name = "main"},SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 48, startChar = 8, len = 8}, name = "putStrLn"}]
        runSessionWithServerInDirAndGetSemantic "T1.hs" $ \res doc -> do
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"
    , testCase "pattern syn" $ do
        let filePath = "./test/testdata/TPatternSyn.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect = [SemanticTokenOriginal {tokenType = TPatternSyn, loc = Loc {line = 5, startChar = 9, len = 3}
            , name = "Foo"}]
        runSessionWithServerInDirAndGetSemantic "TPatternSyn.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"

    , testCase "type family" $ do
        let filePath = "./test/testdata/TTypefamily.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeFamily, loc = Loc {line = 4, startChar = 13, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 4, startChar = 17, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TTypeFamily, loc = Loc {line = 5, startChar = 3, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 7, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 5, startChar = 13, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TTypeFamily, loc = Loc {line = 6, startChar = 3, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeVariable, loc = Loc {line = 6, startChar = 7, len = 1}, name = "a"}
                ,SemanticTokenOriginal {tokenType = TTypeSyn, loc = Loc {line = 6, startChar = 11, len = 6}, name = "String"}]
        runSessionWithServerInDirAndGetSemantic "TTypefamily.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"

    ,testCase "module import test" $ do
        let filePath1 = "./test/testdata/TModuleA.hs"
        let filePath2 = "./test/testdata/TModuleB.hs"
        content1 <- liftIO $ Prelude.readFile filePath1
        content2 <- liftIO $ Prelude.readFile filePath2

        let file1 = "TModuleA.hs"
        let file2 = "TModuleB.hs"
        let expect = [SemanticTokenOriginal {tokenType = TValBind, loc = Loc {line = 5, startChar = 1, len = 2}, name = "go"}
                     ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 5, startChar = 6, len = 4}, name = "Game"}]
        Test.Hls.runSessionWithServerInTmpDir def semanticTokensPlugin (mkFs $ FS.directProjectMulti [file1, file2]) $ do
                doc1 <- openDoc file1 "haskell"
                doc2 <- openDoc file2 "haskell"
                check1 <- waitForAction "TypeCheck" doc1
                check2 <- waitForAction "TypeCheck" doc2
                case check2 of
                    Right (WaitForIdeRuleResult x) -> liftIO $ print $ "result of checking2: " <> show x
                    Left y -> error "TypeCheck2 failed"

                res2 <- Test.getSemanticTokens doc2
                case res2 ^? _L of
                    Just tokens -> do
                        either (error . show)
                            (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content2 tokens
                        return ()
                    _ -> error "No tokens found"
                liftIO $ 1 @?= 1
  ]


semanticTokensDataTypeTests =
    testGroup
    "get semantic Tokens"
    [ testCase "datatype" $ do
        let filePath = "./test/testdata/TDataType.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 3, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 3, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 3, startChar = 16, len = 3}, name = "Int"}
                ,SemanticTokenOriginal {tokenType = TClass, loc = Loc {line = 3, startChar = 30, len = 2}, name = "Eq"}]
        runSessionWithServerInDirAndGetSemantic "TDataType.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"

    -- record is part of datatype
    , testCase "record" $ do
        let filePath = "./test/testdata/TRecord.hs"
        content <- liftIO $ Prelude.readFile filePath
        let expect =
                [SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 6, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TDataCon, loc = Loc {line = 4, startChar = 12, len = 3}, name = "Foo"}
                ,SemanticTokenOriginal {tokenType = TRecField, loc = Loc {line = 4, startChar = 18, len = 3}, name = "foo"}
                ,SemanticTokenOriginal {tokenType = TTypeCon, loc = Loc {line = 4, startChar = 25, len = 3}, name = "Int"}]
        runSessionWithServerInDirAndGetSemantic "TRecord.hs" $ \res doc -> do
            -- content <- waitForAction "getFileContents" doc
            case res ^? _L of
                Just tokens -> do
                    either (error . show)
                        (\ xs -> liftIO $ xs @?= expect) $ recoverSemanticTokens content tokens
                    return ()
                _notokens -> error "No tokens found"
    ]



main :: IO ()
main = defaultTestRunner $
  testGroup "Semantic tokens"
    [semanticTokensClassTests, semanticTokensDataTypeTests, semanticTokensValuePatternTests, semanticTokensTests]
