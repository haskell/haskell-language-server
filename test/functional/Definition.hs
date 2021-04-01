module Definition (tests) where

import           Control.Lens
import           Language.LSP.Types.Lens
import           System.Directory
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "definitions" [

    ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/References.hs" $
       testCase "goto's symbols" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "References.hs" "haskell"
        defs <- getDefinitions doc (Position 7 8)
        let expRange = Range (Position 4 0) (Position 4 3)
        liftIO $ defs @?= InL [Location (doc ^. uri) expRange]

  -- -----------------------------------

  , ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's imported modules" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        defs <- getDefinitions doc (Position 2 8)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL [Location (filePathToUri fp) zeroRange]

  , ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's exported modules" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        defs <- getDefinitions doc (Position 0 15)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL [Location (filePathToUri fp) zeroRange]

  , ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's imported modules that are loaded" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        _ <- openDoc "Bar.hs" "haskell"
        defs <- getDefinitions doc (Position 2 8)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL [Location (filePathToUri fp) zeroRange]

  , ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's imported modules that are loaded, and then closed" $
        runSession hlsCommand fullCaps "test/testdata/definition" $ do
            doc <- openDoc "Foo.hs" "haskell"
            otherDoc <- openDoc "Bar.hs" "haskell"
            closeDoc otherDoc
            defs <- getDefinitions doc (Position 2 8)
            _ <- waitForDiagnostics
            liftIO $ putStrLn "D"
            liftIO $ do
                fp <- canonicalizePath "test/testdata/definition/Bar.hs"
                defs @?= InL [Location (filePathToUri fp) zeroRange]
            liftIO $ putStrLn "E" -- AZ

            noDiagnostics
    ]

zeroRange :: Range
zeroRange = Range (Position 0 0) (Position 0 0)
