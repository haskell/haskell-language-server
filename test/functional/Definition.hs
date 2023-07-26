module Definition (tests) where

import           Control.Lens
import           Data.List                  (isSuffixOf)
import           Language.LSP.Protocol.Lens
import           System.Directory
import           System.FilePath            (splitDirectories)
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "definitions" [symbolTests, moduleTests]

symbolTests :: TestTree
symbolTests = testGroup "gotoDefinition on symbols"
       -- gotoDefinition where the definition is in the same file
  [    testCase "gotoDefinition in this file" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "References.hs" "haskell"
        defs <- getDefinitions doc (Position 7 8)
        let expRange = Range (Position 4 0) (Position 4 3)
        liftIO $ defs @?= InL (Definition (InR [Location (doc ^. uri) expRange]))

        -- gotoDefinition where the definition is in a different file
  ,    testCase "gotoDefinition in other file" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        defs <- getDefinitions doc (Position 4 11)
        let expRange = Range (Position 2 0) (Position 2 1)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL (Definition (InR [Location (filePathToUri fp) expRange]))

        -- gotoDefinition where the definition is in a different file and the
        -- definition in the other file is on a line number that is greater
        -- than the number of lines in the file we are requesting from
  ,    testCase "gotoDefinition in other file past lines in this file" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        defs <- getDefinitions doc (Position 5 13)
        let expRange = Range (Position 8 0) (Position 8 1)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL (Definition (InR [Location (filePathToUri fp) expRange]))

        -- gotoDefinition where the definition is in an external
        -- dependency.
  ,    testCase "gotoDefinition in dependency" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Bar.hs" "haskell"
        defs <- getDefinitions doc (Position 13 12)
        let expRange = Range (Position 513 0) (Position 513 4)
        case defs of
            InL (Definition (InR [Location fp actualRange])) ->
                liftIO $ do
                    let locationDirectories :: [String]
                        locationDirectories =
                            maybe [] splitDirectories $
                                uriToFilePath fp
                    assertBool "empty not found in Data.Set.Internal"
                        $ ["Data", "Set", "Internal.hs"]
                            `isSuffixOf` locationDirectories
                    actualRange @?= expRange
            wrongLocation ->
                liftIO $
                    assertFailure $ "Wrong location for Set.empty: "
                        ++ show wrongLocation
  ]

  -- -----------------------------------

moduleTests :: TestTree
moduleTests = testGroup "gotoDefinition on modules"
  [ ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's imported modules" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        defs <- getDefinitions doc (Position 2 8)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL (Definition (InR [Location (filePathToUri fp) zeroRange]))

  , ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's exported modules" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        defs <- getDefinitions doc (Position 0 15)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL (Definition (InR [Location (filePathToUri fp) zeroRange]))

  , ignoreTestBecause "Broken: file:///Users/jwindsor/src/haskell-language-server/test/testdata/Bar.hs" $
    testCase "goto's imported modules that are loaded" $ runSession hlsCommand fullCaps "test/testdata/definition" $ do
        doc <- openDoc "Foo.hs" "haskell"
        _ <- openDoc "Bar.hs" "haskell"
        defs <- getDefinitions doc (Position 2 8)
        liftIO $ do
            fp <- canonicalizePath "test/testdata/definition/Bar.hs"
            defs @?= InL (Definition (InR [Location (filePathToUri fp) zeroRange]))

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
                defs @?= InL (Definition (InR [Location (filePathToUri fp) zeroRange]))
            liftIO $ putStrLn "E" -- AZ

            noDiagnostics
    ]

zeroRange :: Range
zeroRange = Range (Position 0 0) (Position 0 0)
