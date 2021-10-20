{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Lens            ((^.))
import           Data.Aeson              (toJSON)
import           Data.List               (find)
import           Data.Maybe              (fromJust, isJust)
import qualified Data.Text               as T
import qualified Ide.Plugin.Hlint        as HLint
import           Ide.Plugin.Config       (hlintOn)
import qualified Language.LSP.Types.Lens as L
import           System.FilePath         ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

hlintPlugin :: PluginDescriptor IdeState
hlintPlugin = HLint.descriptor "hlint"

tests :: TestTree
tests =
  testGroup "hlint suggestions" [
    testCase "provides 3.8 code actions including apply all" $ runHlintSession "" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"
        diags@(reduceDiag:_) <- waitForDiagnosticsFromSource doc "hlint"

        liftIO $ do
            length diags @?= 2 -- "Eta Reduce" and "Redundant Id"
            reduceDiag ^. L.range @?= Range (Position 1 0) (Position 1 12)
            reduceDiag ^. L.severity @?= Just DsInfo
            reduceDiag ^. L.code @?= Just (InR "refact:Eta reduce")
            reduceDiag ^. L.source @?= Just "hlint"

        cas <- map fromAction <$> getAllCodeActions doc

        let applyAll = find (\ca -> "Apply all hints" `T.isSuffixOf` (ca ^. L.title)) cas
        let redId = find (\ca -> "Redundant id" `T.isSuffixOf` (ca ^. L.title)) cas
        let redEta = find (\ca -> "Eta reduce" `T.isSuffixOf` (ca ^. L.title)) cas

        liftIO $ isJust applyAll @? "There is 'Apply all hints' code action"
        liftIO $ isJust redId @? "There is 'Redundant id' code action"
        liftIO $ isJust redEta @? "There is 'Eta reduce' code action"

        executeCodeAction (fromJust redId)

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo x = x\n"

    , testCase "falls back to pre 3.8 code actions" $ runSessionWithServer' [hlintPlugin] def def noLiteralCaps "test/testdata" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"

        _ <- waitForDiagnosticsFromSource doc "hlint"

        cars <- getAllCodeActions doc
        etaReduce <- liftIO $ inspectCommand cars ["Eta reduce"]

        executeCommand etaReduce

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo = id\n"

    , testCase "changing configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendConfigurationChanged config

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let config' = def { hlintOn = False }
        sendConfigurationChanged config'

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "changing document contents updates hlint diagnostics" $ runHlintSession "" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let change = TextDocumentContentChangeEvent
                        (Just (Range (Position 1 8) (Position 1 12)))
                         Nothing "x"
        changeDoc doc [change]
        expectNoMoreDiagnostics 3 doc "hlint"

        let change' = TextDocumentContentChangeEvent
                        (Just (Range (Position 1 8) (Position 1 12)))
                         Nothing "id x"
        changeDoc doc [change']
        testHlintDiagnostics doc

    , knownBrokenForGhcVersions [GHC88, GHC86] "hlint doesn't take in account cpp flag as ghc -D argument" $
      testCase "hlint diagnostics works with CPP via ghc -XCPP argument (#554)" $ runHlintSession "cpp" $ do
        doc <- openDoc "ApplyRefact3.hs" "haskell"
        testHlintDiagnostics doc

    , knownBrokenForGhcVersions [GHC88, GHC86] "hlint doesn't take in account cpp flag as ghc -D argument" $
      testCase "hlint diagnostics works with CPP via language pragma (#554)" $ runHlintSession "" $ do
        doc <- openDoc "ApplyRefact3.hs" "haskell"
        testHlintDiagnostics doc

    , testCase "hlint diagnostics works with CPP via -XCPP argument and flag via #include header (#554)" $ runHlintSession "cpp" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

    , testCase "apply-refact works with -XLambdaCase argument (#590)" $ runHlintSession "lambdacase" $ do
        testRefactor "ApplyRefact1.hs" "Redundant bracket"
            expectedLambdaCase

    , testCase "apply-refact works with -XTypeApplications argument (#1242)" $ runHlintSession "typeapps" $ do
        testRefactor "ApplyRefact1.hs" "Redundant bracket"
            expectedTypeApp

    , testCase "apply hints works with LambdaCase via language pragma" $ runHlintSession "" $ do
        testRefactor "ApplyRefact1.hs" "Redundant bracket"
            ("{-# LANGUAGE LambdaCase #-}" : expectedLambdaCase)

    , expectFailBecause "apply-refact doesn't work with cpp" $
      testCase "apply hints works with CPP via -XCPP argument" $ runHlintSession "cpp" $ do
        testRefactor "ApplyRefact3.hs" "Redundant bracket"
            expectedCPP

    , expectFailBecause "apply-refact doesn't work with cpp" $
      testCase "apply hints works with CPP via language pragma" $ runHlintSession "" $ do
        testRefactor "ApplyRefact3.hs" "Redundant bracket"
            ("{-# LANGUAGE CPP #-}" : expectedCPP)

    , testCase "hlint diagnostics ignore hints honouring .hlint.yaml" $ runHlintSession "ignore" $ do
        doc <- openDoc "ApplyRefact.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , testCase "hlint diagnostics ignore hints honouring ANN annotations" $ runHlintSession "" $ do
        doc <- openDoc "ApplyRefact4.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , knownBrokenForGhcVersions [GHC810, GHC90] "hlint plugin doesn't honour HLINT annotations (#838)" $
      testCase "hlint diagnostics ignore hints honouring HLINT annotations" $ runHlintSession "" $ do
        doc <- openDoc "ApplyRefact5.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , testCase "apply-refact preserve regular comments" $ runHlintSession "" $ do
        testRefactor "ApplyRefact6.hs" "Redundant bracket" expectedComments

    , testCase "applyAll is shown only when there is at least one diagnostic in range" $  runHlintSession "" $ do
        doc <- openDoc "ApplyRefact8.hs" "haskell"
        _ <- waitForDiagnosticsFromSource doc "hlint"

        firstLine <- map fromAction <$> getCodeActions doc (mkRange 0 0 0 0)
        secondLine <- map fromAction <$> getCodeActions doc (mkRange 1 0 1 0)
        thirdLine <- map fromAction <$> getCodeActions doc (mkRange 2 0 2 0)
        multiLine <- map fromAction <$> getCodeActions doc (mkRange 0 0 2 0)

        let hasApplyAll = isJust . find (\ca -> "Apply all hints" `T.isSuffixOf` (ca ^. L.title))

        liftIO $ hasApplyAll firstLine @? "Missing apply all code action"
        liftIO $ hasApplyAll secondLine @? "Missing apply all code action"
        liftIO $ not (hasApplyAll thirdLine) @? "Unexpected apply all code action"
        liftIO $ hasApplyAll multiLine @? "Missing apply all code action"
    ]
    where
        runHlintSession :: FilePath -> Session a -> IO a
        runHlintSession subdir  =
            failIfSessionTimeout . runSessionWithServer hlintPlugin ("test/testdata/hlint" </> subdir)

        noHlintDiagnostics :: [Diagnostic] -> Assertion
        noHlintDiagnostics diags =
            Just "hlint" `notElem` map (^. L.source) diags @? "There are no hlint diagnostics"

        testHlintDiagnostics doc = do
            diags <- waitForDiagnosticsFromSource doc "hlint"
            liftIO $ length diags > 0 @? "There are hlint diagnostics"

        testRefactor file caTitle expected = do
            doc <- openDoc file "haskell"
            testHlintDiagnostics doc

            cas <- map fromAction <$> getAllCodeActions doc
            let ca = find (\ca -> caTitle `T.isSuffixOf` (ca ^. L.title)) cas
            liftIO $ isJust ca @? ("There is '" ++ T.unpack caTitle ++"' code action")

            executeCodeAction (fromJust ca)

            contents <- skipManyTill anyMessage $ getDocumentEdit doc
            liftIO $ contents @?= T.unlines expected

        expectedLambdaCase = [ "module ApplyRefact1 where", ""
                             , "f = \\case \"true\" -> True"
                             , "          _ -> False"
                             ]
        expectedCPP =        [ "module ApplyRefact3 where", ""
                             , "#ifdef FLAG"
                             , "f = 1"
                             , "#else"
                             , "g = 2"
                             , "#endif", ""
                             ]
        expectedComments =   [ "-- comment before header"
                             , "module ApplyRefact6 where", ""
                             , "{-# standalone annotation #-}", ""
                             , "-- standalone comment", ""
                             , "-- | haddock comment"
                             , "f = {- inline comment -}{- inline comment inside refactored code -} 1 -- ending comment", ""
                             , "-- final comment"
                             ]
        expectedTypeApp =    [ "module ApplyRefact1 where", ""
                             , "a = id @Int 1"
                             ]
