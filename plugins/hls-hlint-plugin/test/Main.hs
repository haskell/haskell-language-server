{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Lens            ((^.))
import           Data.Aeson              (Value (..), object, toJSON, (.=))
import           Data.List               (find)
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust, isJust)
import qualified Data.Text               as T
import           Ide.Plugin.Config       (Config (..), PluginConfig (..),
                                          hlintOn)
import qualified Ide.Plugin.Config       as Plugin
import qualified Ide.Plugin.Hlint        as HLint
import qualified Language.LSP.Types.Lens as L
import           System.FilePath         ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

hlintPlugin :: PluginDescriptor IdeState
hlintPlugin = HLint.descriptor "hlint"

tests :: TestTree
tests = testGroup "hlint" [
      suggestionsTests
    , configTests
    ]

suggestionsTests :: TestTree
suggestionsTests =
  testGroup "hlint suggestions" [
    testCase "provides 3.8 code actions including apply all" $ runHlintSession "" $ do
        doc <- openDoc "Base.hs" "haskell"
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
        doc <- openDoc "Base.hs" "haskell"

        _ <- waitForDiagnosticsFromSource doc "hlint"

        cars <- getAllCodeActions doc
        etaReduce <- liftIO $ inspectCommand cars ["Eta reduce"]

        executeCommand etaReduce

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo = id\n"

    , testCase "changing document contents updates hlint diagnostics" $ runHlintSession "" $ do
        doc <- openDoc "Base.hs" "haskell"
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

    , knownBrokenForHlintOnGhcLib "hlint doesn't take in account cpp flag as ghc -D argument" $
      testCase "[#554] hlint diagnostics works with CPP via ghc -XCPP argument" $ runHlintSession "cpp" $ do
        doc <- openDoc "CppCond.hs" "haskell"
        testHlintDiagnostics doc

    , knownBrokenForHlintOnGhcLib "hlint doesn't take in account cpp flag as ghc -D argument" $
      testCase "[#554] hlint diagnostics works with CPP via language pragma" $ runHlintSession "" $ do
        doc <- openDoc "CppCond.hs" "haskell"
        testHlintDiagnostics doc

    , testCase "[#554] hlint diagnostics works with CPP via -XCPP argument and flag via #include header" $ runHlintSession "cpp" $ do
        doc <- openDoc "CppHeader.hs" "haskell"
        testHlintDiagnostics doc

    , testCase "[#590] apply-refact works with -XLambdaCase argument" $ runHlintSession "lambdacase" $ do
        testRefactor "LambdaCase.hs" "Redundant bracket"
            expectedLambdaCase

    , testCase "[#1242] apply-refact works with -XTypeApplications argument" $ runHlintSession "typeapps" $ do
        testRefactor "TypeApplication.hs" "Redundant bracket"
            expectedTypeApp

    , testCase "apply hints works with LambdaCase via language pragma" $ runHlintSession "" $ do
        testRefactor "LambdaCase.hs" "Redundant bracket"
            ("{-# LANGUAGE LambdaCase #-}" : expectedLambdaCase)

    , expectFailBecause "apply-refact doesn't work with cpp" $
      testCase "apply hints works with CPP via -XCPP argument" $ runHlintSession "cpp" $ do
        testRefactor "CppCond.hs" "Redundant bracket"
            expectedCPP

    , expectFailBecause "apply-refact doesn't work with cpp" $
      testCase "apply hints works with CPP via language pragma" $ runHlintSession "" $ do
        testRefactor "CppCond.hs" "Redundant bracket"
            ("{-# LANGUAGE CPP #-}" : expectedCPP)

    , testCase "hlint diagnostics ignore hints honouring .hlint.yaml" $ runHlintSession "ignore" $ do
        doc <- openDoc "CamelCase.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , testCase "hlint diagnostics ignore hints honouring ANN annotations" $ runHlintSession "" $ do
        doc <- openDoc "IgnoreAnn.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , knownBrokenForHlintOnRawGhc "[#838] hlint plugin doesn't honour HLINT annotations" $
      testCase "hlint diagnostics ignore hints honouring HLINT annotations" $ runHlintSession "" $ do
        doc <- openDoc "IgnoreAnnHlint.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , testCase "apply-refact preserve regular comments" $ runHlintSession "" $ do
        testRefactor "Comments.hs" "Redundant bracket" expectedComments

    , testCase "[#2290] apply all hints works with a trailing comment" $ runHlintSession "" $ do
        testRefactor "TwoHintsAndComment.hs" "Apply all hints" expectedComments2

    , testCase "applyAll is shown only when there is at least one diagnostic in range" $  runHlintSession "" $ do
        doc <- openDoc "TwoHints.hs" "haskell"
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

    , knownBrokenForHlintOnRawGhc "[#2042] maybe hlint is ignoring pragmas" $
      testCase "hlint should warn about unused extensions" $ runHlintSession "unusedext" $ do
        doc <- openDoc "UnusedExtension.hs" "haskell"
        diags@(unusedExt:_) <- waitForDiagnosticsFromSource doc "hlint"

        liftIO $ do
            length diags @?= 1
            unusedExt ^. L.code @?= Just (InR "refact:Unused LANGUAGE pragma")

    , knownBrokenForHlintOnGhcLib "[#1279] hlint uses a fixed set of extensions" $
      testCase "hlint should not activate extensions like PatternSynonyms" $ runHlintSession "" $ do
        doc <- openDoc "PatternKeyword.hs" "haskell"

        waitForAllProgressDone
        -- hlint will report a parse error if PatternSynonyms is enabled
        expectNoMoreDiagnostics 3 doc "hlint"
    , knownBrokenForHlintOnRawGhc "[#2280] maybe hlint is ignoring pragmas" $
      testCase "hlint should not warn about redundant irrefutable pattern with LANGUAGE Strict" $ runHlintSession "" $ do
        doc <- openDoc "StrictData.hs" "haskell"

        waitForAllProgressDone

        expectNoMoreDiagnostics 3 doc "hlint"
    ]
    where
        testRefactor file caTitle expected = do
            doc <- openDoc file "haskell"
            testHlintDiagnostics doc

            cas <- map fromAction <$> getAllCodeActions doc
            let ca = find (\ca -> caTitle `T.isSuffixOf` (ca ^. L.title)) cas
            liftIO $ isJust ca @? ("There is '" ++ T.unpack caTitle ++"' code action")

            executeCodeAction (fromJust ca)

            contents <- skipManyTill anyMessage $ getDocumentEdit doc
            liftIO $ contents @?= T.unlines expected

        expectedLambdaCase = [ "module LambdaCase where", ""
                             , "f = \\case \"true\" -> True"
                             , "          _ -> False"
                             ]
        expectedCPP =        [ "module CppCond where", ""
                             , "#ifdef FLAG"
                             , "f = 1"
                             , "#else"
                             , "g = 2"
                             , "#endif", ""
                             ]
        expectedComments =   [ "-- comment before header"
                             , "module Comments where", ""
                             , "{-# standalone annotation #-}", ""
                             , "-- standalone comment", ""
                             , "-- | haddock comment"
                             , "f = {- inline comment -}{- inline comment inside refactored code -} 1 -- ending comment", ""
                             , "-- final comment"
                             ]
        expectedComments2 =  [ "module TwoHintsAndComment where"
                             , "biggest = foldr1 max -- the line above will show two hlint hints, \"eta reduce\" and \"use maximum\""
                             ]
        expectedTypeApp =    [ "module TypeApplication where", ""
                             , "a = id @Int 1"
                             ]

configTests :: TestTree
configTests = testGroup "hlint plugin config" [

      testCase "changing hlintOn configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendConfigurationChanged (toJSON config)

        doc <- openDoc "Base.hs" "haskell"
        testHlintDiagnostics doc

        let config' = def { hlintOn = False }
        sendConfigurationChanged (toJSON config')

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "changing hlint plugin configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendConfigurationChanged (toJSON config)

        doc <- openDoc "Base.hs" "haskell"
        testHlintDiagnostics doc

        let config' = pluginGlobalOn config "hlint" False
        sendConfigurationChanged (toJSON config')

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "adding hlint flags to plugin configuration removes hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendConfigurationChanged (toJSON config)

        doc <- openDoc "Base.hs" "haskell"
        testHlintDiagnostics doc

        let config' = hlintConfigWithFlags ["--ignore=Redundant id", "--hint=test-hlint-config.yaml"]
        sendConfigurationChanged (toJSON config')

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "adding hlint flags to plugin configuration adds hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendConfigurationChanged (toJSON config)

        doc <- openDoc "Generalise.hs" "haskell"

        expectNoMoreDiagnostics 3 doc "hlint"

        let config' = hlintConfigWithFlags ["--with-group=generalise"]
        sendConfigurationChanged (toJSON config')

        diags' <- waitForDiagnosticsFromSource doc "hlint"
        d <- liftIO $ inspectDiagnostic diags' ["Use <>"]

        liftIO $ do
            length diags' @?= 1
            d ^. L.range @?= Range (Position 1 10) (Position 1 21)
            d ^. L.severity @?= Just DsInfo
    ]

runHlintSession :: FilePath -> Session a -> IO a
runHlintSession subdir  =
    failIfSessionTimeout . runSessionWithServer hlintPlugin ("test/testdata" </> subdir)

noHlintDiagnostics :: [Diagnostic] -> Assertion
noHlintDiagnostics diags =
    Just "hlint" `notElem` map (^. L.source) diags @? "There are no hlint diagnostics"

testHlintDiagnostics :: TextDocumentIdentifier -> Session ()
testHlintDiagnostics doc = do
    diags <- waitForDiagnosticsFromSource doc "hlint"
    liftIO $ length diags > 0 @? "There are hlint diagnostics"

pluginGlobalOn :: Config -> T.Text -> Bool -> Config
pluginGlobalOn config pid state = config'
  where
      pluginConfig = def { plcGlobalOn = state }
      config' = def { plugins = Map.insert pid pluginConfig (plugins config) }

hlintConfigWithFlags :: [T.Text] -> Config
hlintConfigWithFlags flags =
  def
    { hlintOn = True
    , Plugin.plugins = Map.fromList [("hlint",
        def { Plugin.plcConfig = unObject $ object ["flags" .= flags] }
    )] }
  where
    unObject (Object obj) = obj
    unObject _            = undefined

knownBrokenForHlintOnGhcLib :: String -> TestTree -> TestTree
knownBrokenForHlintOnGhcLib = knownBrokenForGhcVersions [GHC88, GHC86]

knownBrokenForHlintOnRawGhc :: String -> TestTree -> TestTree
knownBrokenForHlintOnRawGhc = knownBrokenForGhcVersions [GHC810, GHC90]
