{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main
  ( main
  ) where

import           Control.Lens               ((^.))
import           Control.Monad              (when)
import           Data.Aeson                 (Value (..), object, (.=))
import           Data.Functor               (void)
import           Data.List                  (find)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromJust, isJust)
import qualified Data.Text                  as T
import           Ide.Plugin.Config          (Config (..))
import qualified Ide.Plugin.Config          as Plugin
import qualified Ide.Plugin.Hlint           as HLint
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath            ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

hlintPlugin :: PluginTestDescriptor HLint.Log
hlintPlugin = mkPluginTestDescriptor HLint.descriptor "hlint"

tests :: TestTree
tests = testGroup "hlint" [
      suggestionsTests
    , configTests
    , ignoreHintTests
    , applyHintTests
    , resolveTests
    ]

getIgnoreHintText :: T.Text -> T.Text
getIgnoreHintText name = "Ignore hint \"" <> name <> "\" in this module"

getApplyHintText :: T.Text -> T.Text
getApplyHintText name = "Apply hint \"" <> name <> "\""

resolveTests :: TestTree
resolveTests = testGroup "hlint resolve tests"
  [
    ignoreHintGoldenResolveTest
      "Resolve version of: Ignore hint in this module inserts -Wno-unrecognised-pragmas and hlint ignore pragma if warn unrecognized pragmas is off"
      "UnrecognizedPragmasOff"
      (Point 3 8)
      "Eta reduce"
  , applyHintGoldenResolveTest
      "Resolve version of: [#2612] Apply hint works when operator fixities go right-to-left"
      "RightToLeftFixities"
      (Point 6 13)
      "Avoid reverse"
  ]


ignoreHintTests :: TestTree
ignoreHintTests = testGroup "hlint ignore hint tests"
  [
    ignoreHintGoldenTest
      "Ignore hint in this module inserts -Wno-unrecognised-pragmas and hlint ignore pragma if warn unrecognized pragmas is off"
      "UnrecognizedPragmasOff"
      (Point 3 8)
      "Eta reduce"
  , ignoreHintGoldenTest
      "Ignore hint in this module inserts only hlint ignore pragma if warn unrecognized pragmas is on"
      "UnrecognizedPragmasOn"
      (Point 3 9)
      "Eta reduce"
  ]

applyHintTests :: TestTree
applyHintTests = testGroup "hlint apply hint tests"
  [
    applyHintGoldenTest
      "[#2612] Apply hint works when operator fixities go right-to-left"
      "RightToLeftFixities"
      (Point 6 13)
      "Avoid reverse"
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
            reduceDiag ^. L.severity @?= Just DiagnosticSeverity_Information
            reduceDiag ^. L.code @?= Just (InR "refact:Eta reduce")
            reduceDiag ^. L.source @?= Just "hlint"

        cas <- map fromAction <$> getAllCodeActions doc

        let redundantIdHintName = "Redundant id"
        let etaReduceHintName = "Eta reduce"
        let applyAll = find (\ca -> "Apply all hints" `T.isSuffixOf` (ca ^. L.title)) cas
        let redId = find (\ca -> redundantIdHintName `T.isInfixOf` (ca ^. L.title)) cas
        let redEta = find (\ca -> etaReduceHintName `T.isInfixOf` (ca ^. L.title)) cas
        let ignoreRedundantIdInThisModule = find (\ca -> getIgnoreHintText redundantIdHintName == (ca ^.L.title)) cas
        let ignoreEtaReduceThisModule = find (\ca -> getIgnoreHintText etaReduceHintName == (ca ^.L.title)) cas

        liftIO $ isJust applyAll @? "There is Apply all hints code action"
        liftIO $ isJust redId @? "There is Redundant id code action"
        liftIO $ isJust redEta @? "There is Eta reduce code action"
        liftIO $ isJust ignoreRedundantIdInThisModule @? "There is ignore Redundant id code action"
        liftIO $ isJust ignoreEtaReduceThisModule @? "There is ignore Eta reduce code action"

        executeCodeAction (fromJust redId)

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo x = x\n"

    , testCase "falls back to pre 3.8 code actions" $
        runSessionWithTestConfig def {
            testConfigCaps = noLiteralCaps,
            testDirLocation = Left testDir,
            testPluginDescriptor = hlintPlugin,
            testShiftRoot = True} $ const $ do
        doc <- openDoc "Base.hs" "haskell"

        _ <- waitForDiagnosticsFromSource doc "hlint"

        cars <- getAllCodeActions doc
        etaReduce <- liftIO $ inspectCommand cars ["Eta reduce"]

        executeCommand etaReduce

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo = id\n"

    , testCase ".hlint.yaml fixity rules are applied" $ runHlintSession "fixity" $ do
        doc <- openDoc "FixityUse.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "hlint"

    , testCase "changing document contents updates hlint diagnostics" $ runHlintSession "" $ do
        doc <- openDoc "Base.hs" "haskell"
        testHlintDiagnostics doc

        let change = TextDocumentContentChangeEvent $ InL
              TextDocumentContentChangePartial
                { _range = Range (Position 1 8) (Position 1 12)
                , _rangeLength = Nothing
                , _text = "x"
                }

        changeDoc doc [change]
        expectNoMoreDiagnostics 3 doc "hlint"

        let change' = TextDocumentContentChangeEvent $ InL
              TextDocumentContentChangePartial
                { _range = Range (Position 1 8) (Position 1 12)
                , _rangeLength = Nothing
                , _text = "id x"
                }
        changeDoc doc [change']
        testHlintDiagnostics doc

    , testCase "[#554] hlint diagnostics works with CPP via ghc -XCPP argument" $ runHlintSession "cpp" $ do
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

    , testCase "hlint diagnostics ignore hints honouring HLINT annotations" $ runHlintSession "" $ do
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

    , testCase "hlint should warn about unused extensions" $ runHlintSession "unusedext" $ do
        doc <- openDoc "UnusedExtension.hs" "haskell"
        diags@(unusedExt:_) <- waitForDiagnosticsFromSource doc "hlint"

        liftIO $ do
            length diags @?= 1
            unusedExt ^. L.code @?= Just (InR "refact:Unused LANGUAGE pragma")

    , testCase "[#1279] hlint should not activate extensions like PatternSynonyms" $ runHlintSession "" $ do
        doc <- openDoc "PatternKeyword.hs" "haskell"

        waitForAllProgressDone
        -- hlint will report a parse error if PatternSynonyms is enabled
        expectNoMoreDiagnostics 3 doc "hlint"
    , testCase "hlint should not warn about redundant irrefutable pattern with LANGUAGE Strict" $ runHlintSession "" $ do
        doc <- openDoc "StrictData.hs" "haskell"

        waitForAllProgressDone

        expectNoMoreDiagnostics 3 doc "hlint"
    ]
    where
        testRefactor file caTitle expected = do
            doc <- openDoc file "haskell"
            testHlintDiagnostics doc

            cas <- map fromAction <$> getAllCodeActions doc
            let ca = find (\ca -> caTitle `T.isInfixOf` (ca ^. L.title)) cas
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
        expectedComments =  [ "-- comment before header"
                             , "module Comments where", ""
                             , "{-# standalone annotation #-}", ""
                             , "-- standalone comment", ""
                             , "-- | haddock comment"
                             , "f = {- inline comment -} {- inline comment inside refactored code -}1 -- ending comment", ""
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

    testCase "changing hlint plugin configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        setIgnoringConfigurationRequests False
        enableHlint

        doc <- openDoc "Base.hs" "haskell"
        testHlintDiagnostics doc

        disableHlint

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "adding hlint flags to plugin configuration removes hlint diagnostics" $ runHlintSession "" $ do
        setIgnoringConfigurationRequests False
        enableHlint

        doc <- openDoc "Base.hs" "haskell"
        testHlintDiagnostics doc

        let config' = hlintConfigWithFlags ["--ignore=Redundant id", "--hint=test-hlint-config.yaml"]
        setHlsConfig config'

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "adding hlint flags to plugin configuration adds hlint diagnostics" $ runHlintSession "" $ do
        setIgnoringConfigurationRequests False
        enableHlint

        doc <- openDoc "Generalise.hs" "haskell"

        expectNoMoreDiagnostics 3 doc "hlint"

        let config' = hlintConfigWithFlags ["--with-group=generalise"]
        setHlsConfig config'

        diags' <- waitForDiagnosticsFromSource doc "hlint"
        d <- liftIO $ inspectDiagnostic diags' ["Use <>"]

        liftIO $ do
            length diags' @?= 1
            d ^. L.range @?= Range (Position 1 10) (Position 1 21)
            d ^. L.severity @?= Just DiagnosticSeverity_Information
    ]

testDir :: FilePath
testDir = "plugins/hls-hlint-plugin/test/testdata"

runHlintSession :: FilePath -> Session a -> IO a
runHlintSession subdir = failIfSessionTimeout .
    runSessionWithTestConfig def
    {testConfigCaps=codeActionNoResolveCaps
    , testShiftRoot=True
    , testDirLocation=Left (testDir </> subdir)
    , testPluginDescriptor=hlintPlugin
    }
    . const

noHlintDiagnostics :: [Diagnostic] -> Assertion
noHlintDiagnostics diags =
    Just "hlint" `notElem` map (^. L.source) diags @? "There are no hlint diagnostics"

testHlintDiagnostics :: TextDocumentIdentifier -> Session ()
testHlintDiagnostics doc = do
    diags <- waitForDiagnosticsFromSource doc "hlint"
    liftIO $ length diags > 0 @? "There are hlint diagnostics"

hlintConfigWithFlags :: [T.Text] -> Config
hlintConfigWithFlags flags =
  def
    { Plugin.plugins = Map.fromList [("hlint",
        def { Plugin.plcGlobalOn = True, Plugin.plcConfig = unObject $ object ["flags" .= flags] }
    )] }
  where
    unObject (Object obj) = obj
    unObject _            = undefined

enableHlint :: Session ()
enableHlint = setHlsConfig $ def { Plugin.plugins = Map.fromList [ ("hlint", def { Plugin.plcGlobalOn = True }) ] }

disableHlint :: Session ()
disableHlint = setHlsConfig $ def { Plugin.plugins = Map.fromList [ ("hlint", def { Plugin.plcGlobalOn = False }) ] }

-- We have two main code paths in the plugin depending on how hlint interacts with ghc:
-- * One when hlint uses ghc-lib (all ghc versions but the last version supported by hlint)
-- * Another one when hlint uses directly ghc (only one version, which not have to be the last version supported by ghcide)
-- As we always are using ghc through ghcide the code to get the ghc parsed AST differs
-- So the issues and bugs usually only affects to one code path or the other.
-- Although a given hlint version supports one direct ghc, we could use several versions of hlint
-- each one supporting a different ghc version. It should be a temporary situation though.
knownBrokenForHlintOnGhcLib :: String -> TestTree -> TestTree
knownBrokenForHlintOnGhcLib = expectFailBecause

-- 1's based
data Point = Point {
  line   :: !Int,
  column :: !Int
}

pointToRange :: Point -> Range
pointToRange Point {..}
  | line <- fromIntegral $ subtract 1 line
  , column <- fromIntegral $ subtract 1 column =
      Range (Position line column) (Position line $ column + 1)

getCodeActionTitle :: (Command |? CodeAction) -> Maybe T.Text
getCodeActionTitle commandOrCodeAction
  | InR CodeAction {_title} <- commandOrCodeAction = Just _title
  | otherwise = Nothing

makeCodeActionNotFoundAtString :: Point -> String
makeCodeActionNotFoundAtString Point {..} =
  "CodeAction not found at line: " <> show line <> ", column: " <> show column

ignoreHintGoldenTest :: TestName -> FilePath -> Point -> T.Text -> TestTree
ignoreHintGoldenTest testCaseName goldenFilename point hintName =
  goldenTest testCaseName goldenFilename point (getIgnoreHintText hintName)

applyHintGoldenTest :: TestName -> FilePath -> Point -> T.Text -> TestTree
applyHintGoldenTest testCaseName goldenFilename point hintName = do
  goldenTest testCaseName goldenFilename point (getApplyHintText hintName)

goldenTest :: TestName -> FilePath -> Point -> T.Text -> TestTree
goldenTest testCaseName goldenFilename point hintText =
  setupGoldenHlintTest testCaseName goldenFilename $ \document -> do
    _ <- waitForDiagnosticsFromSource document "hlint"
    actions <- getCodeActions document $ pointToRange point
    case find ((== Just hintText) . getCodeActionTitle) actions of
      Just (InR codeAction) -> do
        executeCodeAction codeAction
        when (isJust (codeAction ^. L.command)) $
          void $ skipManyTill anyMessage $ getDocumentEdit document
      _ -> liftIO $ assertFailure $ makeCodeActionNotFoundAtString point


setupGoldenHlintTest :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
setupGoldenHlintTest testName path =
    goldenWithTestConfig def
    {testConfigCaps=codeActionNoResolveCaps
    , testShiftRoot=True
    , testPluginDescriptor=hlintPlugin
    , testDirLocation=Left testDir
    }
        testName testDir path "expected" "hs"


ignoreHintGoldenResolveTest :: TestName -> FilePath -> Point -> T.Text -> TestTree
ignoreHintGoldenResolveTest testCaseName goldenFilename point hintName =
  goldenResolveTest testCaseName goldenFilename point (getIgnoreHintText hintName)

applyHintGoldenResolveTest :: TestName -> FilePath -> Point -> T.Text -> TestTree
applyHintGoldenResolveTest testCaseName goldenFilename point hintName = do
  goldenResolveTest testCaseName goldenFilename point (getApplyHintText hintName)

goldenResolveTest :: TestName -> FilePath -> Point -> T.Text -> TestTree
goldenResolveTest testCaseName goldenFilename point hintText =
  setupGoldenHlintResolveTest testCaseName goldenFilename $ \document -> do
    _ <- waitForDiagnosticsFromSource document "hlint"
    actions <- getAndResolveCodeActions document $ pointToRange point
    case find ((== Just hintText) . getCodeActionTitle) actions of
      Just (InR codeAction) -> executeCodeAction codeAction
      _ -> liftIO $ assertFailure $ makeCodeActionNotFoundAtString point

setupGoldenHlintResolveTest :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
setupGoldenHlintResolveTest testName path =
    goldenWithTestConfig def
    {testConfigCaps=codeActionResolveCaps
    , testShiftRoot=True
    , testPluginDescriptor=hlintPlugin
    , testDirLocation=Left testDir
    }
        testName testDir path "expected" "hs"
