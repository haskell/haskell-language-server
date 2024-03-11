{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import           Control.Monad                     (void)
import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Development.IDE.GHC.ExactPrint
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           Ide.Plugin.Config
import qualified Ide.Plugin.Retrie                 as Retrie
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

retriePlugin :: PluginTestDescriptor a
retriePlugin = mkPluginTestDescriptor' Retrie.descriptor "retrie"

refactorPlugin :: PluginTestDescriptor Development.IDE.GHC.ExactPrint.Log
refactorPlugin = mkPluginTestDescriptor Refactor.iePluginDescriptor "refactor"

tests :: TestTree
tests = testGroup "Retrie"
    [ inlineThisTests
    ]

inlineThisTests :: TestTree
inlineThisTests = testGroup "Inline this"
    [
        testGroup "provider" [
            testProvider "lhs" "Identity" 4 1 ["Unfold function", "Unfold function in current file", "Fold function", "Fold function in current file"],
            testProvider "identifier" "Identity" 4 16 ["Inline identity"],
            testProvider "imported identifier" "Imported" 4 12 ["Inline identity"],
            testProvider "nested where" "NestedWhere" 4 16 ["Inline identity"],
            testProvider "nested let" "NestedLet" 6 12 ["Inline identity"],
            testProvider "class member" "Class" 5 16 [],
            testProvider "operator" "Operator" 4 16 ["Inline */"]
        ],
        testGroup "command" [
            testCommand "top level function" "Identity" 4 16,
            testCommand "top level function in another file" "Imported" 4 12,
            testCommand "nested where function" "NestedWhere" 4 16,
            testCommand "nested let function" "NestedLet" 6 12,
            testCommand "operator" "Operator" 4 16
        ]
    ]

testProvider :: TestName -> FilePath -> UInt -> UInt -> [Text] -> TestTree
testProvider title file line row expected = testCase title $ runWithRetrie $ do
    adoc <- openDoc (file <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    let position = Position line row
    codeActions <- getCodeActions adoc $ Range position position
    liftIO $ map codeActionTitle codeActions @?= map Just expected

testCommand :: TestName -> FilePath -> UInt -> UInt -> TestTree
testCommand title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    let p = Position row col
    codeActions <- getCodeActions adoc $ Range p p
    case codeActions of
        [InR ca] -> do
            executeCodeAction ca
            void $ skipManyTill anyMessage $ getDocumentEdit adoc
        cas -> liftIO . assertFailure $ "One code action expected, got " <> show (length cas)

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

goldenWithRetrie :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRetrie title path act =
    goldenWithHaskellDoc (def { plugins = M.singleton "retrie" def }) testPlugins title testDataDir path "expected" "hs" act

runWithRetrie :: Session a -> IO a
runWithRetrie = runSessionWithServer def testPlugins testDataDir

testPlugins :: PluginTestDescriptor Development.IDE.GHC.ExactPrint.Log
testPlugins =
    retriePlugin <>
    refactorPlugin  -- needed for the GetAnnotatedParsedSource rule

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-retrie-plugin" </> "test" </> "testdata"
