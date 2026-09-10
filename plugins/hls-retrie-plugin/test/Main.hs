{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Development.IDE.GHC.ExactPrint    as ExactPrint
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           Ide.Logger
import           Ide.Plugin.Config
import qualified Ide.Plugin.Retrie                 as Retrie
import           System.FilePath
import           Test.Hls

data LogWrap
    = RetrieLog Retrie.Log
    | ExactPrintLog ExactPrint.Log

instance Pretty LogWrap where
    pretty = \case
        RetrieLog msg -> pretty msg
        ExactPrintLog msg -> pretty msg

main :: IO ()
main = defaultTestRunner tests

retriePlugin :: PluginTestDescriptor LogWrap
retriePlugin =  mkPluginTestDescriptor (Retrie.descriptor . cmapWithPrio RetrieLog) "retrie"

refactorPlugin :: PluginTestDescriptor LogWrap
refactorPlugin = mkPluginTestDescriptor (Refactor.iePluginDescriptor  . cmapWithPrio ExactPrintLog) "refactor"

tests :: TestTree
tests =
  testGroup "Retrie"
    [ inlineThisTests
    ]

-- These tests follow the convention that an expression @e@ is being inlined
-- into a function @f@.
inlineThisTests :: TestTree
inlineThisTests =
  testGroup "Inline this"
    [ testGroup "provider"
      [ testProvider "lhs" "Identity" 7 0 ["Unfold f", "Unfold f in current file", "Fold f", "Fold f in current file"]
      , testProvider "identifier" "Identity" 7 6 ["Inline e"]
      , testProvider "imported identifier" "Imported" 4 6 ["Inline e"]
      , testProvider "nested where" "NestedWhere" 3 6 ["Inline e"]
      , testProvider "nested let" "NestedLet" 5 5 ["Inline e"]
      , testProvider "class member" "Class" 6 6 []
      , testProvider "operator" "Operator" 4 8 ["Inline */"]
      ]
    , testGroup "resolve"
      [ testResolve "top level function" "Identity" 7 6
      , testResolve "top level function in another file" "Imported" 4 6
      , testResolve "nested where function" "NestedWhere" 3 6
      , testResolve "nested let function" "NestedLet" 5 5
      , testResolve "operator" "Operator" 4 8
      , testResolve "uses imported fixities" "OpChain" 8 4
      , testResolve "expression has lower precedence" "Parenthesis" 7 8
      , testResolve "partially applied function" "Partial" 7 8
      , testResolve "infix with extra arguments" "InfixExtraArg" 8 8
      ]
    ]

testProvider :: TestName -> FilePath -> UInt -> UInt -> [Text] -> TestTree
testProvider title file line row expected = testCase title $ runWithRetrie $ do
    adoc <- openDoc (file <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    let position = Position line row
    codeActions <- getCodeActions adoc $ Range position position
    liftIO $ map codeActionTitle codeActions @?= map Just expected

testResolve :: TestName -> FilePath -> UInt -> UInt -> TestTree
testResolve title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    let p = Position row col
    codeActions <- getCodeActions adoc $ Range p p
    case codeActions of
        [InR ca] -> resolveAndExecuteCodeAction ca
        cas -> liftIO . assertFailure $ "One code action expected, got " <> show (length cas)

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

goldenWithRetrie :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRetrie title path act =
    goldenWithHaskellAndCaps (def { plugins = M.singleton "retrie" def }) codeActionResolveCaps testPlugins title testDataDir path "expected" "hs" act

runWithRetrie :: Session a -> IO a
runWithRetrie = runSessionWithTestConfig def
    { testDirLocation = Left testDataDir
    , testConfigCaps = codeActionResolveCaps
    , testPluginDescriptor = testPlugins
    } . const

testPlugins :: PluginTestDescriptor LogWrap
testPlugins =
    retriePlugin <>
    refactorPlugin  -- needed for the GetAnnotatedParsedSource rule

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-retrie-plugin" </> "test" </> "testdata"
