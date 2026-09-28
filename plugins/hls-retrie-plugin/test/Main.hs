{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
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
    , unfoldTests
    , foldTests
    ]

-- These tests follow the convention that an expression @e@ is being inlined
-- into a function @f@.
inlineThisTests :: TestTree
inlineThisTests =
  testGroup "Inline this"
    [ testGroup "provider"
      [ testProvider "lhs" "Identity" 7 0 ["Unfold f", "Fold f"]
      , testProvider "identifier" "Identity" 7 6 ["Inline e"]
      , testProvider "imported identifier" "Imported" 4 6 ["Inline e"]
      , testProvider "nested where" "NestedWhere" 3 6 ["Inline e"]
      , testProvider "nested let" "NestedLet" 5 5 ["Inline e"]
      , testProvider "class member" "Class" 6 6 []
      , testProvider "operator" "Operator" 4 8 ["Inline */"]
      , testProvider "rule" "RuleMaybeNothing" 2 11 ["Apply rule maybe/nothing forward", "Apply rule maybe/nothing backwards"]
      , testProvider "type synonym" "TypeSyn" 2 5 ["Unfold UserId", "Fold UserId"]
      ]
    , testGroup "resolve"
      [ testInline "top level function" "Identity" 7 6
      , testInline "top level function in another file" "Imported" 4 6
      , testInline "nested where function" "NestedWhere" 3 6
      , testInline "nested let function" "NestedLet" 5 5
      , testInline "nested function shadowing a top-level one" "ShadowLocal" 6 26
      , testInline "guard let function" "GuardLet" 5 12
      , testInline "operator" "Operator" 4 8
      , testInline "uses imported fixities" "OpChain" 8 4
      , testInline "definition uses imported fixities" "OpChainDef" 8 4
      , testInline "expression has lower precedence" "Parenthesis" 7 8
      , testInline "partially applied function" "Partial" 7 8
      , testInline "infix with extra arguments" "InfixExtraArg" 8 8
      , testInline "list use" "ListLiteral" 6 5
      , testInline "right-associative expression needs parens" "RightAssocOp" 8 4
      , testInline "where to let" "WhereToLet" 7 6
      , testInline "where to let arg substitution" "WhereToLetSubst" 6 4
      , testInline "qualified" "Qualified" 5 6
      , testInline "single clause pattern" "Pattern" 5 4
      , testInline "rename let binding" "Capture" 7 6
      ]
    , expectFail $ testGroup "dispatch"
      [ testInline "dispatch multi clause" "MultiClause" 7 4
      , testInline "transfer guards to case" "Guards" 9 4
      , testInline "multi parameter dispatch" "MultiClauseTuple" 7 6
      , testInline "multi clause partial" "MultiClausePartial" 8 8
      ]
    , testGroup "imports"
      [ testInline "appends an import" "CrossFileUse" 6 4
      , testInline "append import at layout column" "IndentImportUse" 6 6
      , testInline "import unqualified to match template" "QualifiedScopeUse" 6 4
      , testInline "import qualified" "QualifiedBodyUse" 5 4
      , testInline "reject if required import isn't exported" "CrossModuleNotExportedUse" 4 4
      ]
    ]

unfoldTests :: TestTree
unfoldTests =
  testGroup "Unfold"
    [ testUnfold "top level function" "Identity" 4 0
    , testUnfold "cpp branches" "CPP" 5 0

      -- The action is run in the defining module, but the changes all happen
      -- in the target module.
    , testCase "rewrites the defining module too" $ runWithRetrie $ do
        defDoc <- openDoc "CrossFileDef.hs" "haskell"
        useDoc <- openDoc "CrossFileUse.hs" "haskell"
        _ <- waitForTypecheck useDoc
        let p = Position 5 0
        actions <- getCodeActions defDoc $ Range p p
        ca <- liftIO $ case [ca | InR ca@CodeAction{_title = "Unfold e"} <- actions] of
            [ca] -> pure ca
            cas  -> assertFailure $ "One Unfold e action expected, got " <> show (length cas)
        resolveAndExecuteCodeAction ca
        useContents <- documentContents useDoc
        defContents <- documentContents defDoc
        useExpected <- liftIO $ T.readFile (testDataDir </> "CrossFileUse.expected.hs")
        defExpected <- liftIO $ T.readFile (testDataDir </> "CrossFileDef.hs")
        liftIO $ useContents @?= useExpected
        liftIO $ defContents @?= defExpected
    ]

foldTests :: TestTree
foldTests =
  testGroup "Fold"
    [ testFold "top level function" "FoldExpression" 2 0
    ]

testProvider :: TestName -> FilePath -> UInt -> UInt -> [Text] -> TestTree
testProvider title file line row expected = testCase title $ runWithRetrie $ do
    adoc <- openDoc (file <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    let position = Position line row
    codeActions <- getCodeActions adoc $ Range position position
    liftIO $ map codeActionTitle codeActions @?= map Just expected

testFold :: TestName -> FilePath -> UInt -> UInt -> TestTree
testFold = testResolve $ titleStartsWith "Fold"

testUnfold :: TestName -> FilePath -> UInt -> UInt -> TestTree
testUnfold = testResolve $ titleStartsWith "Unfold"

testInline :: TestName -> FilePath -> UInt -> UInt -> TestTree
testInline = testResolve $ titleStartsWith "Inline"

testResolve :: (Command |? CodeAction -> Bool) -> TestName -> FilePath -> UInt -> UInt -> TestTree
testResolve keep title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    let p = Position row col
    codeActions <- getCodeActions adoc $ Range p p
    case filter keep codeActions of
        [InR ca] -> resolveAndExecuteCodeAction ca
        cas -> liftIO . assertFailure $
          "One code action expected, got " <> show (map codeActionTitle cas)

titleStartsWith :: Text -> Command |? CodeAction -> Bool
titleStartsWith prefix = maybe False (prefix `T.isPrefixOf`) . codeActionTitle

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
