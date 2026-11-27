{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLists          #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import qualified Data.List.NonEmpty  as NE
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Ide.Plugin.Retrie
import           System.FilePath
import           Test.Hls
import           Test.Hls.FileSystem

main :: IO ()
main = defaultTestRunner tests

plugin :: PluginTestDescriptor Log
plugin = mkPluginTestDescriptor descriptor "retrie"

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
      [ testProvider "lhs" ["Identity"] 7 0 ["Unfold f", "Fold f"]
      , testProvider "identifier" ["Identity"] 7 6 ["Inline e"]
      , testProvider "imported identifier" ["Imported", "Identity"] 4 6 ["Inline e"]
      , testProvider "nested where" ["NestedWhere"] 3 6 ["Inline e"]
      , testProvider "nested let" ["NestedLet"] 5 5 ["Inline e"]
      , testProvider "class member" ["Class"] 12 6 []
      , testProvider "pattern-bound argument" ["Identity"] 7 8 []
      , testProvider "record field" ["Field"] 8 9 []
      , testProvider "record field control" ["Field"] 8 6 ["Inline g"]
      , testProvider "where binder" ["NestedWhere"] 5 4 ["Inline e"]
      , testProvider "let binder" ["NestedLet"] 4 6 ["Inline e"]
      , testProvider "operator" ["Operator"] 4 8 ["Inline */"]
      , testProvider "rule" ["RuleMaybeNothing"] 2 11 ["Apply rule maybe/nothing forward", "Apply rule maybe/nothing backwards"]
      , testProvider "type synonym" ["TypeSyn"] 2 5 ["Unfold UserId", "Fold UserId"]
      , testProvider "multi clause lhs" ["MultiClause"] 3 0 []
      , testProvider "guarded lhs" ["Guards"] 4 0 []
      ]
    , testGroup "resolve"
      [ testInline "top level function" ["Identity"] 7 6
      , testInline "top level function in another file" ["Imported", "Identity"] 4 6
      , testInline "nested where function" ["NestedWhere"] 3 6
      , testInline "nested where function, one of two uses" ["NestedWhereTwice"] 3 12
      , testInline "inner of two nested uses" ["NestedUse"] 3 9
      , testInline "inner of two nested uses, top level" ["NestedUseTop"] 7 9
      , testInline "outer of two nested uses" ["NestedUseOuter"] 3 6
      , testInline "where binder, every use" ["WhereBinder"] 5 4
      , testInline "where binder, nested use" ["WhereBinderNested"] 5 4
      , testInline "let binder" ["NestedLet"] 4 6
      , testInline "nested let function" ["NestedLet"] 5 5
      , testInline "nested function shadowing a top-level one" ["ShadowLocal"] 6 26
      , testInline "guard let function" ["GuardLet"] 5 12
      , testInline "operator" ["Operator"] 4 8
      , testInline "uses imported fixities" ["OpChain"] 8 4
      , testInline "definition uses imported fixities" ["OpChainDef"] 8 4
      , testInline "expression has lower precedence" ["Parenthesis"] 7 8
      , testInline "partially applied function" ["Partial"] 7 8
      , testInline "infix with extra arguments" ["InfixExtraArg"] 8 8
      , testInline "list use" ["ListLiteral"] 6 5
      , testInline "right-associative expression needs parens" ["RightAssocOp"] 8 4
      , testInline "where to let" ["WhereToLet"] 7 6
      , testInline "where to let arg substitution" ["WhereToLetSubst"] 6 4
      , expectFail $ testInline "qualified" ["Qualified"] 5 6
      , testInline "single clause pattern" ["Pattern"] 5 4
      , testInline "rename let binding" ["Capture"] 7 6
      , testInline "comment in an argument" ["CommentArg"] 7 6
      , testInline "comment in an argument with cpp" ["CommentArgCpp"] 9 6
      ]
    , testGroup "refused"
      [ testRefused "multi clause" ["MultiClause"] 7 4 notSelectable
      , testRefused "guards" ["Guards"] 9 4 notSelectable
      , testRefused "multi parameter" ["MultiClauseTuple"] 7 6 notSelectable
      , testRefused "multi clause partial" ["MultiClausePartial"] 8 8 notSelectable
      , testRefused "record wildcard construction" ["WildcardCon"] 10 4 notSelectable
      , testRefused "recursive where binder" ["WhereBinderRec"] 5 4 "definition is recursive"
      , testRefused "rewrite spanning a cpp directive" ["CppStraddle"] 9 6
          "must not span a CPP directive"
      ]
    , expectFail $ testGroup "dispatch"
      [ testInline "dispatch multi clause" ["MultiClause"] 7 4
      , testInline "transfer guards to case" ["Guards"] 9 4
      , testInline "multi parameter dispatch" ["MultiClauseTuple"] 7 6
      , testInline "multi clause partial" ["MultiClausePartial"] 8 8
      ]
    , testGroup "imports"
      [ testInline "appends an import" ["CrossFileUse", "CrossFileDef"] 6 4
      , testInline "appends an import once when other uses remain" ["CrossFileUses", "CrossFileDef"] 6 4
      , testInline "import unqualified to match template" ["QualifiedScopeUse", "CrossFileDef"] 6 4
      , testInline "import qualified" ["QualifiedBodyUse", "QualifiedBodyDef"] 5 4
      , testRefused "different field selector" ["FieldClash", "FieldClashDef"] 9 4 $ clash "name"
      , testRefused "local variable clashes with field selector" ["VarClash", "FieldClashDef"] 10 4 $ clash "name"
      , testRefused "clashing field selectors" ["FieldFieldClash", "FieldClashDef"] 9 4 $ clash "name"
      , testRefused "clashing record label" ["ConPatNoDisamb", "ConPatDef"] 9 4 $ clash "name"
      , testRefused "reject if required import isn't exported" ["CrossModuleNotExportedUse", "CrossModuleNotExported"] 4 4
          "k is not exported from CrossModuleNotExported"
      , testInline "import a method exported without its class" ["ChildExportUse", "ChildExportDef"] 5 4
      , testInline "import a method imported without its class" ["LoneMethodUse", "LoneMethodDef", "LoneMethod"] 5 4
      , testInline "import a constructor via its type" ["ConstructorUse", "ConstructorDef"] 5 4
      ]
    ]

unfoldTests :: TestTree
unfoldTests =
  testGroup "Unfold"
    [ testUnfold "top level function" ["Identity"] 4 0
    , testUnfold "active cpp branch only" ["CPP"] 5 0
    , testUnfold "comment in the definition" ["UnfoldComment"] 4 0

      -- The action is run in the defining module, but the changes all happen
      -- in the target module.
    , testCase "rewrites the defining module too" $
        runWithRetrie ["CrossFileDef", "CrossFileUse"] $ do
          defDoc <- openDoc "CrossFileDef.hs" "haskell"
          useDoc <- openDoc "CrossFileUse.hs" "haskell"
          _ <- waitForTypecheck useDoc
          resolveAndExecuteCodeAction =<< unfoldE defDoc
          useContents <- documentContents useDoc
          defContents <- documentContents defDoc
          useExpected <- liftIO $ T.readFile (testDataDir </> "CrossFileUse.expected.hs")
          defExpected <- liftIO $ T.readFile (testDataDir </> "CrossFileDef.hs")
          liftIO $ useContents @?= useExpected
          liftIO $ defContents @?= defExpected
    ]

-- | The "Unfold e" action on the definition in CrossFileDef.hs.
unfoldE :: TextDocumentIdentifier -> Session CodeAction
unfoldE defDoc = do
  let p = Position 5 0
  actions <- getCodeActions defDoc $ Range p p
  liftIO $ case [ca | InR ca@CodeAction{_title = "Unfold e"} <- actions] of
    [ca] -> pure ca
    cas  -> assertFailure $ "One Unfold e action expected, got " <> show (length cas)

foldTests :: TestTree
foldTests =
  testGroup "Fold"
    [ testFold "top level function" ["FoldExpression"] 2 0
    ]

testProvider :: TestName -> NE.NonEmpty FilePath -> UInt -> UInt -> [Text] -> TestTree
testProvider title files line row expected =
  testCase title $ runWithRetrie files $ do
    adoc <- openDoc (NE.head files <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    let position = Position line row
    codeActions <- getCodeActions adoc $ Range position position
    liftIO $ map codeActionTitle codeActions @?= map Just expected

testFold :: TestName -> NE.NonEmpty FilePath -> UInt -> UInt -> TestTree
testFold = testResolve $ titleStartsWith "Fold"

testUnfold :: TestName -> NE.NonEmpty FilePath -> UInt -> UInt -> TestTree
testUnfold = testResolve $ titleStartsWith "Unfold"

testInline :: TestName -> NE.NonEmpty FilePath -> UInt -> UInt -> TestTree
testInline = testResolve $ titleStartsWith "Inline"

-- | The Inline action at the position resolves to an error carrying the
-- refusal message.
testRefused :: TestName -> NE.NonEmpty FilePath -> UInt -> UInt -> Text -> TestTree
testRefused title files row col message =
  testCase title $ runWithRetrie files $ do
    adoc <- openDoc ((NE.head files) <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    ca <- codeActionAt (titleStartsWith "Inline") adoc (Position row col)
    TResponseMessage{_result} <- request SMethod_CodeActionResolve ca
    liftIO $ case _result of
      Left TResponseError{_message} ->
        assertBool ("Unexpected refusal: " <> T.unpack _message) $
          message `T.isInfixOf` _message
      Right _ -> assertFailure "Expected the inline to be refused"

-- | The refusal for a definition whose clause cannot be chosen statically.
notSelectable :: Text
notSelectable = "definition has several clauses, guards or a record wildcard"

-- | The refusal for an import whose spelling is taken in the target.
clash :: Text -> Text
clash rdr = "spelling " <> rdr <> " is already bound to a different name in the target"

testResolve
  :: (Command |? CodeAction -> Bool) -- ^ filter code actions
  -> TestName
  -> NE.NonEmpty FilePath -- ^ files involved in this test
  -> UInt
  -> UInt
  -> TestTree
testResolve keep title files row col =
  goldenWithRetrie title files $ \ adoc -> do
    _ <- waitForTypecheck adoc
    resolveAndExecuteCodeAction =<< codeActionAt keep adoc (Position row col)

-- | The one code action at the position that @keep@ accepts.
codeActionAt :: (Command |? CodeAction -> Bool) -> TextDocumentIdentifier -> Position -> Session CodeAction
codeActionAt keep adoc p = do
  codeActions <- getCodeActions adoc $ Range p p
  case filter keep codeActions of
    [InR ca] -> pure ca
    cas -> liftIO . assertFailure $
      "One code action expected, got " <> show (map codeActionTitle cas)

titleStartsWith :: Text -> Command |? CodeAction -> Bool
titleStartsWith prefix = maybe False (prefix `T.isPrefixOf`) . codeActionTitle

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

goldenWithRetrie
  :: TestName
  -> NE.NonEmpty FilePath -- ^ the main file, followed by any other files
  -> (TextDocumentIdentifier -> Session ()) -- ^ test assertions
  -> TestTree
goldenWithRetrie title paths act =
  goldenWithHaskellAndCapsInTmpDir def codeActionResolveCaps plugin title (copyHsFiles paths) (NE.head paths) "expected" "hs" act

runWithRetrie :: NE.NonEmpty FilePath -> Session a -> IO a
runWithRetrie paths session = runSessionWithTestConfig config (const session)
  where
    config = def
      { testDirLocation = Right $ copyHsFiles paths
      , testConfigCaps = codeActionResolveCaps
      , testPluginDescriptor = plugin
      }

-- | Create a 'VirtualFileTree' by copying in all of the provided
-- filenames minus the ".hs" extensions.
copyHsFiles :: NE.NonEmpty FilePath -> VirtualFileTree
copyHsFiles paths =
  mkVirtualFileTree testDataDir $ NE.toList $
    fmap (\ f -> copy (f <.> "hs")) paths

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-retrie-plugin" </> "test" </> "testdata"
