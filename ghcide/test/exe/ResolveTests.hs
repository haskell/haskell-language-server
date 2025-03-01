{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
module ResolveTests (tests) where

import           Config
import           Control.Lens
import           Data.Aeson
import qualified Data.Maybe                    as Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           GHC.Generics
import           Ide.Logger
import           Ide.Types                     (PluginDescriptor (..), PluginId,
                                                defaultPluginDescriptor,
                                                mkPluginHandler,
                                                mkResolveHandler)
import qualified Language.LSP.Protocol.Lens    as J
import qualified Language.LSP.Protocol.Lens    as JL
import           Language.LSP.Protocol.Message (SomeMethod (..))
import qualified Language.LSP.Protocol.Message as LSP
import           Language.LSP.Protocol.Types
import           Language.LSP.Test             hiding (resolveCompletion)
import           Test.Hls                      (IdeState, SMethod (..), liftIO,
                                                mkPluginTestDescriptor,
                                                someMethodToMethodString,
                                                waitForAllProgressDone)
import qualified Test.Hls.FileSystem           as FS
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "resolve"
  [ testGroup "with and without data" resolveRequests
  ]

removeData :: JL.HasData_ s (Maybe a) => s -> s
removeData param = param & JL.data_ .~ Nothing

simpleTestSession :: TestName -> Session () -> TestTree
simpleTestSession name act =
    testCase name $ runWithResolvePlugin (mkIdeTestFs [FS.directCradle ["A.hs"]]) (const act)

runWithResolvePlugin :: FS.VirtualFileTree -> (FilePath -> Session a) -> IO a
runWithResolvePlugin fs =
    testSessionWithPlugin fs
        (mkPluginTestDescriptor resolvePluginDescriptor "resolve-plugin")

data CompletionItemResolveData = CompletionItemResolveData
    { completionItemResolve_number :: Int
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CodeActionResolve = CodeActionResolve
    { codeActionResolve_number :: Int
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data CodeLensResolve = CodeLensResolve
    { codeLensResolve_number :: Int
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

resolvePluginDescriptor :: Recorder (WithPriority Text) -> PluginId -> PluginDescriptor IdeState
resolvePluginDescriptor recorder pid = (defaultPluginDescriptor pid "Test Plugin for Resolve Requests")
  { pluginHandlers = mconcat
      [ mkResolveHandler LSP.SMethod_CompletionItemResolve $ \_ _ param _ CompletionItemResolveData{} -> pure param
      , mkPluginHandler LSP.SMethod_TextDocumentCompletion $ \_ _ _ -> do
          pure $ InL
            [ defCompletionItem "test item without data"
            , defCompletionItem "test item with data"
                & J.data_ .~ Just (toJSON $ CompletionItemResolveData 100)
            ]
      , mkResolveHandler LSP.SMethod_CodeActionResolve $ \_ _ param _ CodeActionResolve{} -> pure param
      , mkPluginHandler LSP.SMethod_TextDocumentCodeAction $ \_ _ _ -> do
          logWith recorder Debug "Why is the handler not called?"
          pure $ InL
            [ InR $ defCodeAction "test item without data"
            , InR $ defCodeAction "test item with data"
                & J.data_ .~ Just (toJSON $ CodeActionResolve 70)
            ]
      , mkResolveHandler LSP.SMethod_CodeLensResolve $ \_ _ param _ CodeLensResolve{} -> pure param
      , mkPluginHandler LSP.SMethod_TextDocumentCodeLens $ \_ _ _ -> do
          pure $ InL
            [ defCodeLens "test item without data"
            , defCodeLens "test item with data"
                & J.data_ .~ Just (toJSON $ CodeLensResolve 50)
            ]
      ]
  }

resolveRequests :: [TestTree]
resolveRequests =
  [ simpleTestSession "completion resolve" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "data Foo = Foo { foo :: Int }"
        , "bar = Foo 4"
        ]
      waitForAllProgressDone
      items <- getCompletions doc (Position 2 7)
      let resolveCompItems = filter (\i -> "test item" `T.isPrefixOf` (i ^. J.label)) items
      liftIO $ assertEqual "There must be exactly two results" 2 (length resolveCompItems)
      -- This must not throw an error.
      _ <- traverse (resolveCompletion . removeData) resolveCompItems
      pure ()
  , simpleTestSession "codeAction resolve" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "data Foo = Foo { foo :: Int }"
        , "bar = Foo 4"
        ]
      waitForAllProgressDone
      -- Cant use 'getAllCodeActions', as this lsp-test function queries the diagnostic
      -- locations and we don't have diagnostics in these tests.
      cas <- Maybe.mapMaybe (preview _R) <$> getCodeActions doc (Range (Position 0 0) (Position 1 0))
      let resolveCas = filter (\i -> "test item" `T.isPrefixOf` (i ^. J.title)) cas
      liftIO $ assertEqual "There must be exactly two results" 2 (length resolveCas)
      -- This must not throw an error.
      _ <- traverse (resolveCodeAction . removeData) resolveCas
      pure ()
  , simpleTestSession "codelens resolve" $ do
      doc <- createDoc "A.hs" "haskell" $ T.unlines
        [ "module A where"
        , "data Foo = Foo { foo :: Int }"
        , "bar = Foo 4"
        ]
      waitForAllProgressDone
      cd <- getCodeLenses doc
      let resolveCodeLenses = filter (\i -> case i ^. J.command of
            Just cmd -> "test item" `T.isPrefixOf` (cmd ^. J.title)
            Nothing  -> False
            ) cd
      liftIO $ assertEqual "There must be exactly two results" 2 (length resolveCodeLenses)
      -- This must not throw an error.
      _ <- traverse (resolveCodeLens . removeData) resolveCodeLenses
      pure ()
  ]

defCompletionItem :: T.Text -> CompletionItem
defCompletionItem lbl = CompletionItem
    { _label = lbl
    , _labelDetails = Nothing
    , _kind = Nothing
    , _tags = Nothing
    , _detail = Nothing
    , _documentation = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _sortText = Nothing
    , _filterText = Nothing
    , _insertText = Just "insertion"
    , _insertTextFormat = Nothing
    , _insertTextMode = Nothing
    , _textEdit = Nothing
    , _textEditText = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

defCodeAction :: T.Text -> CodeAction
defCodeAction lbl = CodeAction
    { _title = lbl
    , _kind = Just CodeActionKind_Refactor
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled = Nothing
    , _edit = Nothing
    , _command = Just $ Command
        { _title = lbl
        , _command = lbl
        , _arguments = Nothing
        }
    , _data_ = Nothing
    }

defCodeLens :: T.Text -> CodeLens
defCodeLens lbl = CodeLens
    { _range = mkRange 0 0 1 0
    , _command = Just $ Command
        { _title = lbl
        , _command = lbl
        , _arguments = Nothing
        }
    , _data_ = Nothing
    }

-- TODO: expose this from lsp-test
resolveCompletion :: CompletionItem -> Session CompletionItem
resolveCompletion item = do
  rsp <- request SMethod_CompletionItemResolve item
  case rsp ^. JL.result of
      Left err -> liftIO $ assertFailure (someMethodToMethodString (SomeMethod SMethod_CompletionItemResolve) <> " failed with: " <> show err)
      Right x -> pure x
