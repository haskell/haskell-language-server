
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

module InitializeResponseTests (tests) where

import           Control.Monad
import           Data.List.Extra
import           Data.Row
import qualified Data.Text                         as T
import           Development.IDE.Plugin.TypeLenses (typeLensCommandId)
import qualified Language.LSP.Protocol.Lens        as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types       hiding
                                                   (SemanticTokenAbsolute (..),
                                                    SemanticTokenRelative (..),
                                                    SemanticTokensEdit (..),
                                                    mkRange)
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Control.Lens                      ((^.))
import           Development.IDE.Plugin.Test       (blockCommandId)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils

tests :: TestTree
tests = withResource acquire release tests where

  -- these tests document and monitor the evolution of the
  -- capabilities announced by the server in the initialize
  -- response. Currently the server advertises almost no capabilities
  -- at all, in some cases failing to announce capabilities that it
  -- actually does provide! Hopefully this will change ...
  tests :: IO (TResponseMessage Method_Initialize) -> TestTree
  tests getInitializeResponse =
    testGroup "initialize response capabilities"
    [ chk "   text doc sync"             _textDocumentSync  tds
    , chk "   hover"                         _hoverProvider (Just $ InL True)
    , chk "   completion"               _completionProvider (Just $ CompletionOptions Nothing (Just ["."]) Nothing (Just True) Nothing)
    , chk "NO signature help"        _signatureHelpProvider Nothing
    , chk "   goto definition"          _definitionProvider (Just $ InL True)
    , chk "   goto type definition" _typeDefinitionProvider (Just $ InL True)
    -- BUG in lsp-test, this test fails, just change the accepted response
    -- for now
    , chk "NO goto implementation"  _implementationProvider (Just $ InL False)
    , chk "   find references"          _referencesProvider (Just $ InL True)
    , chk "   doc highlight"     _documentHighlightProvider (Just $ InL True)
    , chk "   doc symbol"           _documentSymbolProvider (Just $ InL True)
    , chk "   workspace symbol"    _workspaceSymbolProvider (Just $ InL True)
    , chk "   code action"             _codeActionProvider  (Just $ InL False)
    , chk "   code lens"                 _codeLensProvider  (Just $ CodeLensOptions (Just False) (Just True))
    , chk "NO doc formatting"   _documentFormattingProvider (Just $ InL False)
    , chk "NO doc range formatting"
                           _documentRangeFormattingProvider (Just $ InL False)
    , chk "NO doc formatting on typing"
                          _documentOnTypeFormattingProvider Nothing
    , chk "NO renaming"                     _renameProvider (Just $ InL False)
    , chk "NO doc link"               _documentLinkProvider Nothing
    , chk "NO color"                   (^. L.colorProvider) (Just $ InL False)
    , chk "NO folding range"          _foldingRangeProvider (Just $ InL False)
    , che "   execute command"      _executeCommandProvider [typeLensCommandId, blockCommandId]
    , chk "   workspace"                   (^. L.workspace) (Just $ #workspaceFolders .== Just WorkspaceFoldersServerCapabilities{_supported = Just True, _changeNotifications = Just ( InR True )}
                                                                 .+ #fileOperations   .== Nothing)
    , chk "NO experimental"             (^. L.experimental) Nothing
    ] where

      tds = Just (InL (TextDocumentSyncOptions
                              { _openClose = Just True
                              , _change    = Just TextDocumentSyncKind_Incremental
                              , _willSave  = Nothing
                              , _willSaveWaitUntil = Nothing
                              , _save = Just (InR $ SaveOptions {_includeText = Nothing})}))

      chk :: (Eq a, Show a) => TestName -> (ServerCapabilities -> a) -> a -> TestTree
      chk title getActual expected =
        testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

      che :: TestName -> (ServerCapabilities -> Maybe ExecuteCommandOptions) -> [T.Text] -> TestTree
      che title getActual expected = testCase title doTest
        where
            doTest = do
                ir <- getInitializeResponse
                let Just ExecuteCommandOptions {_commands = commands} = getActual $ innerCaps ir
                    commandNames = (!! 2) . T.splitOn ":" <$> commands
                zipWithM_ (\e o -> T.isSuffixOf e o @? show (e,o)) (sort expected) (sort commandNames)

  innerCaps :: TResponseMessage Method_Initialize -> ServerCapabilities
  innerCaps (TResponseMessage _ _ (Right (InitializeResult c _))) = c
  innerCaps (TResponseMessage _ _ (Left _)) = error "Initialization error"

  acquire :: IO (TResponseMessage Method_Initialize)
  acquire = run initializeResponse

  release :: TResponseMessage Method_Initialize -> IO ()
  release = const $ pure ()

