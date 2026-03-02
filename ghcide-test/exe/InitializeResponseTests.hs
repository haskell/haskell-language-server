
{-# LANGUAGE DataKinds #-}

module InitializeResponseTests (tests) where

import           Control.Monad
import           Data.List.Extra
import qualified Data.Text                         as T
import           Development.IDE.Plugin.TypeLenses (typeLensCommandId)
import qualified Language.LSP.Protocol.Lens        as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Test

import           Config
import           Control.Lens                      ((^.))
import           Development.IDE.Plugin.Test       (blockCommandId)
import           Test.Hls

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
    , chk "   hover"                         _hoverProvider (Just $ InR (HoverOptions (Just False)))
    , chk "   completion"               _completionProvider (Just $ CompletionOptions (Just False) (Just ["."]) Nothing (Just True) Nothing)
    , chk "NO signature help"        _signatureHelpProvider Nothing
    , chk "   goto definition"          _definitionProvider (Just $ InR (DefinitionOptions (Just False)))
    , chk "   goto type definition" _typeDefinitionProvider (Just $ InR (InL (TypeDefinitionOptions (Just False))))
    , chk "   goto implementation"  _implementationProvider (Just $ InR (InL (ImplementationOptions (Just False))))
    , chk "   find references"          _referencesProvider (Just $ InR (ReferenceOptions (Just False)))
    , chk "   doc highlight"     _documentHighlightProvider (Just $ InR (DocumentHighlightOptions (Just False)))
    , chk "   doc symbol"           _documentSymbolProvider (Just $ InR (DocumentSymbolOptions (Just False) Nothing))
    , chk "   workspace symbol"    _workspaceSymbolProvider (Just $ InR (WorkspaceSymbolOptions (Just False) (Just False)))
    , chk "NO code action"             _codeActionProvider  Nothing
    , chk "   code lens"                 _codeLensProvider  (Just $ CodeLensOptions (Just False) (Just True))
    , chk "NO doc formatting"   _documentFormattingProvider Nothing
    , chk "NO doc range formatting"
                           _documentRangeFormattingProvider Nothing
    , chk "NO doc formatting on typing"
                          _documentOnTypeFormattingProvider Nothing
    , chk "NO renaming"                     _renameProvider Nothing
    , chk "NO doc link"               _documentLinkProvider Nothing
    , chk "NO color"                   (^. L.colorProvider) Nothing
    , chk "NO folding range"          _foldingRangeProvider Nothing
    , che "   execute command"      _executeCommandProvider [typeLensCommandId, blockCommandId]
    , chk "   workspace"                   (^. L.workspace) (Just $ WorkspaceOptions
                                                                      { _workspaceFolders = Just WorkspaceFoldersServerCapabilities
                                                                           { _supported = Just True
                                                                           , _changeNotifications = Just (InR True)
                                                                           }
                                                                      , _fileOperations = Just $ FileOperationOptions
                                                                          { _didCreate = Nothing
                                                                          , _willCreate = Nothing
                                                                          , _didRename = Nothing
                                                                          , _willRename = Nothing
                                                                          , _didDelete = Nothing
                                                                          , _willDelete = Nothing
                                                                          }
                                                                      })
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
      che title getActual expected = testCase title $ do
        ir <- getInitializeResponse
        ExecuteCommandOptions {_commands = commands} <- case getActual $ innerCaps ir of
          Just eco -> pure eco
          Nothing -> assertFailure "Was expecting Just ExecuteCommandOptions, got Nothing"
        let commandNames = (!! 2) . T.splitOn ":" <$> commands
        zipWithM_ (\e o -> T.isSuffixOf e o @? show (e,o)) (sort expected) (sort commandNames)

  innerCaps :: TResponseMessage Method_Initialize -> ServerCapabilities
  innerCaps (TResponseMessage _ _ (Right (InitializeResult c _))) = c
  innerCaps (TResponseMessage _ _ (Left _)) = error "Initialization error"

  acquire :: IO (TResponseMessage Method_Initialize)
  acquire = run initializeResponse

  release :: TResponseMessage Method_Initialize -> IO ()
  release = mempty

