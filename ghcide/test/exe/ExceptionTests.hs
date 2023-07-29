
module ExceptionTests (tests) where

import           Control.Concurrent.Async
import           Control.Exception                 (ArithException (DivideByZero),
                                                    finally, throwIO)
import           Control.Lens
import           Control.Monad.Error.Class         (MonadError (throwError))
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Aeson                        as A
import           Data.Text                         as T
import           Development.IDE.Core.Shake        (IdeState (..))
import qualified Development.IDE.LSP.Notifications as Notifications
import qualified Development.IDE.Main              as IDE
import           Development.IDE.Plugin.Test       as Test
import           Development.IDE.Types.Options
import           GHC.Base                          (coerce)
import           Ide.Logger                        (Logger, Recorder,
                                                    WithPriority, cmapWithPrio)
import           Ide.Plugin.Error
import           Ide.PluginUtils                   (idePluginsToPluginDesc,
                                                    pluginDescToIdePlugins)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens        as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types       hiding
                                                   (SemanticTokenAbsolute (..),
                                                    SemanticTokenRelative (..),
                                                    SemanticTokensEdit (..),
                                                    mkRange)
import           Language.LSP.Test
import           LogType                           (Log (..))
import           System.Directory
import           System.Process.Extra              (createPipe)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils

tests :: Recorder (WithPriority Log) -> Logger -> TestTree
tests recorder logger = do
  testGroup "Exceptions and PluginError" [
    testGroup "Testing that IO Exceptions are caught in..."
      [ testCase "PluginHandlers" $ do
          let pluginId = "plugin-handler-exception"
              plugins = pluginDescToIdePlugins $
                  [ (defaultPluginDescriptor pluginId)
                      { pluginHandlers = mconcat
                          [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                              _ <- liftIO $ throwIO DivideByZero
                              pure (InL [])
                          ]
                      }]

          testIde recorder (testingLite recorder logger plugins) $ do
              doc <- createDoc "A.hs" "haskell" "module A where"
              waitForProgressDone
              (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
              case lens of
                Left (ResponseError {_code = InR ErrorCodes_InternalError, _message}) ->
                  liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "divide by zero" _message && T.isInfixOf (coerce pluginId) _message)
                _ -> liftIO $ assertFailure $ show lens
        , testCase "Commands" $ do
          let pluginId = "command-exception"
              commandId = CommandId "exception"
              plugins = pluginDescToIdePlugins $
                  [ (defaultPluginDescriptor pluginId)
                      { pluginCommands =
                          [ PluginCommand commandId "Causes an exception" $ \_ (_::Int) -> do
                              _ <- liftIO $ throwIO DivideByZero
                              pure (InR Null)
                          ]
                      }]

          testIde recorder (testingLite recorder logger plugins) $ do
              _ <- createDoc "A.hs" "haskell" "module A where"
              waitForProgressDone
              let cmd = mkLspCommand (coerce pluginId) commandId "" (Just [A.toJSON (1::Int)])
                  execParams = ExecuteCommandParams Nothing (cmd ^. L.command) (cmd ^. L.arguments)
              (view L.result -> res) <- request SMethod_WorkspaceExecuteCommand execParams
              case res of
                Left (ResponseError {_code = InR ErrorCodes_InternalError, _message}) ->
                  liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "divide by zero" _message && T.isInfixOf (coerce pluginId) _message)
                _ -> liftIO $ assertFailure $ show res
        , testCase "Notification Handlers" $ do
          let pluginId = "notification-exception"
              plugins = pluginDescToIdePlugins $
                  [ (defaultPluginDescriptor pluginId)
                      { pluginNotificationHandlers = mconcat
                          [  mkPluginNotificationHandler SMethod_TextDocumentDidOpen $ \_ _ _ _ ->
                              liftIO $ throwIO DivideByZero
                          ]
                        , pluginHandlers = mconcat
                          [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                              pure (InL [])
                          ]
                      }
                    , Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"]

          testIde recorder (testingLite recorder logger plugins) $ do
              doc <- createDoc "A.hs" "haskell" "module A where"
              waitForProgressDone
              (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
              case lens of
                Right (InL []) ->
                  pure ()
                _ -> liftIO $ assertFailure $ "We should have had an empty list" <> show lens]

   , testGroup "Testing PluginError order..."
      [ testCase "InternalError over InvalidParams" $ do
        let pluginId = "internal-error-order"
            plugins = pluginDescToIdePlugins $
                [ (defaultPluginDescriptor pluginId)
                    { pluginHandlers = mconcat
                        [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                            throwError $ PluginInternalError "error test"
                         ,mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                            throwError $ PluginInvalidParams "error test"
                        ]
                    }
                  , Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"]

        testIde recorder (testingLite recorder logger plugins) $ do
            doc <- createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
            case lens of
              Left (ResponseError {_code = InR ErrorCodes_InternalError, _message}) ->
                  liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "error test" _message && T.isInfixOf (coerce pluginId) _message)
              _ -> liftIO $ assertFailure $ show lens
      , testCase "InvalidParams over InvalidUserState" $ do
        let pluginId = "invalid-params-order"
            plugins = pluginDescToIdePlugins $
                [ (defaultPluginDescriptor pluginId)
                    { pluginHandlers = mconcat
                        [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                            throwError $ PluginInvalidParams "error test"
                         ,mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                            throwError $ PluginInvalidUserState "error test"
                        ]
                    }
                  , Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"]

        testIde recorder (testingLite recorder logger plugins) $ do
            doc <- createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
            case lens of
              Left (ResponseError {_code = InR ErrorCodes_InvalidParams, _message}) ->
                  liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "error test" _message && T.isInfixOf (coerce pluginId) _message)
              _ -> liftIO $ assertFailure $ show lens
      , testCase "InvalidUserState over RequestRefused" $ do
        let pluginId = "invalid-user-state-order"
            plugins = pluginDescToIdePlugins $
                [ (defaultPluginDescriptor pluginId)
                    { pluginHandlers = mconcat
                        [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                            throwError $ PluginInvalidUserState "error test"
                         ,mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                            throwError $ PluginRequestRefused "error test"
                        ]
                    }
                  , Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"]

        testIde recorder (testingLite recorder logger plugins) $ do
            doc <- createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
            case lens of
              Left (ResponseError {_code = InL LSPErrorCodes_RequestFailed, _message}) ->
                  liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "error test" _message && T.isInfixOf (coerce pluginId) _message)
              _ -> liftIO $ assertFailure $ show lens
     ]]

testIde :: Recorder (WithPriority Log) -> IDE.Arguments -> Session () -> IO ()
testIde recorder arguments session = do
    config <- getConfigFromEnv
    cwd <- getCurrentDirectory
    (hInRead, hInWrite) <- createPipe
    (hOutRead, hOutWrite) <- createPipe
    let projDir = "."
    let server = IDE.defaultMain (cmapWithPrio LogIDEMain recorder) arguments
            { IDE.argsHandleIn = pure hInRead
            , IDE.argsHandleOut = pure hOutWrite
            }

    flip finally (setCurrentDirectory cwd) $ withAsync server $ \_ ->
        runSessionWithHandles hInWrite hOutRead config lspTestCaps projDir session

testingLite :: Recorder (WithPriority Log) -> Logger -> IdePlugins IdeState -> IDE.Arguments
testingLite recorder logger plugins =
  let
    arguments@IDE.Arguments{ argsIdeOptions } =
        IDE.defaultArguments (cmapWithPrio LogIDEMain recorder) logger plugins
    hlsPlugins = pluginDescToIdePlugins $
      idePluginsToPluginDesc plugins
      ++ [Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"]
      ++ [Test.blockCommandDescriptor "block-command", Test.plugin]
    ideOptions config sessionLoader =
      let
        defOptions = argsIdeOptions config sessionLoader
      in
        defOptions{ optTesting = IdeTesting True }
  in
    arguments
      { IDE.argsHlsPlugins = hlsPlugins
      , IDE.argsIdeOptions = ideOptions
      }
