
module ExceptionTests (tests) where

import           Control.Exception                 (ArithException (DivideByZero),
                                                    throwIO)
import           Control.Lens
import           Control.Monad.Error.Class         (MonadError (throwError))
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Aeson                        as A
import           Data.Default                      (Default (..))
import           Data.Text                         as T
import           Development.IDE.Core.Shake        (IdeState (..))
import qualified Development.IDE.LSP.Notifications as Notifications
import           Development.IDE.Plugin.HLS        (toResponseError)
import           GHC.Base                          (coerce)
import           Ide.Logger                        (Recorder, WithPriority,
                                                    cmapWithPrio)
import           Ide.Plugin.Error
import           Ide.Plugin.HandleRequestTypes     (RejectionReason (DisabledGlobally))
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
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
import           Test.Hls                          (TestConfig (testDisableDefaultPlugin, testPluginDescriptor),
                                                    runSessionWithTestConfig,
                                                    testCheckProject,
                                                    waitForProgressDone)
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = do
  testGroup "Exceptions and PluginError" [
    testGroup "Testing that IO Exceptions are caught in..."
      [ testCase "PluginHandlers" $ do
          let pluginId = "plugin-handler-exception"
              plugins :: Recorder (WithPriority Log) -> IdePlugins IdeState
              plugins r = pluginDescToIdePlugins $
                  [ (defaultPluginDescriptor pluginId "")
                      { pluginHandlers = mconcat
                          [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                              _ <- liftIO $ throwIO DivideByZero
                              pure (InL [])
                          ]
                      }] ++ [Notifications.descriptor (cmapWithPrio LogNotifications r) "ghcide-core"]
          runSessionWithTestConfig def {testPluginDescriptor = plugins, testDisableDefaultPlugin=True, testCheckProject=False
          } $ const $ do
              doc <- createDoc "A.hs" "haskell" "module A where"
              (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
              case lens of
                Left (TResponseError {_code = InR ErrorCodes_InternalError, _message}) ->
                    liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "divide by zero" _message && T.isInfixOf (coerce pluginId) _message)
                _ -> liftIO $ assertFailure $ show lens

        , testCase "Commands" $ do
          let pluginId = "command-exception"
              commandId = CommandId "exception"
              plugins :: Recorder (WithPriority Log) -> IdePlugins IdeState
              plugins r = pluginDescToIdePlugins $
                  [ (defaultPluginDescriptor pluginId "")
                      { pluginCommands =
                          [ PluginCommand commandId "Causes an exception" $ \_ _ (_::Int) -> do
                              _ <- liftIO $ throwIO DivideByZero
                              pure (InR Null)
                          ]
                      }] ++ [Notifications.descriptor (cmapWithPrio LogNotifications r) "ghcide-core"]
          runSessionWithTestConfig def {testPluginDescriptor = plugins, testDisableDefaultPlugin=True, testCheckProject=False} $ const $ do
              _ <- createDoc "A.hs" "haskell" "module A where"
              waitForProgressDone
              let cmd = mkLspCommand (coerce pluginId) commandId "" (Just [A.toJSON (1::Int)])
                  execParams = ExecuteCommandParams Nothing (cmd ^. L.command) (cmd ^. L.arguments)
              (view L.result -> res) <- request SMethod_WorkspaceExecuteCommand execParams
              case res of
                Left (TResponseError {_code = InR ErrorCodes_InternalError, _message}) ->
                  liftIO $ assertBool "We caught an error, but it wasn't ours!"
                          (T.isInfixOf "divide by zero" _message && T.isInfixOf (coerce pluginId) _message)
                _ -> liftIO $ assertFailure $ show res

        , testCase "Notification Handlers" $ do
          let pluginId = "notification-exception"
              plugins :: Recorder (WithPriority Log) -> IdePlugins IdeState
              plugins r = pluginDescToIdePlugins $
                  [ (defaultPluginDescriptor pluginId "")
                      { pluginNotificationHandlers = mconcat
                          [  mkPluginNotificationHandler SMethod_TextDocumentDidOpen $ \_ _ _ _ ->
                              liftIO $ throwIO DivideByZero
                          ]
                        , pluginHandlers = mconcat
                          [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                              pure (InL [])
                          ]
                        }] ++ [Notifications.descriptor (cmapWithPrio LogNotifications r) "ghcide-core"]
          runSessionWithTestConfig def {testPluginDescriptor = plugins, testDisableDefaultPlugin=True, testCheckProject=False} $ const $ do
              doc <- createDoc "A.hs" "haskell" "module A where"
              waitForProgressDone
              (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
              case lens of
                Right (InL []) ->
                  -- We don't get error responses from notification handlers, so
                  -- we can only make sure that the server is still responding
                  pure ()
                _ -> liftIO $ assertFailure $ "We should have had an empty list" <> show lens]

   , testGroup "Testing PluginError order..."
      [ pluginOrderTestCase "InternalError over InvalidParams" (PluginInternalError "error test") (PluginInvalidParams "error test")
      , pluginOrderTestCase "InvalidParams over InvalidUserState" (PluginInvalidParams "error test") (PluginInvalidUserState "error test")
      , pluginOrderTestCase "InvalidUserState over RequestRefused" (PluginInvalidUserState "error test") (PluginRequestRefused DisabledGlobally)
      ]
   ]

pluginOrderTestCase :: TestName -> PluginError -> PluginError -> TestTree
pluginOrderTestCase msg err1 err2 =
  testCase msg $ do
      let pluginId = "error-order-test"
          plugins :: Recorder (WithPriority Log) -> IdePlugins IdeState
          plugins r = pluginDescToIdePlugins $
              [ (defaultPluginDescriptor pluginId "")
                  { pluginHandlers = mconcat
                      [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                          throwError err1
                        ,mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                          throwError err2
                      ]
                  }] ++ [Notifications.descriptor (cmapWithPrio LogNotifications r) "ghcide-core"]
      runSessionWithTestConfig def {testPluginDescriptor = plugins, testDisableDefaultPlugin=True, testCheckProject=False} $ const $ do
          doc <- createDoc "A.hs" "haskell" "module A where"
          waitForProgressDone
          (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
          case lens of
            Left re | toResponseError (pluginId, err1) == re -> pure ()
                    | otherwise -> liftIO $ assertFailure "We caught an error, but it wasn't ours!"
            _ -> liftIO $ assertFailure $ show lens
