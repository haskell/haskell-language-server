
module ExceptionTests (tests) where

import           Control.Exception                 (ArithException (DivideByZero),
                                                    throwIO)
import           Control.Lens
import           Control.Monad.Error.Class         (MonadError (throwError))
import           Control.Monad.IO.Class            (liftIO)
import qualified Data.Aeson                        as A
import           Data.Text                         as T
import           Development.IDE.Core.Shake        (IdeState (..))
import qualified Development.IDE.LSP.Notifications as Notifications
import qualified Development.IDE.Main              as IDE
import           Development.IDE.Plugin.HLS        (toResponseError)
import           Development.IDE.Plugin.Test       as Test
import           Development.IDE.Types.Options
import           GHC.Base                          (coerce)
import           Ide.Logger                        (Logger, Recorder,
                                                    WithPriority, cmapWithPrio)
import           Ide.Plugin.Error
import           Ide.Plugin.HandleRequestTypes     (RejectionReason (DisabledGlobally))
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
                  [ (defaultPluginDescriptor pluginId "")
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
                  [ (defaultPluginDescriptor pluginId "")
                      { pluginCommands =
                          [ PluginCommand commandId "Causes an exception" $ \_ _ (_::Int) -> do
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
                  [ (defaultPluginDescriptor pluginId "")
                      { pluginNotificationHandlers = mconcat
                          [  mkPluginNotificationHandler SMethod_TextDocumentDidOpen $ \_ _ _ _ ->
                              liftIO $ throwIO DivideByZero
                          ]
                        , pluginHandlers = mconcat
                          [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                              pure (InL [])
                          ]
                      }]
          testIde recorder (testingLite recorder logger plugins) $ do
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
      [ pluginOrderTestCase recorder logger  "InternalError over InvalidParams" (PluginInternalError "error test") (PluginInvalidParams "error test")
      , pluginOrderTestCase recorder logger  "InvalidParams over InvalidUserState" (PluginInvalidParams "error test") (PluginInvalidUserState "error test")
      , pluginOrderTestCase recorder logger  "InvalidUserState over RequestRefused" (PluginInvalidUserState "error test") (PluginRequestRefused DisabledGlobally)
      ]
   ]

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

pluginOrderTestCase :: Recorder (WithPriority Log) -> Logger -> TestName -> PluginError -> PluginError -> TestTree
pluginOrderTestCase recorder logger msg err1 err2 =
  testCase msg $ do
      let pluginId = "error-order-test"
          plugins = pluginDescToIdePlugins $
              [ (defaultPluginDescriptor pluginId "")
                  { pluginHandlers = mconcat
                      [ mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                          throwError err1
                        ,mkPluginHandler SMethod_TextDocumentCodeLens $ \_ _ _-> do
                          throwError err2
                      ]
                  }]
      testIde recorder (testingLite recorder logger plugins) $ do
          doc <- createDoc "A.hs" "haskell" "module A where"
          waitForProgressDone
          (view L.result -> lens) <- request SMethod_TextDocumentCodeLens (CodeLensParams Nothing Nothing doc)
          case lens of
            Left re | toResponseError (pluginId, err1) == re -> pure ()
                    | otherwise -> liftIO $ assertFailure "We caught an error, but it wasn't ours!"
            _ -> liftIO $ assertFailure $ show lens
