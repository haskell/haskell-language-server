{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Ide.Plugin.Tilia
  ( descriptor
  , provider
  , LogEvent
  )
where

import           Control.Exception                (IOException, try)
import           Control.Monad                    (unless)
import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Except       (ExceptT (..), runExceptT)
import           Data.Choice                      (pattern Do, pattern Don't)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as T
import           Development.IDE                  hiding (pluginHandlers)
import           Development.IDE.Core.PluginUtils (mkFormattingHandlers)
import           Ide.Plugin.Error                 (PluginError (PluginInternalError, PluginInvalidParams))
import           Ide.Plugin.Properties
import           Ide.PluginUtils                  (makeDiffTextEdit)
import           Ide.Types                        hiding (Config)
import           Language.LSP.Protocol.Types
import           Language.LSP.Server              (ProgressCancellable (Cancellable))
import           System.Exit                      (ExitCode (..))
import           System.FilePath                  (takeDirectory, takeFileName)
import           System.Process.ByteString        (readCreateProcessWithExitCode)
import           System.Process.Run               (cwd, proc)
import           Tilia.Editor                     (editorSession, formatBuffer)
import           Tilia.Format                     (describeFormatError)
import           Tilia.Palette                    (Palette (Plain))
import           Tilia.Run                        (Outcome (..))

-- ---------------------------------------------------------------------

descriptor :: Recorder (WithPriority LogEvent) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId desc)
    { pluginHandlers = mkFormattingHandlers $ provider recorder plId,
      pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
    }
  where
    desc = "Provides formatting of Haskell files via tilia. Built with tilia-" <> VERSION_tilia

properties :: Properties '[ 'PropertyKey "external" 'TBoolean]
properties =
  emptyProperties
    & defineBooleanProperty
      #external
      "Call out to an external \"tilia\" executable, rather than using the bundled library"
      False

-- ---------------------------------------------------------------------

-- | Format a whole module with Tilia, which works out the project, the
-- extensions in force, and the fixities of operators from the file's path.
provider :: Recorder (WithPriority LogEvent) -> PluginId -> FormattingHandler IdeState
provider _ _ _ _ (FormatRange _) _ _ _ =
  throwError $ PluginInvalidParams "Tilia formats whole modules, not ranges"
provider recorder plId ideState token FormatText contents fp _ =
  ExceptT $ pluginWithIndefiniteProgress title token Cancellable $ \_updater -> runExceptT $ do
    useCLI <- liftIO $ runAction "Tilia" ideState $ usePropertyAction #external plId properties
    formatted <-
      if useCLI
        then cliHandler
        else do
          logWith recorder Debug $ LogCompiledInVersion VERSION_tilia
          libraryHandler
    pure $ InL $ maybe [] (makeDiffTextEdit contents) formatted
  where
    fp' = fromNormalizedFilePath fp
    title = T.pack $ "Formatting " <> takeFileName fp'

    libraryHandler :: ExceptT PluginError (HandlerM config) (Maybe Text)
    libraryHandler = do
      outcome <- liftIO . try @IOException $
        editorSession fp' Nothing (Do #useCache) (Do #download) (Don't #checkAst) (Don't #checkIdempotence) (Don't #debugFixity)
          >>= \case
            Left e        -> pure (Failed e)
            Right session -> formatBuffer session fp' contents
      case outcome of
        Left e -> throwError $ PluginInternalError $ "Tilia failed: " <> T.pack (show e)
        Right (Changed _ after) -> pure (Just after)
        Right Unchanged -> pure Nothing
        Right (Declined e) -> do
          logWith recorder Info $ LogDeclined (describeFormatError Plain e)
          pure Nothing
        Right (Failed e) -> throwError $ PluginInternalError $ describeFormatError Plain e

    cliHandler :: ExceptT PluginError (HandlerM config) (Maybe Text)
    cliHandler = do
      let commandArgs = ["for-editor", fp']
          dir = takeDirectory fp'
      logWith recorder Debug $ LogTiliaCommand commandArgs dir
      result <- liftIO . try @IOException $
        readCreateProcessWithExitCode (proc "tilia" commandArgs) {cwd = Just dir} (T.encodeUtf8 contents)
      case result of
        Left e -> throwError $ PluginInternalError $ "Could not run tilia: " <> T.pack (show e)
        Right (exitCode, out, err) -> do
          let errText = T.stripEnd (T.decodeUtf8Lenient err)
          case exitCode of
            ExitSuccess -> do
              unless (T.null errText) $ logWith recorder Info $ StdErr errText
              case T.decodeUtf8' out of
                Left _ -> throwError $ PluginInternalError "Tilia printed output that is not valid UTF-8"
                Right after -> pure (Just after)
            ExitFailure n -> do
              logWith recorder Info $ StdErr errText
              throwError $ PluginInternalError $
                "Tilia failed with exit code " <> T.pack (show n)
                  <> (if T.null errText then "" else "\n" <> errText)

data LogEvent
    = StdErr Text
    | LogDeclined Text
    | LogCompiledInVersion String
    | LogTiliaCommand [String] FilePath
    deriving (Show)

instance Pretty LogEvent where
    pretty = \case
        StdErr t -> "Tilia stderr:" <> line <> indent 2 (pretty t)
        LogDeclined t -> "Tilia declined to format:" <> line <> indent 2 (pretty t)
        LogCompiledInVersion v -> "Using compiled in tilia-" <> pretty v
        LogTiliaCommand commandArgs dir ->
            "Running: `tilia " <> pretty (unwords commandArgs) <> "` in directory " <> pretty dir
