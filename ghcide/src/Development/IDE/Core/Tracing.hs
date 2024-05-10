
module Development.IDE.Core.Tracing
    ( otTracedHandler
    , otTracedAction
    , otTracedProvider
    , otSetUri
    , otTracedGarbageCollection
    , withTrace
    , withEventTrace
    , withTelemetryRecorder
    )
where

import           Control.Exception.Safe            (generalBracket)
import           Control.Monad.Catch               (ExitCase (..), MonadMask)
import           Control.Monad.IO.Unlift
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (pack)
import           Data.String                       (IsString (fromString))
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8)
import           Data.Word                         (Word16)
import           Debug.Trace.Flags                 (userTracingEnabled)
import           Development.IDE.Graph             (Action)
import           Development.IDE.Graph.Rule
import           Development.IDE.Types.Diagnostics (FileDiagnostic,
                                                    showDiagnostics)
import           Development.IDE.Types.Location    (Uri (..))
import           Ide.Logger
import           Ide.Types                         (PluginId (..))
import           Language.LSP.Protocol.Types       (NormalizedFilePath,
                                                    fromNormalizedFilePath)
import           OpenTelemetry.Eventlog            (SpanInFlight (..), addEvent,
                                                    beginSpan, endSpan, setTag,
                                                    withSpan)


withTrace :: (MonadMask m, MonadIO m) => String -> ((String -> String -> m ()) -> m a) -> m a
withTrace name act
  | userTracingEnabled
  = withSpan (fromString name) $ \sp -> do
      let setSpan' k v = setTag sp (fromString k) (fromString v)
      act setSpan'
  | otherwise = act (\_ _ -> pure ())

withEventTrace :: (MonadMask m, MonadIO m) => String -> ((ByteString -> m ()) -> m a) -> m a
withEventTrace name act
  | userTracingEnabled
  = withSpan (fromString name) $ \sp -> do
      act (addEvent sp "")
  | otherwise = act (\_ -> pure ())

-- | Returns a logger that produces telemetry events in a single span
withTelemetryRecorder :: (MonadIO m, MonadMask m) => (Recorder (WithPriority (Doc a)) -> m c) -> m c
withTelemetryRecorder k = withSpan "Logger" $ \sp ->
    -- Tracy doesn't like when we create a new span for every log line.
    -- To workaround that, we create a single span for all log events.
    -- This is fine since we don't care about the span itself, only about the events
    k $ telemetryLogRecorder sp

-- | Returns a logger that produces telemetry events in a single span.
telemetryLogRecorder :: SpanInFlight -> Recorder (WithPriority (Doc a))
telemetryLogRecorder sp = Recorder $ \WithPriority {..} ->
  liftIO $ addEvent sp (fromString $ show priority) (encodeUtf8 $ trim $ renderStrict $ layoutCompact $ payload)
  where
    -- eventlog message size is limited by EVENT_PAYLOAD_SIZE_MAX = STG_WORD16_MAX
    trim = T.take (fromIntegral(maxBound :: Word16) - 10)

-- | Trace a handler using OpenTelemetry. Adds various useful info into tags in the OpenTelemetry span.
otTracedHandler
    :: MonadUnliftIO m
    => String -- ^ Message type
    -> String -- ^ Message label
    -> (SpanInFlight -> m a)
    -> m a
otTracedHandler requestType label act
  | userTracingEnabled = do
    let !name =
            if null label
            then requestType
            else requestType <> ":" <> show label
    -- Add an event so all requests can be quickly seen in the viewer without searching
    runInIO <- askRunInIO
    liftIO $ withSpan (fromString name) (\sp -> addEvent sp "" (fromString $ name <> " received") >> runInIO (act sp))
  | otherwise = act (SpanInFlight 0)

otSetUri :: SpanInFlight -> Uri -> IO ()
otSetUri sp (Uri t) = setTag sp "uri" (encodeUtf8 t)

-- | Trace a Shake action using opentelemetry.
otTracedAction
    :: Show k
    => k -- ^ The Action's Key
    -> NormalizedFilePath -- ^ Path to the file the action was run for
    -> RunMode
    -> (a -> String)
    -> (([FileDiagnostic] -> Action ()) -> Action (RunResult a)) -- ^ The action
    -> Action (RunResult a)
otTracedAction key file mode result act
  | userTracingEnabled = fst <$>
    generalBracket
        (do
            sp <- beginSpan (fromString (show key))
            setTag sp "File" (fromString $ fromNormalizedFilePath file)
            setTag sp "Mode" (fromString $ show mode)
            return sp
        )
        (\sp ec -> do
          case ec of
            ExitCaseAbort -> setTag sp "aborted" "1"
            ExitCaseException e -> setTag sp "exception" (pack $ show e)
            ExitCaseSuccess res -> do
                setTag sp "result" (pack $ result $ runValue res)
                setTag sp "changed" $ case res of
                    RunResult x _ _ _ -> fromString $ show x
          endSpan sp)
        (\sp -> act (liftIO . setTag sp "diagnostics" . encodeUtf8 . showDiagnostics ))
  | otherwise = act (\_ -> return ())

otTracedGarbageCollection :: (MonadMask f, MonadIO f, Show a) => ByteString -> f [a] -> f [a]
otTracedGarbageCollection label act
  | userTracingEnabled = fst <$>
      generalBracket
        (beginSpan label)
        (\sp ec -> do
            case ec of
                ExitCaseAbort -> setTag sp "aborted" "1"
                ExitCaseException e -> setTag sp "exception" (pack $ show e)
                ExitCaseSuccess res -> setTag sp "keys" (pack $ unlines $ map show res)
            endSpan sp)
        (const act)
  | otherwise = act

otTracedProvider :: MonadUnliftIO m => PluginId -> ByteString -> m a -> m a
otTracedProvider (PluginId pluginName) provider act
  | userTracingEnabled = do
    runInIO <- askRunInIO
    liftIO $ withSpan (provider <> " provider") $ \sp -> do
        setTag sp "plugin" (encodeUtf8 pluginName)
        runInIO act
  | otherwise = act

