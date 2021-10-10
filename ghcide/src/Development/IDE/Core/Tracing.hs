{-# LANGUAGE CPP             #-}
{-# LANGUAGE NoApplicativeDo #-}
{-# HLINT ignore #-}
module Development.IDE.Core.Tracing
    ( otTracedHandler
    , otTracedAction
    , startProfilingTelemetry
    , measureMemory
    , getInstrumentCached
    , otTracedProvider
    , otSetUri
    , otTracedGarbageCollection
    , withTrace
    , withEventTrace
    )
where

import           Control.Concurrent.Async       (Async, async)
import           Control.Concurrent.Extra       (Var, modifyVar_, newVar,
                                                 readVar, threadDelay)
import           Control.Exception              (evaluate)
import           Control.Exception.Safe         (SomeException, catch,
                                                 generalBracket)
import           Control.Monad                  (forM_, forever, void, when,
                                                 (>=>))
import           Control.Monad.Catch            (ExitCase (..), MonadMask)
import           Control.Monad.Extra            (whenJust)
import           Control.Monad.IO.Unlift
import           Control.Seq                    (r0, seqList, seqTuple2, using)
import           Data.ByteString                (ByteString)
import           Data.ByteString.Char8          (pack)
import           Data.Dynamic                   (Dynamic)
import qualified Data.HashMap.Strict            as HMap
import           Data.IORef                     (modifyIORef', newIORef,
                                                 readIORef, writeIORef)
import           Data.String                    (IsString (fromString))
import           Data.Text.Encoding             (encodeUtf8)
import           Data.Typeable                  (TypeRep, typeOf)
import           Debug.Trace.Flags              (userTracingEnabled)
import           Development.IDE.Core.RuleTypes (GhcSession (GhcSession),
                                                 GhcSessionDeps (GhcSessionDeps),
                                                 GhcSessionIO (GhcSessionIO))
import           Development.IDE.Graph          (Action)
import           Development.IDE.Graph.Rule
import           Development.IDE.Types.Location (Uri (..))
import           Development.IDE.Types.Logger   (Logger, logDebug, logInfo)
import           Development.IDE.Types.Shake    (Value,
                                                 ValueWithDiagnostics (..),
                                                 Values, fromKeyType)
import           Foreign.Storable               (Storable (sizeOf))
import           HeapSize                       (recursiveSize, runHeapsize)
import           Ide.PluginUtils                (installSigUsr1Handler)
import           Ide.Types                      (PluginId (..))
import           Language.LSP.Types             (NormalizedFilePath,
                                                 fromNormalizedFilePath)
import           Numeric.Natural                (Natural)
import           OpenTelemetry.Eventlog         (SpanInFlight (..), addEvent,
                                                 beginSpan, endSpan,
                                                 mkValueObserver, observe,
                                                 setTag, withSpan, withSpan_)

withTrace :: (MonadMask m, MonadIO m) =>
    String -> ((String -> String -> m ()) -> m a) -> m a
withTrace name act
  | userTracingEnabled
  = withSpan (fromString name) $ \sp -> do
      let setSpan' k v = setTag sp (fromString k) (fromString v)
      act setSpan'
  | otherwise = act (\_ _ -> pure ())

#if MIN_VERSION_ghc(8,8,0)
withEventTrace :: (MonadMask m, MonadIO m) => String -> ((ByteString -> ByteString -> m ()) -> m a) -> m a
#else
withEventTrace :: (MonadMask m, MonadIO m) => String -> ((String -> ByteString -> m ()) -> m a) -> m a
#endif
withEventTrace name act
  | userTracingEnabled
  = withSpan (fromString name) $ \sp -> do
      act (addEvent sp)
  | otherwise = act (\_ _ -> pure ())

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
    -> Action (RunResult a) -- ^ The action
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
                    RunResult x _ _ -> fromString $ show x
          endSpan sp)
        (const act)
  | otherwise = act

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

#if MIN_VERSION_ghc(8,8,0)
otTracedProvider :: MonadUnliftIO m => PluginId -> ByteString -> m a -> m a
#else
otTracedProvider :: MonadUnliftIO m => PluginId -> String -> m a -> m a
#endif
otTracedProvider (PluginId pluginName) provider act
  | userTracingEnabled = do
    runInIO <- askRunInIO
    liftIO $ withSpan (provider <> " provider") $ \sp -> do
        setTag sp "plugin" (encodeUtf8 pluginName)
        runInIO act
  | otherwise = act


startProfilingTelemetry :: Bool -> Logger -> Var Values -> IO ()
startProfilingTelemetry allTheTime logger stateRef = do
    instrumentFor <- getInstrumentCached

    installSigUsr1Handler $ do
        logInfo logger "SIGUSR1 received: performing memory measurement"
        performMeasurement logger stateRef instrumentFor

    when allTheTime $ void $ regularly (1 * seconds) $
        performMeasurement logger stateRef instrumentFor
  where
        seconds = 1000000

        regularly :: Int -> IO () -> IO (Async ())
        regularly delay act = async $ forever (act >> threadDelay delay)


performMeasurement ::
  Logger ->
  Var Values ->
  (Maybe String -> IO OurValueObserver) ->
  IO ()
performMeasurement logger stateRef instrumentFor = do

    values <- readVar stateRef
    let keys = typeOf GhcSession
             : typeOf GhcSessionDeps
             -- TODO restore
             : [ kty
                | k <- HMap.keys values
                , Just kty <- [fromKeyType k]
                -- do GhcSessionIO last since it closes over stateRef itself
                , kty /= typeOf GhcSession
                , kty /= typeOf GhcSessionDeps
                , kty /= typeOf GhcSessionIO
             ]
             ++ [typeOf GhcSessionIO]
    groupedForSharing <- evaluate (keys `using` seqList r0)
    measureMemory logger [groupedForSharing] instrumentFor stateRef
        `catch` \(e::SomeException) ->
        logInfo logger ("MEMORY PROFILING ERROR: " <> fromString (show e))


type OurValueObserver = Int -> IO ()

getInstrumentCached :: IO (Maybe String -> IO OurValueObserver)
getInstrumentCached = do
    instrumentMap <- newVar HMap.empty
    mapBytesInstrument <- mkValueObserver "value map size_bytes"

    let instrumentFor k = do
          mb_inst <- HMap.lookup k <$> readVar instrumentMap
          case mb_inst of
            Nothing -> do
                instrument <- mkValueObserver (fromString (show k ++ " size_bytes"))
                modifyVar_ instrumentMap (return . HMap.insert k instrument)
                return $ observe instrument
            Just v -> return $ observe v
    return $ maybe (return $ observe mapBytesInstrument) instrumentFor

whenNothing :: IO () -> IO (Maybe a) -> IO ()
whenNothing act mb = mb >>= f
  where f Nothing = act
        f Just{}  = return ()

measureMemory
    :: Logger
    -> [[TypeRep]]     -- ^ Grouping of keys for the sharing-aware analysis
    -> (Maybe String -> IO OurValueObserver)
    -> Var Values
    -> IO ()
measureMemory logger groups instrumentFor stateRef = withSpan_ "Measure Memory" $ do
    values <- readVar stateRef
    valuesSizeRef <- newIORef $ Just 0
    let !groupsOfGroupedValues = groupValues values
    logDebug logger "STARTING MEMORY PROFILING"
    forM_ groupsOfGroupedValues $ \groupedValues -> do
        keepGoing <- readIORef valuesSizeRef
        whenJust keepGoing $ \_ ->
          whenNothing (writeIORef valuesSizeRef Nothing) $
          repeatUntilJust 3 $ do
          -- logDebug logger (fromString $ show $ map fst groupedValues)
          runHeapsize 25000000 $
              forM_ groupedValues $ \(k,v) -> withSpan ("Measure " <> fromString k) $ \sp -> do
              acc <- liftIO $ newIORef 0
              observe <- liftIO $ instrumentFor $ Just k
              mapM_ (recursiveSize >=> \x -> liftIO (modifyIORef' acc (+ x))) v
              size <- liftIO $ readIORef acc
              let !byteSize = sizeOf (undefined :: Word) * size
              setTag sp "size" (fromString (show byteSize ++ " bytes"))
              () <- liftIO $ observe byteSize
              liftIO $ modifyIORef' valuesSizeRef (fmap (+ byteSize))

    mbValuesSize <- readIORef valuesSizeRef
    case mbValuesSize of
        Just valuesSize -> do
            observe <- instrumentFor Nothing
            observe valuesSize
            logDebug logger "MEMORY PROFILING COMPLETED"
        Nothing ->
            logInfo logger "Memory profiling could not be completed: increase the size of your nursery (+RTS -Ax) and try again"

    where
        groupValues :: Values -> [ [(String, [Value Dynamic])] ]
        groupValues values =
            let !groupedValues =
                    [ [ (show ty, vv)
                      | ty <- groupKeys
                      , let vv = [ v | (fromKeyType -> Just kty, ValueWithDiagnostics v _) <- HMap.toList values
                                     , kty == ty]
                      ]
                    | groupKeys <- groups
                    ]
                -- force the spine of the nested lists
            in groupedValues `using` seqList (seqList (seqTuple2 r0 (seqList r0)))

repeatUntilJust :: Monad m => Natural -> m (Maybe a) -> m (Maybe a)
repeatUntilJust 0 _ = return Nothing
repeatUntilJust nattempts action = do
    res <- action
    case res of
        Nothing -> repeatUntilJust (nattempts-1) action
        Just{}  -> return res
