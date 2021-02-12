{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
#include "ghc-api-version.h"
module Development.IDE.Core.Tracing
    ( otTracedHandler
    , otTracedAction
    , startTelemetry
    , measureMemory
    , getInstrumentCached
    ,otTracedProvider,otSetUri)
where

import           Control.Concurrent.Async       (Async, async)
import           Control.Concurrent.Extra       (Var, modifyVar_, newVar,
                                                 readVar, threadDelay)
import           Control.Exception              (evaluate)
import           Control.Exception.Safe         (catch, SomeException)
import           Control.Monad                  (void, when, unless, forM_, forever, (>=>))
import           Control.Monad.Extra            (whenJust)
import           Control.Seq                    (r0, seqList, seqTuple2, using)
import           Data.Dynamic                   (Dynamic)
import qualified Data.HashMap.Strict            as HMap
import           Data.IORef                     (modifyIORef', newIORef,
                                                 readIORef, writeIORef)
import           Data.String                    (IsString (fromString))
import           Development.IDE.Core.RuleTypes (GhcSession (GhcSession),
                                                 GhcSessionDeps (GhcSessionDeps),
                                                 GhcSessionIO (GhcSessionIO))
import           Development.IDE.Types.Logger   (logInfo, Logger, logDebug)
import           Development.IDE.Types.Shake    (ValueWithDiagnostics(..), Key (..), Value, Values)
import           Development.Shake              (Action, actionBracket, liftIO)
import           Ide.PluginUtils                (installSigUsr1Handler)
import           Foreign.Storable               (Storable (sizeOf))
import           HeapSize                       (recursiveSize, runHeapsize)
import           Language.LSP.Types             (NormalizedFilePath,
                                                 fromNormalizedFilePath)
import           Numeric.Natural                (Natural)
import           OpenTelemetry.Eventlog         (SpanInFlight, Synchronicity(Asynchronous), Instrument, addEvent, beginSpan, endSpan,
                                                 mkValueObserver, observe,
                                                 setTag, withSpan, withSpan_)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Ide.Types (PluginId (..))
import Development.IDE.Types.Location (Uri (..))

-- | Trace a handler using OpenTelemetry. Adds various useful info into tags in the OpenTelemetry span.
otTracedHandler
    :: String -- ^ Message type
    -> String -- ^ Message label
    -> (SpanInFlight -> IO a)
    -> IO a
otTracedHandler requestType label act =
  let !name =
        if null label
          then requestType
          else requestType <> ":" <> show label
   -- Add an event so all requests can be quickly seen in the viewer without searching
   in withSpan (fromString name) (\sp -> addEvent sp "" (fromString $ name <> " received") >> act sp)

otSetUri :: SpanInFlight -> Uri -> IO ()
otSetUri sp (Uri t) = setTag sp "uri" (encodeUtf8 t)

-- | Trace a Shake action using opentelemetry.
otTracedAction
    :: Show k
    => k -- ^ The Action's Key
    -> NormalizedFilePath -- ^ Path to the file the action was run for
    -> (a -> Bool) -- ^ Did this action succeed?
    -> Action a -- ^ The action
    -> Action a
otTracedAction key file success act = actionBracket
    (do
        sp <- beginSpan (fromString (show key))
        setTag sp "File" (fromString $ fromNormalizedFilePath file)
        return sp
    )
    endSpan
    (\sp -> do
        res <- act
        unless (success res) $ setTag sp "error" "1"
        return res)

#if MIN_GHC_API_VERSION(8,8,0)
otTracedProvider :: PluginId -> ByteString -> IO a -> IO a
#else
otTracedProvider :: PluginId -> String -> IO a -> IO a
#endif
otTracedProvider (PluginId pluginName) provider act =
  withSpan (provider <> " provider") $ \sp -> do
    setTag sp "plugin" (encodeUtf8 pluginName)
    act

startTelemetry :: Bool -> Logger -> Var Values -> IO ()
startTelemetry allTheTime logger stateRef = do
    instrumentFor <- getInstrumentCached
    mapCountInstrument <- mkValueObserver "values map count"

    installSigUsr1Handler $ do
        logInfo logger "SIGUSR1 received: performing memory measurement"
        performMeasurement logger stateRef instrumentFor mapCountInstrument

    when allTheTime $ void $ regularly (1 * seconds) $
        performMeasurement logger stateRef instrumentFor mapCountInstrument
  where
        seconds = 1000000

        regularly :: Int -> IO () -> IO (Async ())
        regularly delay act = async $ forever (act >> threadDelay delay)


performMeasurement ::
  Logger ->
  Var Values ->
  (Maybe Key -> IO OurValueObserver) ->
  Instrument 'Asynchronous a m' ->
  IO ()
performMeasurement logger stateRef instrumentFor mapCountInstrument = do
    withSpan_ "Measure length" $ readVar stateRef >>= observe mapCountInstrument . length

    values <- readVar stateRef
    let keys = Key GhcSession
             : Key GhcSessionDeps
             : [ k | (_,k) <- HMap.keys values
                        -- do GhcSessionIO last since it closes over stateRef itself
                        , k /= Key GhcSession
                        , k /= Key GhcSessionDeps
                        , k /= Key GhcSessionIO
             ] ++ [Key GhcSessionIO]
    groupedForSharing <- evaluate (keys `using` seqList r0)
    measureMemory logger [groupedForSharing] instrumentFor stateRef
        `catch` \(e::SomeException) ->
        logInfo logger ("MEMORY PROFILING ERROR: " <> fromString (show e))


type OurValueObserver = Int -> IO ()

getInstrumentCached :: IO (Maybe Key -> IO OurValueObserver)
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
    -> [[Key]]     -- ^ Grouping of keys for the sharing-aware analysis
    -> (Maybe Key -> IO OurValueObserver)
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
              forM_ groupedValues $ \(k,v) -> withSpan ("Measure " <> (fromString $ show k)) $ \sp -> do
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
        groupValues :: Values -> [ [(Key, [Value Dynamic])] ]
        groupValues values =
            let !groupedValues =
                    [ [ (k, vv)
                      | k <- groupKeys
                      , let vv = [ v | ((_,k'), ValueWithDiagnostics v _) <- HMap.toList values , k == k']
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
