-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ConstraintKinds            #-}

-- | A Shake implementation of the compiler service.
--
--   There are two primary locations where data lives, and both of
--   these contain much the same data:
--
-- * The Shake database (inside 'shakeDb') stores a map of shake keys
--   to shake values. In our case, these are all of type 'Q' to 'A'.
--   During a single run all the values in the Shake database are consistent
--   so are used in conjunction with each other, e.g. in 'uses'.
--
-- * The 'Values' type stores a map of keys to values. These values are
--   always stored as real Haskell values, whereas Shake serialises all 'A' values
--   between runs. To deserialise a Shake value, we just consult Values.
module Development.IDE.Core.Shake(
    IdeState,
    IdeRule, IdeResult, GetModificationTime(..),
    shakeOpen, shakeShut,
    shakeRun,
    shakeProfile,
    use, useWithStale, useNoFile, uses, usesWithStale,
    use_, useNoFile_, uses_,
    define, defineEarlyCutoff,
    getDiagnostics, unsafeClearDiagnostics,
    IsIdeGlobal, addIdeGlobal, getIdeGlobalState, getIdeGlobalAction,
    garbageCollect,
    setPriority,
    sendEvent,
    ideLogger,
    actionLogger,
    FileVersion(..),
    Priority(..),
    updatePositionMapping
    ) where

import           Development.Shake hiding (ShakeValue)
import           Development.Shake.Database
import           Development.Shake.Classes
import           Development.Shake.Rule
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Map
import qualified Data.ByteString.Char8 as BS
import           Data.Dynamic
import           Data.Maybe
import Data.Map.Strict (Map)
import           Data.List.Extra
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Tuple.Extra
import Data.Unique
import Development.IDE.Core.Debouncer
import Development.IDE.Core.PositionMapping
import Development.IDE.Types.Logger hiding (Priority)
import Language.Haskell.LSP.Diagnostics
import qualified Data.SortedList as SL
import           Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import           Control.Concurrent.Extra
import           Control.Exception
import           Control.DeepSeq
import           System.Time.Extra
import           Data.Typeable
import qualified Language.Haskell.LSP.Messages as LSP
import qualified Language.Haskell.LSP.Types as LSP
import           System.FilePath hiding (makeRelative)
import qualified Development.Shake as Shake
import           Control.Monad.Extra
import           Data.Time
import           GHC.Generics
import           System.IO.Unsafe
import           Numeric.Extra
import Language.Haskell.LSP.Types


-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    {eventer :: LSP.FromServerMessage -> IO ()
    ,debouncer :: Debouncer NormalizedUri
    ,logger :: Logger
    ,globals :: Var (HMap.HashMap TypeRep Dynamic)
    ,state :: Var Values
    ,diagnostics :: Var DiagnosticStore
    ,publishedDiagnostics :: Var (Map NormalizedUri [Diagnostic])
    -- ^ This represents the set of diagnostics that we have published.
    -- Due to debouncing not every change might get published.
    ,positionMapping :: Var (Map NormalizedUri (Map TextDocumentVersion PositionMapping))
    -- ^ Map from a text document version to a PositionMapping that describes how to map
    -- positions in a version of that document to positions in the latest version
    }

getShakeExtras :: Action ShakeExtras
getShakeExtras = do
    Just x <- getShakeExtra @ShakeExtras
    return x

getShakeExtrasRules :: Rules ShakeExtras
getShakeExtrasRules = do
    Just x <- getShakeExtraRules @ShakeExtras
    return x



class Typeable a => IsIdeGlobal a where

addIdeGlobal :: IsIdeGlobal a => a -> Rules ()
addIdeGlobal x@(typeOf -> ty) = do
    ShakeExtras{globals} <- getShakeExtrasRules
    liftIO $ modifyVar_ globals $ \mp -> case HMap.lookup ty mp of
        Just _ -> error $ "Can't addIdeGlobal twice on the same type, got " ++ show ty
        Nothing -> return $! HMap.insert ty (toDyn x) mp


getIdeGlobalExtras :: forall a . IsIdeGlobal a => ShakeExtras -> IO a
getIdeGlobalExtras ShakeExtras{globals} = do
    Just x <- HMap.lookup (typeRep (Proxy :: Proxy a)) <$> readVar globals
    return $ fromDyn x $ error "Serious error, corrupt globals"

getIdeGlobalAction :: forall a . IsIdeGlobal a => Action a
getIdeGlobalAction = liftIO . getIdeGlobalExtras =<< getShakeExtras

getIdeGlobalState :: forall a . IsIdeGlobal a => IdeState -> IO a
getIdeGlobalState = getIdeGlobalExtras . shakeExtras


-- | The state of the all values.
type Values = HMap.HashMap (NormalizedFilePath, Key) (Value Dynamic)

-- | Key type
data Key = forall k . (Typeable k, Hashable k, Eq k, Show k) => Key k

instance Show Key where
  show (Key k) = show k

instance Eq Key where
    Key k1 == Key k2 | Just k2' <- cast k2 = k1 == k2'
                     | otherwise = False

instance Hashable Key where
    hashWithSalt salt (Key key) = hashWithSalt salt key

-- | The result of an IDE operation. Warnings and errors are in the Diagnostic,
--   and a value is in the Maybe. For operations that throw an error you
--   expect a non-empty list of diagnostics, at least one of which is an error,
--   and a Nothing. For operations that succeed you expect perhaps some warnings
--   and a Just. For operations that depend on other failing operations you may
--   get empty diagnostics and a Nothing, to indicate this phase throws no fresh
--   errors but still failed.
--
--   A rule on a file should only return diagnostics for that given file. It should
--   not propagate diagnostic errors through multiple phases.
type IdeResult v = ([FileDiagnostic], Maybe v)

data Value v
    = Succeeded TextDocumentVersion v
    | Stale TextDocumentVersion v
    | Failed
    deriving (Functor, Generic, Show)

instance NFData v => NFData (Value v)

-- | Convert a Value to a Maybe. This will only return `Just` for
-- up2date results not for stale values.
currentValue :: Value v -> Maybe v
currentValue (Succeeded _ v) = Just v
currentValue (Stale _ _) = Nothing
currentValue Failed = Nothing

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValue :: NormalizedFilePath -> Value v -> Action (Maybe (v, PositionMapping))
lastValue file v = do
    ShakeExtras{positionMapping} <- getShakeExtras
    allMappings <- liftIO $ readVar positionMapping
    pure $ case v of
        Succeeded ver v -> Just (v, mappingForVersion allMappings file ver)
        Stale ver v -> Just (v, mappingForVersion allMappings file ver)
        Failed -> Nothing

valueVersion :: Value v -> Maybe TextDocumentVersion
valueVersion = \case
    Succeeded ver _ -> Just ver
    Stale ver _ -> Just ver
    Failed -> Nothing

mappingForVersion
    :: Map NormalizedUri (Map TextDocumentVersion PositionMapping)
    -> NormalizedFilePath
    -> TextDocumentVersion
    -> PositionMapping
mappingForVersion allMappings file ver =
    fromMaybe idMapping $
    Map.lookup ver =<<
    Map.lookup (filePathToUri' file) allMappings

type IdeRule k v =
  ( Shake.RuleResult k ~ v
  , Show k
  , Typeable k
  , NFData k
  , Hashable k
  , Eq k
  , Show v
  , Typeable v
  , NFData v
  )

-- | A Shake database plus persistent store. Can be thought of as storing
--   mappings from @(FilePath, k)@ to @RuleResult k@.
data IdeState = IdeState
    {shakeDb :: ShakeDatabase
    ,shakeAbort :: Var (IO ()) -- close whoever was running last
    ,shakeClose :: IO ()
    ,shakeExtras :: ShakeExtras
    ,shakeProfileDir :: Maybe FilePath
    }


-- This is debugging code that generates a series of profiles, if the Boolean is true
shakeRunDatabaseProfile :: Maybe FilePath -> ShakeDatabase -> [Action a] -> IO [a]
shakeRunDatabaseProfile mbProfileDir shakeDb acts = do
        (time, (res,_)) <- duration $ shakeRunDatabase shakeDb acts
        whenJust mbProfileDir $ \dir -> do
            count <- modifyVar profileCounter $ \x -> let !y = x+1 in return (y,y)
            let file = "ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) ++ "-" ++ showDP 2 time <.> "html"
            shakeProfileDatabase shakeDb $ dir </> file
        return res
    where

{-# NOINLINE profileStartTime #-}
profileStartTime :: String
profileStartTime = unsafePerformIO $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime

{-# NOINLINE profileCounter #-}
profileCounter :: Var Int
profileCounter = unsafePerformIO $ newVar 0

setValues :: IdeRule k v
          => Var Values
          -> k
          -> NormalizedFilePath
          -> Value v
          -> IO ()
setValues state key file val = modifyVar_ state $ \vals -> do
    -- Force to make sure the old HashMap is not retained
    evaluate $ HMap.insert (file, Key key) (fmap toDyn val) vals

-- | We return Nothing if the rule has not run and Just Failed if it has failed to produce a value.
getValues :: forall k v. IdeRule k v => Var Values -> k -> NormalizedFilePath -> IO (Maybe (Value v))
getValues state key file = do
    vs <- readVar state
    case HMap.lookup (file, Key key) vs of
        Nothing -> pure Nothing
        Just v -> do
            let r = fmap (fromJust . fromDynamic @v) v
            -- Force to make sure we do not retain a reference to the HashMap
            -- and we blow up immediately if the fromJust should fail
            -- (which would be an internal error).
            evaluate (r `seqValue` Just r)

-- | Seq the result stored in the Shake value. This only
-- evaluates the value to WHNF not NF. We take care of the latter
-- elsewhere and doing it twice is expensive.
seqValue :: Value v -> b -> b
seqValue v b = case v of
    Succeeded ver v -> rnf ver `seq` v `seq` b
    Stale ver v -> rnf ver `seq` v `seq` b
    Failed -> b

-- | Open a 'IdeState', should be shut using 'shakeShut'.
shakeOpen :: (LSP.FromServerMessage -> IO ()) -- ^ diagnostic handler
          -> Logger
          -> Maybe FilePath
          -> IdeReportProgress
          -> ShakeOptions
          -> Rules ()
          -> IO IdeState
shakeOpen eventer logger shakeProfileDir (IdeReportProgress reportProgress) opts rules = do
    shakeExtras <- do
        globals <- newVar HMap.empty
        state <- newVar HMap.empty
        diagnostics <- newVar mempty
        publishedDiagnostics <- newVar mempty
        debouncer <- newDebouncer
        positionMapping <- newVar Map.empty
        pure ShakeExtras{..}
    (shakeDb, shakeClose) <-
        shakeOpenDatabase
            opts
                { shakeExtra = addShakeExtra shakeExtras $ shakeExtra opts
                , shakeProgress = if reportProgress then lspShakeProgress eventer else const (pure ())
                }
            rules
    shakeAbort <- newVar $ return ()
    shakeDb <- shakeDb
    return IdeState{..}

lspShakeProgress :: (LSP.FromServerMessage -> IO ()) -> IO Progress -> IO ()
lspShakeProgress sendMsg prog = do
    u <- T.pack . show . hashUnique <$> newUnique
    bracket_ (start u) (stop u) (loop u)
    where
        start id = sendMsg $ LSP.NotProgressStart $ LSP.fmServerProgressStartNotification
            ProgressStartParams
                { _id = id
                , _title = "Processing"
                , _cancellable = Nothing
                , _message = Nothing
                , _percentage = Nothing
                }
        stop id = sendMsg $ LSP.NotProgressDone $ LSP.fmServerProgressDoneNotification
            ProgressDoneParams
                { _id = id
                }
        sample = 0.1
        loop id = forever $ do
            sleep sample
            p <- prog
            let done = countSkipped p + countBuilt p
            let todo = done + countUnknown p + countTodo p
            sendMsg $ LSP.NotProgressReport $ LSP.fmServerProgressReportNotification
                ProgressReportParams
                    { _id = id
                    , _message = Just $ T.pack $ show done <> "/" <> show todo
                    , _percentage = Nothing
                    }

shakeProfile :: IdeState -> FilePath -> IO ()
shakeProfile IdeState{..} = shakeProfileDatabase shakeDb

shakeShut :: IdeState -> IO ()
shakeShut IdeState{..} = withVar shakeAbort $ \stop -> do
    -- Shake gets unhappy if you try to close when there is a running
    -- request so we first abort that.
    stop
    shakeClose

-- | Spawn immediately. If you are already inside a call to shakeRun that will be aborted with an exception.
shakeRun :: IdeState -> [Action a] -> IO (IO [a])
-- FIXME: If there is already a shakeRun queued up and waiting to send me a kill, I should probably
--        not even start, which would make issues with async exceptions less problematic.
shakeRun IdeState{shakeExtras=ShakeExtras{..}, ..} acts = modifyVar shakeAbort $ \stop -> do
    (stopTime,_) <- duration stop
    logDebug logger $ T.pack $ "Starting shakeRun (aborting the previous one took " ++ showDuration stopTime ++ ")"
    bar <- newBarrier
    start <- offsetTime
    thread <- forkFinally (shakeRunDatabaseProfile shakeProfileDir shakeDb acts) $ \res -> do
        runTime <- start
        let res' = case res of
                Left e -> "exception: " <> displayException e
                Right _ -> "completed"
        logDebug logger $ T.pack $
            "Finishing shakeRun (took " ++ showDuration runTime ++ ", " ++ res' ++ ")"
        signalBarrier bar res
    -- important: we send an async exception to the thread, then wait for it to die, before continuing
    return (do killThread thread; void $ waitBarrier bar, either throwIO return =<< waitBarrier bar)

getDiagnostics :: IdeState -> IO [FileDiagnostic]
getDiagnostics IdeState{shakeExtras = ShakeExtras{diagnostics}} = do
    val <- readVar diagnostics
    return $ getAllDiagnostics val

-- | FIXME: This function is temporary! Only required because the files of interest doesn't work
unsafeClearDiagnostics :: IdeState -> IO ()
unsafeClearDiagnostics IdeState{shakeExtras = ShakeExtras{diagnostics}} =
    writeVar diagnostics mempty

-- | Clear the results for all files that do not match the given predicate.
garbageCollect :: (NormalizedFilePath -> Bool) -> Action ()
garbageCollect keep = do
    ShakeExtras{state, diagnostics,publishedDiagnostics,positionMapping} <- getShakeExtras
    liftIO $
        do newState <- modifyVar state $ \values -> do
               values <- evaluate $ HMap.filterWithKey (\(file, _) _ -> keep file) values
               return $! dupe values
           modifyVar_ diagnostics $ \diags -> return $! filterDiagnostics keep diags
           modifyVar_ publishedDiagnostics $ \diags -> return $! Map.filterWithKey (\uri _ -> keep (fromUri uri)) diags
           let versionsForFile =
                   Map.fromListWith Set.union $
                   mapMaybe (\((file, _key), v) -> (filePathToUri' file,) . Set.singleton <$> valueVersion v) $
                   HMap.toList newState
           modifyVar_ positionMapping $ \mappings -> return $! filterVersionMap versionsForFile mappings
define
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define op = defineEarlyCutoff $ \k v -> (Nothing,) <$> op k v

use :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe v)
use key file = head <$> uses key [file]

useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
useWithStale key file = head <$> usesWithStale key [file]

useNoFile :: IdeRule k v => k -> Action (Maybe v)
useNoFile key = use key ""

use_ :: IdeRule k v => k -> NormalizedFilePath -> Action v
use_ key file = head <$> uses_ key [file]

useNoFile_ :: IdeRule k v => k -> Action v
useNoFile_ key = use_ key ""

uses_ :: IdeRule k v => k -> [NormalizedFilePath] -> Action [v]
uses_ key files = do
    res <- uses key files
    case sequence res of
        Nothing -> liftIO $ throwIO BadDependency
        Just v -> return v


-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
data BadDependency = BadDependency deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (x :: ShakeException) <- fromException x = isBadDependency $ shakeExceptionInner x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False

newtype Q k = Q (k, NormalizedFilePath)
    deriving (Eq,Hashable,NFData)

-- Using Database we don't need Binary instances for keys
instance Binary (Q k) where
    put _ = return ()
    get = fail "Binary.get not defined for type Development.IDE.Core.Shake.Q"

instance Show k => Show (Q k) where
    show (Q (k, file)) = show k ++ "; " ++ fromNormalizedFilePath file

-- | Invariant: the 'v' must be in normal form (fully evaluated).
--   Otherwise we keep repeatedly 'rnf'ing values taken from the Shake database
-- Note (MK) I am not sure why we need the ShakeValue here, maybe we
-- can just remove it?
data A v = A (Value v) ShakeValue
    deriving Show

instance NFData (A v) where rnf (A v x) = v `seq` rnf x

-- In the Shake database we only store one type of key/result pairs,
-- namely Q (question) / A (answer).
type instance RuleResult (Q k) = A (RuleResult k)


-- | Return up2date results. Stale results will be ignored.
uses :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe v]
uses key files = map (\(A value _) -> currentValue value) <$> apply (map (Q . (key,)) files)

-- | Return the last computed result which might be stale.
usesWithStale :: IdeRule k v
    => k -> [NormalizedFilePath] -> Action [Maybe (v, PositionMapping)]
usesWithStale key files = do
    values <- map (\(A value _) -> value) <$> apply (map (Q . (key,)) files)
    mapM (uncurry lastValue) (zip files values)

defineEarlyCutoff
    :: IdeRule k v
    => (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
    -> Rules ()
defineEarlyCutoff op = addBuiltinRule noLint noIdentity $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> do
    extras@ShakeExtras{state} <- getShakeExtras
    val <- case old of
        Just old | mode == RunDependenciesSame -> do
            v <- liftIO $ getValues state key file
            case v of
                -- No changes in the dependencies and we have
                -- an existing result.
                Just v -> return $ Just $ RunResult ChangedNothing old $ A v (decodeShakeValue old)
                _ -> return Nothing
        _ -> return Nothing
    case val of
        Just res -> return res
        Nothing -> do
            (bs, (diags, res)) <- actionCatch
                (do v <- op key file; liftIO $ evaluate $ force $ v) $
                \(e :: SomeException) -> pure (Nothing, ([ideErrorText file $ T.pack $ show e | not $ isBadDependency e],Nothing))
            modTime <- liftIO $ join . fmap currentValue <$> getValues state GetModificationTime file
            (bs, res) <- case res of
                Nothing -> do
                    staleV <- liftIO $ getValues state key file
                    pure $ case staleV of
                        Nothing -> (toShakeValue ShakeResult bs, Failed)
                        Just v -> case v of
                            Succeeded ver v -> (toShakeValue ShakeStale bs, Stale ver v)
                            Stale ver v -> (toShakeValue ShakeStale bs, Stale ver v)
                            Failed -> (toShakeValue ShakeResult bs, Failed)
                Just v -> pure $ (maybe ShakeNoCutoff ShakeResult bs, Succeeded (vfsVersion =<< modTime) v)
            liftIO $ setValues state key file res
            updateFileDiagnostics file (Key key) extras $ map snd diags
            let eq = case (bs, fmap decodeShakeValue old) of
                    (ShakeResult a, Just (ShakeResult b)) -> a == b
                    (ShakeStale a, Just (ShakeStale b)) -> a == b
                    -- If we do not have a previous result
                    -- or we got ShakeNoCutoff we always return False.
                    _ -> False
            return $ RunResult
                (if eq then ChangedRecomputeSame else ChangedRecomputeDiff)
                (encodeShakeValue bs) $
                A res bs

toShakeValue :: (BS.ByteString -> ShakeValue) -> Maybe BS.ByteString -> ShakeValue
toShakeValue = maybe ShakeNoCutoff

data ShakeValue
    = ShakeNoCutoff
    -- ^ This is what we use when we get Nothing from
    -- a rule.
    | ShakeResult !BS.ByteString
    -- ^ This is used both for `Failed`
    -- as well as `Succeeded`.
    | ShakeStale !BS.ByteString
    deriving (Generic, Show)

instance NFData ShakeValue

encodeShakeValue :: ShakeValue -> BS.ByteString
encodeShakeValue = \case
    ShakeNoCutoff -> BS.empty
    ShakeResult r -> BS.cons 'r' r
    ShakeStale r -> BS.cons 's' r

decodeShakeValue :: BS.ByteString -> ShakeValue
decodeShakeValue bs = case BS.uncons bs of
    Nothing -> ShakeNoCutoff
    Just (x, xs)
      | x == 'r' -> ShakeResult xs
      | x == 's' -> ShakeStale xs
      | otherwise -> error $ "Failed to parse shake value " <> show bs


updateFileDiagnostics ::
     NormalizedFilePath
  -> Key
  -> ShakeExtras
  -> [Diagnostic] -- ^ current results
  -> Action ()
updateFileDiagnostics fp k ShakeExtras{diagnostics, publishedDiagnostics, state, debouncer, eventer} current = liftIO $ do
    modTime <- join . fmap currentValue <$> getValues state GetModificationTime fp
    mask_ $ do
        -- Mask async exceptions to ensure that updated diagnostics are always
        -- published. Otherwise, we might never publish certain diagnostics if
        -- an exception strikes between modifyVar but before
        -- publishDiagnosticsNotification.
        newDiags <- modifyVar diagnostics $ \old -> do
            let newDiagsStore = setStageDiagnostics fp (vfsVersion =<< modTime) (T.pack $ show k) current old
            let newDiags = getFileDiagnostics fp newDiagsStore
            _ <- evaluate newDiagsStore
            _ <- evaluate newDiags
            pure $! (newDiagsStore, newDiags)
        let uri = filePathToUri' fp
        let delay = if null newDiags then 0.1 else 0
        registerEvent debouncer delay uri $ do
             mask_ $ modifyVar_ publishedDiagnostics $ \published -> do
                 let lastPublish = Map.findWithDefault [] uri published
                 when (lastPublish /= newDiags) $
                     eventer $ publishDiagnosticsNotification (fromNormalizedUri uri) newDiags
                 pure $! Map.insert uri newDiags published

publishDiagnosticsNotification :: Uri -> [Diagnostic] -> LSP.FromServerMessage
publishDiagnosticsNotification uri diags =
    LSP.NotPublishDiagnostics $
    LSP.NotificationMessage "2.0" LSP.TextDocumentPublishDiagnostics $
    LSP.PublishDiagnosticsParams uri (List diags)

newtype Priority = Priority Double

setPriority :: Priority -> Action ()
setPriority (Priority p) = deprioritize p

sendEvent :: LSP.FromServerMessage -> Action ()
sendEvent e = do
    ShakeExtras{eventer} <- getShakeExtras
    liftIO $ eventer e

ideLogger :: IdeState -> Logger
ideLogger IdeState{shakeExtras=ShakeExtras{logger}} = logger

actionLogger :: Action Logger
actionLogger = do
    ShakeExtras{logger} <- getShakeExtras
    return logger


data GetModificationTime = GetModificationTime
    deriving (Eq, Show, Generic)
instance Hashable GetModificationTime
instance NFData   GetModificationTime

-- | Get the modification time of a file.
type instance RuleResult GetModificationTime = FileVersion

-- | We store the modification time as a ByteString since we need
-- a ByteString anyway for Shake and we do not care about how times
-- are represented.
data FileVersion = VFSVersion Int | ModificationTime BS.ByteString
    deriving (Show, Generic)

instance NFData FileVersion

vfsVersion :: FileVersion -> Maybe Int
vfsVersion (VFSVersion i) = Just i
vfsVersion (ModificationTime _) = Nothing



getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) = concatMap SL.fromSortedList $ Map.elems diags


-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setStageDiagnostics
    :: NormalizedFilePath
    -> TextDocumentVersion -- ^ the time that the file these diagnostics originate from was last edited
    -> T.Text
    -> [LSP.Diagnostic]
    -> DiagnosticStore
    -> DiagnosticStore
setStageDiagnostics fp timeM stage diags ds  =
    updateDiagnostics ds uri timeM diagsBySource
    where
        diagsBySource = Map.singleton (Just stage) (SL.toSortedList diags)
        uri = filePathToUri' fp

getAllDiagnostics ::
    DiagnosticStore ->
    [FileDiagnostic]
getAllDiagnostics =
    concatMap (\(k,v) -> map (fromUri k,) $ getDiagnosticsFromStore v) . Map.toList

getFileDiagnostics ::
    NormalizedFilePath ->
    DiagnosticStore ->
    [LSP.Diagnostic]
getFileDiagnostics fp ds =
    maybe [] getDiagnosticsFromStore $
    Map.lookup (filePathToUri' fp) ds

filterDiagnostics ::
    (NormalizedFilePath -> Bool) ->
    DiagnosticStore ->
    DiagnosticStore
filterDiagnostics keep =
    Map.filterWithKey (\uri _ -> maybe True (keep . toNormalizedFilePath) $ uriToFilePath' $ fromNormalizedUri uri)

filterVersionMap
    :: Map NormalizedUri (Set.Set TextDocumentVersion)
    -> Map NormalizedUri (Map TextDocumentVersion a)
    -> Map NormalizedUri (Map TextDocumentVersion a)
filterVersionMap =
    Map.merge Map.dropMissing Map.dropMissing $
    Map.zipWithMatched $ \_ versionsToKeep versionMap -> Map.restrictKeys versionMap versionsToKeep

updatePositionMapping :: IdeState -> VersionedTextDocumentIdentifier -> List TextDocumentContentChangeEvent -> IO ()
updatePositionMapping IdeState{shakeExtras = ShakeExtras{positionMapping}} VersionedTextDocumentIdentifier{..} changes = do
    modifyVar_ positionMapping $ \allMappings -> do
        let uri = toNormalizedUri _uri
        let mappingForUri = Map.findWithDefault Map.empty uri allMappings
        let updatedMapping =
                Map.insert _version idMapping $
                Map.map (\oldMapping -> foldl' applyChange oldMapping changes) mappingForUri
        pure $! Map.insert uri updatedMapping allMappings
