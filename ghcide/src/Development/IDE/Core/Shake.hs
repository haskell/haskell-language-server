-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeFamilies          #-}

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
    IdeState, shakeSessionInit, shakeExtras, shakeDb, rootDir,
    ShakeExtras(..), getShakeExtras, getShakeExtrasRules,
    KnownTargets, Target(..), toKnownFiles,
    IdeRule, IdeResult,
    GetModificationTime(GetModificationTime, GetModificationTime_, missingFileDiagnostics),
    shakeOpen, shakeShut,
    shakeEnqueue,
    newSession,
    use, useNoFile, uses, useWithStaleFast, useWithStaleFast', delayedAction,
    FastResult(..),
    use_, useNoFile_, uses_,
    useWithStale, usesWithStale,
    useWithStale_, usesWithStale_,
    BadDependency(..),
    RuleBody(..),
    define, defineNoDiagnostics,
    defineEarlyCutoff,
    defineNoFile, defineEarlyCutOffNoFile,
    getDiagnostics,
    mRunLspT, mRunLspTCallback,
    getHiddenDiagnostics,
    IsIdeGlobal, addIdeGlobal, addIdeGlobalExtras, getIdeGlobalState, getIdeGlobalAction,
    getIdeGlobalExtras,
    getIdeOptions,
    getIdeOptionsIO,
    GlobalIdeOptions(..),
    HLS.getClientConfig,
    getPluginConfigAction,
    knownTargets,
    ideLogger,
    actionLogger,
    getVirtualFile,
    FileVersion(..),
    updatePositionMapping,
    updatePositionMappingHelper,
    deleteValue,
    WithProgressFunc, WithIndefiniteProgressFunc,
    ProgressEvent(..),
    DelayedAction, mkDelayedAction,
    IdeAction(..), runIdeAction,
    mkUpdater,
    -- Exposed for testing.
    Q(..),
    IndexQueue,
    HieDb,
    HieDbWriter(..),
    addPersistentRule,
    garbageCollectDirtyKeys,
    garbageCollectDirtyKeysOlderThan,
    Log(..),
    VFSModified(..), getClientConfigAction,
    ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.Stats           (atomicallyNamed)
import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Exception.Extra                hiding (bracket_)
import           Control.Lens                           ((&), (?~))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.Aeson                             (Result (Success),
                                                         toJSON)
import qualified Data.Aeson.Types                       as A
import qualified Data.ByteString.Char8                  as BS
import qualified Data.ByteString.Char8                  as BS8
import           Data.Coerce                            (coerce)
import           Data.Default
import           Data.Dynamic
import           Data.EnumMap.Strict                    (EnumMap)
import qualified Data.EnumMap.Strict                    as EM
import           Data.Foldable                          (find, for_)
import           Data.Functor                           ((<&>))
import           Data.Functor.Identity
import           Data.Hashable
import qualified Data.HashMap.Strict                    as HMap
import           Data.HashSet                           (HashSet)
import qualified Data.HashSet                           as HSet
import           Data.List.Extra                        (foldl', partition,
                                                         takeEnd)
import qualified Data.Map.Strict                        as Map
import           Data.Maybe
import qualified Data.SortedList                        as SL
import           Data.String                            (fromString)
import qualified Data.Text                              as T
import           Data.Time
import           Data.Traversable
import           Data.Tuple.Extra
import           Data.Typeable
import           Data.Unique
import           Data.Vector                            (Vector)
import qualified Data.Vector                            as Vector
import           Development.IDE.Core.Debouncer
import           Development.IDE.Core.FileUtils         (getModTime)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Tracing
import           Development.IDE.GHC.Compat             (NameCache,
                                                         initNameCache,
                                                         knownKeyNames)
import           Development.IDE.GHC.Orphans            ()
import           Development.IDE.Graph                  hiding (ShakeValue,
                                                         action)
import qualified Development.IDE.Graph                  as Shake
import           Development.IDE.Graph.Database         (ShakeDatabase,
                                                         shakeGetBuildStep,
                                                         shakeGetDatabaseKeys,
                                                         shakeNewDatabase,
                                                         shakeProfileDatabase,
                                                         shakeRunDatabaseForKeys)
import           Development.IDE.Graph.Rule
import           Development.IDE.Types.Action
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Exports          hiding (exportsMapSize)
import qualified Development.IDE.Types.Exports          as ExportsMap
import           Development.IDE.Types.KnownTargets
import           Development.IDE.Types.Location
import           Development.IDE.Types.Monitoring       (Monitoring (..))
import           Development.IDE.Types.Options
import           Development.IDE.Types.Shake
import qualified Focus
import           GHC.Fingerprint
import           GHC.Stack                              (HasCallStack)
import           HieDb.Types
import           Ide.Logger                             hiding (Priority)
import qualified Ide.Logger                             as Logger
import           Ide.Plugin.Config
import qualified Ide.PluginUtils                        as HLS
import           Ide.Types                              (IdePlugins (IdePlugins),
                                                         PluginDescriptor (pluginId),
                                                         PluginId)
import           Language.LSP.Diagnostics
import qualified Language.LSP.Protocol.Lens             as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types            as LSP
import qualified Language.LSP.Server                    as LSP
import           Language.LSP.VFS                       hiding (start)
import qualified "list-t" ListT
import           OpenTelemetry.Eventlog                 hiding (addEvent)
import qualified Prettyprinter                          as Pretty
import qualified StmContainers.Map                      as STM
import           System.FilePath                        hiding (makeRelative)
import           System.IO.Unsafe                       (unsafePerformIO)
import           System.Time.Extra

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,3,0)
import           Data.IORef
import           Development.IDE.GHC.Compat             (NameCacheUpdater (NCU),
                                                         mkSplitUniqSupply,
                                                         upNameCache)
#endif

#if MIN_VERSION_ghc(9,3,0)
import           Development.IDE.GHC.Compat             (NameCacheUpdater)
#endif

data Log
  = LogCreateHieDbExportsMapStart
  | LogCreateHieDbExportsMapFinish !Int
  | LogBuildSessionRestart !String ![DelayedActionInternal] !KeySet !Seconds !(Maybe FilePath)
  | LogBuildSessionRestartTakingTooLong !Seconds
  | LogDelayedAction !(DelayedAction ()) !Seconds
  | LogBuildSessionFinish !(Maybe SomeException)
  | LogDiagsDiffButNoLspEnv ![FileDiagnostic]
  | LogDefineEarlyCutoffRuleNoDiagHasDiag !FileDiagnostic
  | LogDefineEarlyCutoffRuleCustomNewnessHasDiag !FileDiagnostic
  | LogCancelledAction !T.Text
  | LogSessionInitialised
  | LogLookupPersistentKey !T.Text
  | LogShakeGarbageCollection !T.Text !Int !Seconds
  -- * OfInterest Log messages
  | LogSetFilesOfInterest ![(NormalizedFilePath, FileOfInterestStatus)]
  deriving Show

instance Pretty Log where
  pretty = \case
    LogCreateHieDbExportsMapStart ->
      "Initializing exports map from hiedb"
    LogCreateHieDbExportsMapFinish exportsMapSize ->
      "Done initializing exports map from hiedb. Size:" <+> pretty exportsMapSize
    LogBuildSessionRestart reason actionQueue keyBackLog abortDuration shakeProfilePath ->
      vcat
        [ "Restarting build session due to" <+> pretty reason
        , "Action Queue:" <+> pretty (map actionName actionQueue)
        , "Keys:" <+> pretty (map show $ toListKeySet keyBackLog)
        , "Aborting previous build session took" <+> pretty (showDuration abortDuration) <+> pretty shakeProfilePath ]
    LogBuildSessionRestartTakingTooLong seconds ->
        "Build restart is taking too long (" <> pretty seconds <> " seconds)"
    LogDelayedAction delayedAct seconds ->
      hsep
        [ "Finished:" <+> pretty (actionName delayedAct)
        , "Took:" <+> pretty (showDuration seconds) ]
    LogBuildSessionFinish e ->
      vcat
        [ "Finished build session"
        , pretty (fmap displayException e) ]
    LogDiagsDiffButNoLspEnv fileDiagnostics ->
      "updateFileDiagnostics published different from new diagnostics - file diagnostics:"
      <+> pretty (showDiagnosticsColored fileDiagnostics)
    LogDefineEarlyCutoffRuleNoDiagHasDiag fileDiagnostic ->
      "defineEarlyCutoff RuleNoDiagnostics - file diagnostic:"
      <+> pretty (showDiagnosticsColored [fileDiagnostic])
    LogDefineEarlyCutoffRuleCustomNewnessHasDiag fileDiagnostic ->
      "defineEarlyCutoff RuleWithCustomNewnessCheck - file diagnostic:"
      <+> pretty (showDiagnosticsColored [fileDiagnostic])
    LogCancelledAction action ->
        pretty action <+> "was cancelled"
    LogSessionInitialised -> "Shake session initialized"
    LogLookupPersistentKey key ->
        "LOOKUP PERSISTENT FOR:" <+> pretty key
    LogShakeGarbageCollection label number duration ->
        pretty label <+> "of" <+> pretty number <+> "keys (took " <+> pretty (showDuration duration) <> ")"
    LogSetFilesOfInterest ofInterest ->
        "Set files of interst to" <> Pretty.line
            <> indent 4 (pretty $ fmap (first fromNormalizedFilePath) ofInterest)

-- | We need to serialize writes to the database, so we send any function that
-- needs to write to the database over the channel, where it will be picked up by
-- a worker thread.
data HieDbWriter
  = HieDbWriter
  { indexQueue         :: IndexQueue
  , indexPending       :: TVar (HMap.HashMap NormalizedFilePath Fingerprint) -- ^ Avoid unnecessary/out of date indexing
  , indexCompleted     :: TVar Int -- ^ to report progress
  , indexProgressToken :: Var (Maybe LSP.ProgressToken)
  -- ^ This is a Var instead of a TVar since we need to do IO to initialise/update, so we need a lock
  }

-- | Actions to queue up on the index worker thread
-- The inner `(HieDb -> IO ()) -> IO ()` wraps `HieDb -> IO ()`
-- with (currently) retry functionality
type IndexQueue = TQueue (((HieDb -> IO ()) -> IO ()) -> IO ())

-- Note [Semantic Tokens Cache Location]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- storing semantic tokens cache for each file in shakeExtras might
-- not be ideal, since it most used in LSP request handlers
-- instead of rules. We should consider moving it to a more
-- appropriate place in the future if we find one, store it for now.

-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    { --eventer :: LSP.FromServerMessage -> IO ()
     lspEnv :: Maybe (LSP.LanguageContextEnv Config)
    ,debouncer :: Debouncer NormalizedUri
    ,shakeRecorder :: Recorder (WithPriority Log)
    ,idePlugins :: IdePlugins IdeState
    ,globals :: TVar (HMap.HashMap TypeRep Dynamic)
      -- ^ Registry of global state used by rules.
      -- Small and immutable after startup, so not worth using an STM.Map.
    ,state :: Values
    ,diagnostics :: STMDiagnosticStore
    ,hiddenDiagnostics :: STMDiagnosticStore
    ,publishedDiagnostics :: STM.Map NormalizedUri [Diagnostic]
    -- ^ This represents the set of diagnostics that we have published.
    -- Due to debouncing not every change might get published.

    ,semanticTokensCache:: STM.Map NormalizedFilePath SemanticTokens
    -- ^ Cache of last response of semantic tokens for each file,
    -- so we can compute deltas for semantic tokens(SMethod_TextDocumentSemanticTokensFullDelta).
    -- putting semantic tokens cache and id in shakeExtras might not be ideal
    -- see Note [Semantic Tokens Cache Location]
    ,semanticTokensId :: TVar Int
    -- ^ semanticTokensId is used to generate unique ids for each lsp response of semantic tokens.
    ,positionMapping :: STM.Map NormalizedUri (EnumMap Int32 (PositionDelta, PositionMapping))
    -- ^ Map from a text document version to a PositionMapping that describes how to map
    -- positions in a version of that document to positions in the latest version
    -- First mapping is delta from previous version and second one is an
    -- accumulation to the current version.
    ,progress :: ProgressReporting
    ,ideTesting :: IdeTesting
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
    ,restartShakeSession
        :: VFSModified
        -> String
        -> [DelayedAction ()]
        -> IO [Key]
        -> IO ()
#if MIN_VERSION_ghc(9,3,0)
    ,ideNc :: NameCache
#else
    ,ideNc :: IORef NameCache
#endif
    -- | A mapping of module name to known target (or candidate targets, if missing)
    ,knownTargetsVar :: TVar (Hashed KnownTargets)
    -- | A mapping of exported identifiers for local modules. Updated on kick
    ,exportsMap :: TVar ExportsMap
    -- | A work queue for actions added via 'runInShakeSession'
    ,actionQueue :: ActionQueue
    ,clientCapabilities :: ClientCapabilities
    , withHieDb :: WithHieDb -- ^ Use only to read.
    , hiedbWriter :: HieDbWriter -- ^ use to write
    , persistentKeys :: TVar (KeyMap GetStalePersistent)
      -- ^ Registry for functions that compute/get "stale" results for the rule
      -- (possibly from disk)
    , vfsVar :: TVar VFS
    -- ^ A snapshot of the current state of the virtual file system. Updated on shakeRestart
    -- VFS state is managed by LSP. However, the state according to the lsp library may be newer than the state of the current session,
    -- leaving us vulnerable to subtle race conditions. To avoid this, we take a snapshot of the state of the VFS on every
    -- restart, so that the whole session sees a single consistent view of the VFS.
    -- We don't need a STM.Map because we never update individual keys ourselves.
    , defaultConfig :: Config
      -- ^ Default HLS config, only relevant if the client does not provide any Config
    , dirtyKeys :: TVar KeySet
      -- ^ Set of dirty rule keys since the last Shake run
    }

type WithProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> ((LSP.ProgressAmount -> IO ()) -> IO a) -> IO a
type WithIndefiniteProgressFunc = forall a.
    T.Text -> LSP.ProgressCancellable -> IO a -> IO a

type GetStalePersistent = NormalizedFilePath -> IdeAction (Maybe (Dynamic,PositionDelta,Maybe Int32))

getShakeExtras :: Action ShakeExtras
getShakeExtras = do
    -- Will fail the action with a pattern match failure, but be caught
    Just x <- getShakeExtra @ShakeExtras
    return x

getShakeExtrasRules :: Rules ShakeExtras
getShakeExtrasRules = do
    mExtras <- getShakeExtraRules @ShakeExtras
    case mExtras of
      Just x  -> return x
      -- This will actually crash HLS
      Nothing -> liftIO $ fail "missing ShakeExtras"

-- See Note [Client configuration in Rules]
-- | Returns the client configuration, creating a build dependency.
--   You should always use this function when accessing client configuration
--   from build rules.
getClientConfigAction :: Action Config
getClientConfigAction = do
  ShakeExtras{lspEnv, idePlugins} <- getShakeExtras
  currentConfig <- (`LSP.runLspT` LSP.getConfig) `traverse` lspEnv
  mbVal <- unhashed <$> useNoFile_ GetClientSettings
  let defValue = fromMaybe def currentConfig
  case A.parse (parseConfig idePlugins defValue) <$> mbVal of
    Just (Success c) -> return c
    _                -> return defValue

getPluginConfigAction :: PluginId -> Action PluginConfig
getPluginConfigAction plId = do
    config <- getClientConfigAction
    ShakeExtras{idePlugins = IdePlugins plugins} <- getShakeExtras
    let plugin = fromMaybe (error $ "Plugin not found: " <> show plId) $
                    find (\p -> pluginId p == plId) plugins
    return $ HLS.configForPlugin config plugin

-- | Register a function that will be called to get the "stale" result of a rule, possibly from disk
-- This is called when we don't already have a result, or computing the rule failed.
-- The result of this function will always be marked as 'stale', and a 'proper' rebuild of the rule will
-- be queued if the rule hasn't run before.
addPersistentRule :: IdeRule k v => k -> (NormalizedFilePath -> IdeAction (Maybe (v,PositionDelta,Maybe Int32))) -> Rules ()
addPersistentRule k getVal = do
  ShakeExtras{persistentKeys} <- getShakeExtrasRules
  void $ liftIO $ atomically $ modifyTVar' persistentKeys $ insertKeyMap (newKey k) (fmap (fmap (first3 toDyn)) . getVal)

class Typeable a => IsIdeGlobal a where

-- | Read a virtual file from the current snapshot
getVirtualFile :: NormalizedFilePath -> Action (Maybe VirtualFile)
getVirtualFile nf = do
  vfs <- fmap _vfsMap . liftIO . readTVarIO . vfsVar =<< getShakeExtras
  pure $! Map.lookup (filePathToUri' nf) vfs -- Don't leak a reference to the entire map

-- Take a snapshot of the current LSP VFS
vfsSnapshot :: Maybe (LSP.LanguageContextEnv a) -> IO VFS
vfsSnapshot Nothing       = pure $ VFS mempty
vfsSnapshot (Just lspEnv) = LSP.runLspT lspEnv LSP.getVirtualFiles


addIdeGlobal :: IsIdeGlobal a => a -> Rules ()
addIdeGlobal x = do
    extras <- getShakeExtrasRules
    liftIO $ addIdeGlobalExtras extras x

addIdeGlobalExtras :: IsIdeGlobal a => ShakeExtras -> a -> IO ()
addIdeGlobalExtras ShakeExtras{globals} x@(typeOf -> ty) =
    void $ liftIO $ atomically $ modifyTVar' globals $ \mp -> case HMap.lookup ty mp of
        Just _ -> error $ "Internal error, addIdeGlobalExtras, got the same type twice for " ++ show ty
        Nothing -> HMap.insert ty (toDyn x) mp

getIdeGlobalExtras :: forall a . (HasCallStack, IsIdeGlobal a) => ShakeExtras -> IO a
getIdeGlobalExtras ShakeExtras{globals} = do
    let typ = typeRep (Proxy :: Proxy a)
    x <- HMap.lookup (typeRep (Proxy :: Proxy a)) <$> readTVarIO globals
    case x of
        Just y
            | Just z <- fromDynamic y -> pure z
            | otherwise -> errorIO $ "Internal error, getIdeGlobalExtras, wrong type for " ++ show typ ++ " (got " ++ show (dynTypeRep y) ++ ")"
        Nothing -> errorIO $ "Internal error, getIdeGlobalExtras, no entry for " ++ show typ

getIdeGlobalAction :: forall a . (HasCallStack, IsIdeGlobal a) => Action a
getIdeGlobalAction = liftIO . getIdeGlobalExtras =<< getShakeExtras

getIdeGlobalState :: forall a . IsIdeGlobal a => IdeState -> IO a
getIdeGlobalState = getIdeGlobalExtras . shakeExtras

newtype GlobalIdeOptions = GlobalIdeOptions IdeOptions
instance IsIdeGlobal GlobalIdeOptions

getIdeOptions :: Action IdeOptions
getIdeOptions = do
    GlobalIdeOptions x <- getIdeGlobalAction
    mbEnv <- lspEnv <$> getShakeExtras
    case mbEnv of
        Nothing -> return x
        Just env -> do
            config <- liftIO $ LSP.runLspT env HLS.getClientConfig
            return x{optCheckProject = pure $ checkProject config,
                     optCheckParents = pure $ checkParents config
                }

getIdeOptionsIO :: ShakeExtras -> IO IdeOptions
getIdeOptionsIO ide = do
    GlobalIdeOptions x <- getIdeGlobalExtras ide
    return x

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValueIO :: IdeRule k v => ShakeExtras -> k -> NormalizedFilePath -> IO (Maybe (v, PositionMapping))
lastValueIO s@ShakeExtras{positionMapping,persistentKeys,state} k file = do

    let readPersistent
          | IdeTesting testing <- ideTesting s -- Don't read stale persistent values in tests
          , testing = pure Nothing
          | otherwise = do
          pmap <- readTVarIO persistentKeys
          mv <- runMaybeT $ do
            liftIO $ logWith (shakeRecorder s) Debug $ LogLookupPersistentKey (T.pack $ show k)
            f <- MaybeT $ pure $ lookupKeyMap (newKey k) pmap
            (dv,del,ver) <- MaybeT $ runIdeAction "lastValueIO" s $ f file
            MaybeT $ pure $ (,del,ver) <$> fromDynamic dv
          case mv of
            Nothing -> atomicallyNamed "lastValueIO 1" $ do
                STM.focus (Focus.alter (alterValue $ Failed True)) (toKey k file) state
                return Nothing
            Just (v,del,mbVer) -> do
                actual_version <- case mbVer of
                  Just ver -> pure (Just $ VFSVersion ver)
                  Nothing -> (Just . ModificationTime <$> getModTime (fromNormalizedFilePath file))
                              `catch` (\(_ :: IOException) -> pure Nothing)
                atomicallyNamed "lastValueIO 2" $ do
                  STM.focus (Focus.alter (alterValue $ Stale (Just del) actual_version (toDyn v))) (toKey k file) state
                  Just . (v,) . addOldDelta del <$> mappingForVersion positionMapping file actual_version

        -- We got a new stale value from the persistent rule, insert it in the map without affecting diagnostics
        alterValue new Nothing = Just (ValueWithDiagnostics new mempty) -- If it wasn't in the map, give it empty diagnostics
        alterValue new (Just old@(ValueWithDiagnostics val diags)) = Just $ case val of
          -- Old failed, we can update it preserving diagnostics
          Failed{} -> ValueWithDiagnostics new diags
          -- Something already succeeded before, leave it alone
          _        -> old

    atomicallyNamed "lastValueIO 4"  (STM.lookup (toKey k file) state) >>= \case
      Nothing -> readPersistent
      Just (ValueWithDiagnostics value _) -> case value of
        Succeeded ver (fromDynamic -> Just v) ->
            atomicallyNamed "lastValueIO 5"  $ Just . (v,) <$> mappingForVersion positionMapping file ver
        Stale del ver (fromDynamic -> Just v) ->
            atomicallyNamed "lastValueIO 6"  $ Just . (v,) . maybe id addOldDelta del <$> mappingForVersion positionMapping file ver
        Failed p | not p -> readPersistent
        _ -> pure Nothing

-- | Return the most recent, potentially stale, value and a PositionMapping
-- for the version of that value.
lastValue :: IdeRule k v => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
lastValue key file = do
    s <- getShakeExtras
    liftIO $ lastValueIO s key file

mappingForVersion
    :: STM.Map NormalizedUri (EnumMap Int32 (a, PositionMapping))
    -> NormalizedFilePath
    -> Maybe FileVersion
    -> STM PositionMapping
mappingForVersion allMappings file (Just (VFSVersion ver)) = do
    mapping <- STM.lookup (filePathToUri' file) allMappings
    return $ maybe zeroMapping snd $ EM.lookup ver =<< mapping
mappingForVersion _ _ _ = pure zeroMapping

type IdeRule k v =
  ( Shake.RuleResult k ~ v
  , Shake.ShakeValue k
  , Show v
  , Typeable v
  , NFData v
  )

-- | A live Shake session with the ability to enqueue Actions for running.
--   Keeps the 'ShakeDatabase' open, so at most one 'ShakeSession' per database.
newtype ShakeSession = ShakeSession
  { cancelShakeSession :: IO ()
    -- ^ Closes the Shake session
  }

-- | A Shake database plus persistent store. Can be thought of as storing
--   mappings from @(FilePath, k)@ to @RuleResult k@.
data IdeState = IdeState
    {shakeDb              :: ShakeDatabase
    ,shakeSession         :: MVar ShakeSession
    ,shakeExtras          :: ShakeExtras
    ,shakeDatabaseProfile :: ShakeDatabase -> IO (Maybe FilePath)
    ,stopMonitoring       :: IO ()
    ,rootDir              :: FilePath
    }



-- This is debugging code that generates a series of profiles, if the Boolean is true
shakeDatabaseProfileIO :: Maybe FilePath -> IO(ShakeDatabase -> IO (Maybe FilePath))
shakeDatabaseProfileIO mbProfileDir = do
    profileStartTime <- formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
    profileCounter <- newVar (0::Int)
    return $ \shakeDb ->
        for mbProfileDir $ \dir -> do
                count <- modifyVar profileCounter $ \x -> let !y = x+1 in return (y,y)
                let file = "ide-" ++ profileStartTime ++ "-" ++ takeEnd 5 ("0000" ++ show count) <.> "html"
                shakeProfileDatabase shakeDb $ dir </> file
                return (dir </> file)

setValues :: IdeRule k v
          => Values
          -> k
          -> NormalizedFilePath
          -> Value v
          -> Vector FileDiagnostic
          -> STM ()
setValues state key file val diags =
    STM.insert (ValueWithDiagnostics (fmap toDyn val) diags) (toKey key file) state


-- | Delete the value stored for a given ide build key
-- and return the key that was deleted.
deleteValue
  :: Shake.ShakeValue k
  => ShakeExtras
  -> k
  -> NormalizedFilePath
  -> STM [Key]
deleteValue ShakeExtras{state} key file = do
    STM.delete (toKey key file) state
    return [toKey key file]


-- | We return Nothing if the rule has not run and Just Failed if it has failed to produce a value.
getValues ::
  forall k v.
  IdeRule k v =>
  Values ->
  k ->
  NormalizedFilePath ->
  STM (Maybe (Value v, Vector FileDiagnostic))
getValues state key file = do
    STM.lookup (toKey key file) state >>= \case
        Nothing -> pure Nothing
        Just (ValueWithDiagnostics v diagsV) -> do
            let !r = seqValue $ fmap (fromJust . fromDynamic @v) v
                !res = (r,diagsV)
            -- Force to make sure we do not retain a reference to the HashMap
            -- and we blow up immediately if the fromJust should fail
            -- (which would be an internal error).
            return $ Just res

-- | Get all the files in the project
knownTargets :: Action (Hashed KnownTargets)
knownTargets = do
  ShakeExtras{knownTargetsVar} <- getShakeExtras
  liftIO $ readTVarIO knownTargetsVar

-- | Seq the result stored in the Shake value. This only
-- evaluates the value to WHNF not NF. We take care of the latter
-- elsewhere and doing it twice is expensive.
seqValue :: Value v -> Value v
seqValue val = case val of
    Succeeded ver v -> rnf ver `seq` v `seq` val
    Stale d ver v   -> rnf d `seq` rnf ver `seq` v `seq` val
    Failed _        -> val

-- | Open a 'IdeState', should be shut using 'shakeShut'.
shakeOpen :: Recorder (WithPriority Log)
          -> Maybe (LSP.LanguageContextEnv Config)
          -> Config
          -> IdePlugins IdeState
          -> Debouncer NormalizedUri
          -> Maybe FilePath
          -> IdeReportProgress
          -> IdeTesting
          -> WithHieDb
          -> IndexQueue
          -> ShakeOptions
          -> Monitoring
          -> Rules ()
          -> FilePath
          -> IO IdeState
shakeOpen recorder lspEnv defaultConfig idePlugins debouncer
  shakeProfileDir (IdeReportProgress reportProgress)
  ideTesting@(IdeTesting testing)
  withHieDb indexQueue opts monitoring rules rootDir = mdo

#if MIN_VERSION_ghc(9,3,0)
    ideNc <- initNameCache 'r' knownKeyNames
#else
    us <- mkSplitUniqSupply 'r'
    ideNc <- newIORef (initNameCache us knownKeyNames)
#endif
    shakeExtras <- do
        globals <- newTVarIO HMap.empty
        state <- STM.newIO
        diagnostics <- STM.newIO
        hiddenDiagnostics <- STM.newIO
        publishedDiagnostics <- STM.newIO
        semanticTokensCache <- STM.newIO
        positionMapping <- STM.newIO
        knownTargetsVar <- newTVarIO $ hashed HMap.empty
        let restartShakeSession = shakeRestart recorder ideState
        persistentKeys <- newTVarIO mempty
        indexPending <- newTVarIO HMap.empty
        indexCompleted <- newTVarIO 0
        semanticTokensId <- newTVarIO 0
        indexProgressToken <- newVar Nothing
        let hiedbWriter = HieDbWriter{..}
        exportsMap <- newTVarIO mempty
        -- lazily initialize the exports map with the contents of the hiedb
        -- TODO: exceptions can be swallowed here?
        _ <- async $ do
            logWith recorder Debug LogCreateHieDbExportsMapStart
            em <- createExportsMapHieDb withHieDb
            atomically $ modifyTVar' exportsMap (<> em)
            logWith recorder Debug $ LogCreateHieDbExportsMapFinish (ExportsMap.size em)

        progress <- do
            let (before, after) = if testing then (0,0.1) else (0.1,0.1)
            if reportProgress
                then delayedProgressReporting before after lspEnv optProgressStyle
                else noProgressReporting
        actionQueue <- newQueue

        let clientCapabilities = maybe def LSP.resClientCapabilities lspEnv
        dirtyKeys <- newTVarIO mempty
        -- Take one VFS snapshot at the start
        vfsVar <- newTVarIO =<< vfsSnapshot lspEnv
        pure ShakeExtras{shakeRecorder = recorder, ..}
    shakeDb  <-
        shakeNewDatabase
            opts { shakeExtra = newShakeExtra shakeExtras }
            rules
    shakeSession <- newEmptyMVar
    shakeDatabaseProfile <- shakeDatabaseProfileIO shakeProfileDir

    IdeOptions
        { optProgressStyle
        , optCheckParents
        } <- getIdeOptionsIO shakeExtras

    checkParents <- optCheckParents

    -- monitoring
    let readValuesCounter = fromIntegral . countRelevantKeys checkParents <$> getStateKeys shakeExtras
        readDirtyKeys = fromIntegral . countRelevantKeys checkParents . toListKeySet <$> readTVarIO(dirtyKeys shakeExtras)
        readIndexPending = fromIntegral . HMap.size <$> readTVarIO (indexPending $ hiedbWriter shakeExtras)
        readExportsMap = fromIntegral . ExportsMap.exportsMapSize <$> readTVarIO (exportsMap shakeExtras)
        readDatabaseCount = fromIntegral . countRelevantKeys checkParents . map fst <$> shakeGetDatabaseKeys shakeDb
        readDatabaseStep =  fromIntegral <$> shakeGetBuildStep shakeDb

    registerGauge monitoring "ghcide.values_count" readValuesCounter
    registerGauge monitoring "ghcide.dirty_keys_count" readDirtyKeys
    registerGauge monitoring "ghcide.indexing_pending_count" readIndexPending
    registerGauge monitoring "ghcide.exports_map_count" readExportsMap
    registerGauge monitoring "ghcide.database_count" readDatabaseCount
    registerCounter monitoring "ghcide.num_builds" readDatabaseStep

    stopMonitoring <- start monitoring

    let ideState = IdeState{..}
    return ideState


getStateKeys :: ShakeExtras -> IO [Key]
getStateKeys = (fmap.fmap) fst . atomically . ListT.toList . STM.listT . state

-- | Must be called in the 'Initialized' handler and only once
shakeSessionInit :: Recorder (WithPriority Log) -> IdeState -> IO ()
shakeSessionInit recorder IdeState{..} = do
    -- Take a snapshot of the VFS - it should be empty as we've received no notifications
    -- till now, but it can't hurt to be in sync with the `lsp` library.
    vfs <- vfsSnapshot (lspEnv shakeExtras)
    initSession <- newSession recorder shakeExtras (VFSModified vfs) shakeDb [] "shakeSessionInit"
    putMVar shakeSession initSession
    logWith recorder Debug LogSessionInitialised

shakeShut :: IdeState -> IO ()
shakeShut IdeState{..} = do
    runner <- tryReadMVar shakeSession
    -- Shake gets unhappy if you try to close when there is a running
    -- request so we first abort that.
    for_ runner cancelShakeSession
    void $ shakeDatabaseProfile shakeDb
    progressStop $ progress shakeExtras
    stopMonitoring


-- | This is a variant of withMVar where the first argument is run unmasked and if it throws
-- an exception, the previous value is restored while the second argument is executed masked.
withMVar' :: MVar a -> (a -> IO b) -> (b -> IO (a, c)) -> IO c
withMVar' var unmasked masked = uninterruptibleMask $ \restore -> do
    a <- takeMVar var
    b <- restore (unmasked a) `onException` putMVar var a
    (a', c) <- masked b
    putMVar var a'
    pure c


mkDelayedAction :: String -> Logger.Priority -> Action a -> DelayedAction a
mkDelayedAction = DelayedAction Nothing

-- | These actions are run asynchronously after the current action is
-- finished running. For example, to trigger a key build after a rule
-- has already finished as is the case with useWithStaleFast
delayedAction :: DelayedAction a -> IdeAction (IO a)
delayedAction a = do
  extras <- ask
  liftIO $ shakeEnqueue extras a

-- | Restart the current 'ShakeSession' with the given system actions.
--   Any actions running in the current session will be aborted,
--   but actions added via 'shakeEnqueue' will be requeued.
shakeRestart :: Recorder (WithPriority Log) -> IdeState -> VFSModified -> String -> [DelayedAction ()] -> IO [Key] -> IO ()
shakeRestart recorder IdeState{..} vfs reason acts ioActionBetweenShakeSession =
    withMVar'
        shakeSession
        (\runner -> do
              (stopTime,()) <- duration $ logErrorAfter 10 $ cancelShakeSession runner
              keys <- ioActionBetweenShakeSession
              -- it is every important to update the dirty keys after we enter the critical section
              -- see Note [Housekeeping rule cache and dirty key outside of hls-graph]
              atomically $ modifyTVar' (dirtyKeys shakeExtras) $ \x -> foldl' (flip insertKeySet) x keys
              res <- shakeDatabaseProfile shakeDb
              backlog <- readTVarIO $ dirtyKeys shakeExtras
              queue <- atomicallyNamed "actionQueue - peek" $ peekInProgress $ actionQueue shakeExtras

              -- this log is required by tests
              logWith recorder Debug $ LogBuildSessionRestart reason queue backlog stopTime res
        )
        -- It is crucial to be masked here, otherwise we can get killed
        -- between spawning the new thread and updating shakeSession.
        -- See https://github.com/haskell/ghcide/issues/79
        (\() -> do
          (,()) <$> newSession recorder shakeExtras vfs shakeDb acts reason)
    where
        logErrorAfter :: Seconds -> IO () -> IO ()
        logErrorAfter seconds action = flip withAsync (const action) $ do
            sleep seconds
            logWith recorder Error (LogBuildSessionRestartTakingTooLong seconds)

-- | Enqueue an action in the existing 'ShakeSession'.
--   Returns a computation to block until the action is run, propagating exceptions.
--   Assumes a 'ShakeSession' is available.
--
--   Appropriate for user actions other than edits.
shakeEnqueue :: ShakeExtras -> DelayedAction a -> IO (IO a)
shakeEnqueue ShakeExtras{actionQueue, shakeRecorder} act = do
    (b, dai) <- instantiateDelayedAction act
    atomicallyNamed "actionQueue - push" $ pushQueue dai actionQueue
    let wait' barrier =
            waitBarrier barrier `catches`
              [ Handler(\BlockedIndefinitelyOnMVar ->
                    fail $ "internal bug: forever blocked on MVar for " <>
                            actionName act)
              , Handler (\e@AsyncCancelled -> do
                  logWith shakeRecorder Debug $ LogCancelledAction (T.pack $ actionName act)

                  atomicallyNamed "actionQueue - abort" $ abortQueue dai actionQueue
                  throw e)
              ]
    return (wait' b >>= either throwIO return)

data VFSModified = VFSUnmodified | VFSModified !VFS

-- | Set up a new 'ShakeSession' with a set of initial actions
--   Will crash if there is an existing 'ShakeSession' running.
newSession
    :: Recorder (WithPriority Log)
    -> ShakeExtras
    -> VFSModified
    -> ShakeDatabase
    -> [DelayedActionInternal]
    -> String
    -> IO ShakeSession
newSession recorder extras@ShakeExtras{..} vfsMod shakeDb acts reason = do

    -- Take a new VFS snapshot
    case vfsMod of
      VFSUnmodified   -> pure ()
      VFSModified vfs -> atomically $ writeTVar vfsVar vfs

    IdeOptions{optRunSubset} <- getIdeOptionsIO extras
    reenqueued <- atomicallyNamed "actionQueue - peek" $ peekInProgress actionQueue
    allPendingKeys <-
        if optRunSubset
          then Just <$> readTVarIO dirtyKeys
          else return Nothing
    let
        -- A daemon-like action used to inject additional work
        -- Runs actions from the work queue sequentially
        pumpActionThread otSpan = do
            d <- liftIO $ atomicallyNamed "action queue - pop" $ popQueue actionQueue
            actionFork (run otSpan d) $ \_ -> pumpActionThread otSpan

        -- TODO figure out how to thread the otSpan into defineEarlyCutoff
        run _otSpan d  = do
            start <- liftIO offsetTime
            getAction d
            liftIO $ atomicallyNamed "actionQueue - done" $ doneQueue d actionQueue
            runTime <- liftIO start
            logWith recorder (actionPriority d) $ LogDelayedAction d runTime

        -- The inferred type signature doesn't work in ghc >= 9.0.1
        workRun :: (forall b. IO b -> IO b) -> IO (IO ())
        workRun restore = withSpan "Shake session" $ \otSpan -> do
          setTag otSpan "reason" (fromString reason)
          setTag otSpan "queue" (fromString $ unlines $ map actionName reenqueued)
          whenJust allPendingKeys $ \kk -> setTag otSpan "keys" (BS8.pack $ unlines $ map show $ toListKeySet kk)
          let keysActs = pumpActionThread otSpan : map (run otSpan) (reenqueued ++ acts)
          res <- try @SomeException $
            restore $ shakeRunDatabaseForKeys (toListKeySet <$> allPendingKeys) shakeDb keysActs
          return $ do
              let exception =
                    case res of
                      Left e -> Just e
                      _      -> Nothing
              logWith recorder Debug $ LogBuildSessionFinish exception

    -- Do the work in a background thread
    workThread <- asyncWithUnmask workRun

    -- run the wrap up in a separate thread since it contains interruptible
    -- commands (and we are not using uninterruptible mask)
    -- TODO: can possibly swallow exceptions?
    _ <- async $ join $ wait workThread

    --  Cancelling is required to flush the Shake database when either
    --  the filesystem or the Ghc configuration have changed
    let cancelShakeSession :: IO ()
        cancelShakeSession = cancel workThread

    pure (ShakeSession{..})

instantiateDelayedAction
    :: DelayedAction a
    -> IO (Barrier (Either SomeException a), DelayedActionInternal)
instantiateDelayedAction (DelayedAction _ s p a) = do
  u <- newUnique
  b <- newBarrier
  let a' = do
        -- work gets reenqueued when the Shake session is restarted
        -- it can happen that a work item finished just as it was reenqueued
        -- in that case, skipping the work is fine
        alreadyDone <- liftIO $ isJust <$> waitBarrierMaybe b
        unless alreadyDone $ do
          x <- actionCatch @SomeException (Right <$> a) (pure . Left)
          -- ignore exceptions if the barrier has been filled concurrently
          liftIO $ void $ try @SomeException $ signalBarrier b x
      d' = DelayedAction (Just u) s p a'
  return (b, d')

getDiagnostics :: IdeState -> STM [FileDiagnostic]
getDiagnostics IdeState{shakeExtras = ShakeExtras{diagnostics}} = do
    getAllDiagnostics diagnostics

getHiddenDiagnostics :: IdeState -> STM [FileDiagnostic]
getHiddenDiagnostics IdeState{shakeExtras = ShakeExtras{hiddenDiagnostics}} = do
    getAllDiagnostics hiddenDiagnostics

-- | Find and release old keys from the state Hashmap
--   For the record, there are other state sources that this process does not release:
--     * diagnostics store (normal, hidden and published)
--     * position mapping store
--     * indexing queue
--     * exports map
garbageCollectDirtyKeys :: Action [Key]
garbageCollectDirtyKeys = do
    IdeOptions{optCheckParents} <- getIdeOptions
    checkParents <- liftIO optCheckParents
    garbageCollectDirtyKeysOlderThan 0 checkParents

garbageCollectDirtyKeysOlderThan :: Int -> CheckParents -> Action [Key]
garbageCollectDirtyKeysOlderThan maxAge checkParents = otTracedGarbageCollection "dirty GC" $ do
    dirtySet <- getDirtySet
    garbageCollectKeys "dirty GC" maxAge checkParents dirtySet

garbageCollectKeys :: String -> Int -> CheckParents -> [(Key, Int)] -> Action [Key]
garbageCollectKeys label maxAge checkParents agedKeys = do
    start <- liftIO offsetTime
    ShakeExtras{state, dirtyKeys, lspEnv, shakeRecorder, ideTesting} <- getShakeExtras
    (n::Int, garbage) <- liftIO $
        foldM (removeDirtyKey dirtyKeys state) (0,[]) agedKeys
    t <- liftIO start
    when (n>0) $ liftIO $ do
        logWith shakeRecorder Debug $ LogShakeGarbageCollection (T.pack label) n t
    when (coerce ideTesting) $ liftIO $ mRunLspT lspEnv $
        LSP.sendNotification (SMethod_CustomMethod (Proxy @"ghcide/GC"))
                             (toJSON $ mapMaybe (fmap showKey . fromKeyType) garbage)
    return garbage

    where
        showKey = show . Q
        removeDirtyKey dk values st@(!counter, keys) (k, age)
            | age > maxAge
            , Just (kt,_) <- fromKeyType k
            , not(kt `HSet.member` preservedKeys checkParents)
            = atomicallyNamed "GC" $ do
                gotIt <- STM.focus (Focus.member <* Focus.delete) k values
                when gotIt $
                   modifyTVar' dk (insertKeySet k)
                return $ if gotIt then (counter+1, k:keys) else st
            | otherwise = pure st

countRelevantKeys :: CheckParents -> [Key] -> Int
countRelevantKeys checkParents =
    Prelude.length . filter (maybe False (not . (`HSet.member` preservedKeys checkParents) . fst) . fromKeyType)

preservedKeys :: CheckParents -> HashSet TypeRep
preservedKeys checkParents = HSet.fromList $
    -- always preserved
    [ typeOf GetFileExists
    , typeOf GetModificationTime
    , typeOf IsFileOfInterest
    , typeOf GhcSessionIO
    , typeOf GetClientSettings
    , typeOf AddWatchedFile
    , typeOf GetKnownTargets
    ]
    ++ concat
    -- preserved if CheckParents is enabled since we need to rebuild the ModuleGraph
    [ [ typeOf GetModSummary
       , typeOf GetModSummaryWithoutTimestamps
       , typeOf GetLocatedImports
       ]
    | checkParents /= NeverCheck
    ]

-- | Define a new Rule without early cutoff
define
    :: IdeRule k v
    => Recorder (WithPriority Log) -> (k -> NormalizedFilePath -> Action (IdeResult v)) -> Rules ()
define recorder op = defineEarlyCutoff recorder $ Rule $ \k v -> (Nothing,) <$> op k v

defineNoDiagnostics
    :: IdeRule k v
    => Recorder (WithPriority Log) -> (k -> NormalizedFilePath -> Action (Maybe v)) -> Rules ()
defineNoDiagnostics recorder op = defineEarlyCutoff recorder $ RuleNoDiagnostics $ \k v -> (Nothing,) <$> op k v

-- | Request a Rule result if available
use :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe v)
use key file = runIdentity <$> uses key (Identity file)

-- | Request a Rule result, it not available return the last computed result, if any, which may be stale
useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe (v, PositionMapping))
useWithStale key file = runIdentity <$> usesWithStale key (Identity file)

-- |Request a Rule result, it not available return the last computed result
--  which may be stale.
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `useWithStaleE` instead.
useWithStale_ :: IdeRule k v
    => k -> NormalizedFilePath -> Action (v, PositionMapping)
useWithStale_ key file = runIdentity <$> usesWithStale_ key (Identity file)

-- |Plural version of 'useWithStale_'
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers.
usesWithStale_ :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> Action (f (v, PositionMapping))
usesWithStale_ key files = do
    res <- usesWithStale key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v  -> return v

-- | IdeActions are used when we want to return a result immediately, even if it
-- is stale Useful for UI actions like hover, completion where we don't want to
-- block.
--
-- Run via 'runIdeAction'.
newtype IdeAction a = IdeAction { runIdeActionT  :: (ReaderT ShakeExtras IO) a }
    deriving newtype (MonadReader ShakeExtras, MonadIO, Functor, Applicative, Monad, Semigroup)

runIdeAction :: String -> ShakeExtras -> IdeAction a -> IO a
runIdeAction _herald s i = runReaderT (runIdeActionT i) s

askShake :: IdeAction ShakeExtras
askShake = ask


#if MIN_VERSION_ghc(9,3,0)
mkUpdater :: NameCache -> NameCacheUpdater
mkUpdater = id
#else
mkUpdater :: IORef NameCache -> NameCacheUpdater
mkUpdater ref = NCU (upNameCache ref)
#endif

-- | A (maybe) stale result now, and an up to date one later
data FastResult a = FastResult { stale :: Maybe (a,PositionMapping), uptoDate :: IO (Maybe a)  }

-- | Lookup value in the database and return with the stale value immediately
-- Will queue an action to refresh the value.
-- Might block the first time the rule runs, but never blocks after that.
useWithStaleFast :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (Maybe (v, PositionMapping))
useWithStaleFast key file = stale <$> useWithStaleFast' key file

-- | Same as useWithStaleFast but lets you wait for an up to date result
useWithStaleFast' :: IdeRule k v => k -> NormalizedFilePath -> IdeAction (FastResult v)
useWithStaleFast' key file = do
  -- This lookup directly looks up the key in the shake database and
  -- returns the last value that was computed for this key without
  -- checking freshness.

  -- Async trigger the key to be built anyway because we want to
  -- keep updating the value in the key.
  waitValue <- delayedAction $ mkDelayedAction ("C:" ++ show key ++ ":" ++ fromNormalizedFilePath file) Debug $ use key file

  s@ShakeExtras{state} <- askShake
  r <- liftIO $ atomicallyNamed "useStateFast" $ getValues state key file
  liftIO $ case r of
    -- block for the result if we haven't computed before
    Nothing -> do
      -- Check if we can get a stale value from disk
      res <- lastValueIO s key file
      case res of
        Nothing -> do
          a <- waitValue
          pure $ FastResult ((,zeroMapping) <$> a) (pure a)
        Just _ -> pure $ FastResult res waitValue
    -- Otherwise, use the computed value even if it's out of date.
    Just _ -> do
      res <- lastValueIO s key file
      pure $ FastResult res waitValue

useNoFile :: IdeRule k v => k -> Action (Maybe v)
useNoFile key = use key emptyFilePath

-- Requests a rule if available.
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `useE` instead.
use_ :: IdeRule k v => k -> NormalizedFilePath -> Action v
use_ key file = runIdentity <$> uses_ key (Identity file)

useNoFile_ :: IdeRule k v => k -> Action v
useNoFile_ key = use_ key emptyFilePath

-- |Plural version of `use_`
--
-- Throws an `BadDependency` exception which is caught by the rule system if
-- none available.
--
-- WARNING: Not suitable for PluginHandlers. Use `usesE` instead.
uses_ :: (Traversable f, IdeRule k v) => k -> f NormalizedFilePath -> Action (f v)
uses_ key files = do
    res <- uses key files
    case sequence res of
        Nothing -> liftIO $ throwIO $ BadDependency (show key)
        Just v  -> return v

-- | Plural version of 'use'
uses :: (Traversable f, IdeRule k v)
    => k -> f NormalizedFilePath -> Action (f (Maybe v))
uses key files = fmap (\(A value) -> currentValue value) <$> apply (fmap (Q . (key,)) files)

-- | Return the last computed result which might be stale.
usesWithStale :: (Traversable f, IdeRule k v)
    => k -> f NormalizedFilePath -> Action (f (Maybe (v, PositionMapping)))
usesWithStale key files = do
    _ <- apply (fmap (Q . (key,)) files)
    -- We don't look at the result of the 'apply' since 'lastValue' will
    -- return the most recent successfully computed value regardless of
    -- whether the rule succeeded or not.
    traverse (lastValue key) files

useWithoutDependency :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe v)
useWithoutDependency key file =
    (\(Identity (A value)) -> currentValue value) <$> applyWithoutDependency (Identity (Q (key, file)))

data RuleBody k v
  = Rule (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, IdeResult v))
  | RuleNoDiagnostics (k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v))
  | RuleWithCustomNewnessCheck
    { newnessCheck :: BS.ByteString -> BS.ByteString -> Bool
    , build :: k -> NormalizedFilePath -> Action (Maybe BS.ByteString, Maybe v)
    }
  | RuleWithOldValue (k -> NormalizedFilePath -> Value v -> Action (Maybe BS.ByteString, IdeResult v))

-- | Define a new Rule with early cutoff
defineEarlyCutoff
    :: IdeRule k v
    => Recorder (WithPriority Log)
    -> RuleBody k v
    -> Rules ()
defineEarlyCutoff recorder (Rule op) = addRule $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> otTracedAction key file mode traceA $ \traceDiagnostics -> do
    extras <- getShakeExtras
    let diagnostics ver diags = do
            traceDiagnostics diags
            updateFileDiagnostics recorder file ver (newKey key) extras . map (\(_,y,z) -> (y,z)) $ diags
    defineEarlyCutoff' diagnostics (==) key file old mode $ const $ op key file
defineEarlyCutoff recorder (RuleNoDiagnostics op) = addRule $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> otTracedAction key file mode traceA $ \traceDiagnostics -> do
    let diagnostics _ver diags = do
            traceDiagnostics diags
            mapM_ (logWith recorder Warning . LogDefineEarlyCutoffRuleNoDiagHasDiag) diags
    defineEarlyCutoff' diagnostics (==) key file old mode $ const $ second (mempty,) <$> op key file
defineEarlyCutoff recorder RuleWithCustomNewnessCheck{..} =
    addRule $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode ->
        otTracedAction key file mode traceA $ \ traceDiagnostics -> do
            let diagnostics _ver diags = do
                    traceDiagnostics diags
                    mapM_ (logWith recorder Warning . LogDefineEarlyCutoffRuleCustomNewnessHasDiag) diags
            defineEarlyCutoff' diagnostics newnessCheck key file old mode $
                const $ second (mempty,) <$> build key file
defineEarlyCutoff recorder (RuleWithOldValue op) = addRule $ \(Q (key, file)) (old :: Maybe BS.ByteString) mode -> otTracedAction key file mode traceA $ \traceDiagnostics -> do
    extras <- getShakeExtras
    let diagnostics ver diags = do
            traceDiagnostics diags
            updateFileDiagnostics recorder file ver (newKey key) extras . map (\(_,y,z) -> (y,z)) $ diags
    defineEarlyCutoff' diagnostics (==) key file old mode $ op key file

defineNoFile :: IdeRule k v => Recorder (WithPriority Log) -> (k -> Action v) -> Rules ()
defineNoFile recorder f = defineNoDiagnostics recorder $ \k file -> do
    if file == emptyFilePath then do res <- f k; return (Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutOffNoFile :: IdeRule k v => Recorder (WithPriority Log) -> (k -> Action (BS.ByteString, v)) -> Rules ()
defineEarlyCutOffNoFile recorder f = defineEarlyCutoff recorder $ RuleNoDiagnostics $ \k file -> do
    if file == emptyFilePath then do (hashString, res) <- f k; return (Just hashString, Just res) else
        fail $ "Rule " ++ show k ++ " should always be called with the empty string for a file"

defineEarlyCutoff'
    :: forall k v. IdeRule k v
    => (Maybe Int32 -> [FileDiagnostic] -> Action ()) -- ^ update diagnostics
    -- | compare current and previous for freshness
    -> (BS.ByteString -> BS.ByteString -> Bool)
    -> k
    -> NormalizedFilePath
    -> Maybe BS.ByteString
    -> RunMode
    -> (Value v -> Action (Maybe BS.ByteString, IdeResult v))
    -> Action (RunResult (A (RuleResult k)))
defineEarlyCutoff' doDiagnostics cmp key file mbOld mode action = do
    ShakeExtras{state, progress, dirtyKeys} <- getShakeExtras
    options <- getIdeOptions
    (if optSkipProgress options key then id else inProgress progress file) $ do
        val <- case mbOld of
            Just old | mode == RunDependenciesSame -> do
                mbValue <- liftIO $ atomicallyNamed "define - read 1" $ getValues state key file
                case mbValue of
                    -- No changes in the dependencies and we have
                    -- an existing successful result.
                    Just (v@(Succeeded _ x), diags) -> do
                        ver <- estimateFileVersionUnsafely key (Just x) file
                        doDiagnostics (vfsVersion =<< ver) $ Vector.toList diags
                        return $ Just $ RunResult ChangedNothing old (A v) $ return ()
                    _ -> return Nothing
            _ ->
                -- assert that a "clean" rule is never a cache miss
                -- as this is likely a bug in the dirty key tracking
                assert (mode /= RunDependenciesSame) $ return Nothing
        res <- case val of
            Just res -> return res
            Nothing -> do
                staleV <- liftIO $ atomicallyNamed "define -read 3" $ getValues state key file <&> \case
                    Nothing                   -> Failed False
                    Just (Succeeded ver v, _) -> Stale Nothing ver v
                    Just (Stale d ver v, _)   -> Stale d ver v
                    Just (Failed b, _)        -> Failed b
                (mbBs, (diags, mbRes)) <- actionCatch
                    (do v <- action staleV; liftIO $ evaluate $ force v) $
                    \(e :: SomeException) -> do
                        pure (Nothing, ([ideErrorText file $ T.pack $ show e | not $ isBadDependency e],Nothing))

                ver <- estimateFileVersionUnsafely key mbRes file
                (bs, res) <- case mbRes of
                    Nothing -> do
                        pure (toShakeValue ShakeStale mbBs, staleV)
                    Just v -> pure (maybe ShakeNoCutoff ShakeResult mbBs, Succeeded ver v)
                doDiagnostics (vfsVersion =<< ver) diags
                let eq = case (bs, fmap decodeShakeValue mbOld) of
                        (ShakeResult a, Just (ShakeResult b)) -> cmp a b
                        (ShakeStale a, Just (ShakeStale b))   -> cmp a b
                        -- If we do not have a previous result
                        -- or we got ShakeNoCutoff we always return False.
                        _                                     -> False
                return $ RunResult
                    (if eq then ChangedRecomputeSame else ChangedRecomputeDiff)
                    (encodeShakeValue bs)
                    (A res) $ do
                        -- this hook needs to be run in the same transaction as the key is marked clean
                        -- see Note [Housekeeping rule cache and dirty key outside of hls-graph]
                        setValues state key file res (Vector.fromList diags)
                        modifyTVar' dirtyKeys (deleteKeySet $ toKey key file)
        return res
  where
    -- Highly unsafe helper to compute the version of a file
    -- without creating a dependency on the GetModificationTime rule
    -- (and without creating cycles in the build graph).
    estimateFileVersionUnsafely
        :: k
        -> Maybe v
        -> NormalizedFilePath
        -> Action (Maybe FileVersion)
    estimateFileVersionUnsafely _k v fp
        | fp == emptyFilePath = pure Nothing
        | Just Refl <- eqT @k @GetModificationTime = pure v
        -- GetModificationTime depends on these rules, so avoid creating a cycle
        | Just Refl <- eqT @k @AddWatchedFile = pure Nothing
        | Just Refl <- eqT @k @IsFileOfInterest = pure Nothing
        -- GetFileExists gets called for missing files
        | Just Refl <- eqT @k @GetFileExists = pure Nothing
        -- For all other rules - compute the version properly without:
        --  * creating a dependency: If everything depends on GetModificationTime, we lose early cutoff
        --  * creating bogus "file does not exists" diagnostics
        | otherwise = useWithoutDependency (GetModificationTime_ False) fp

-- Note [Housekeeping rule cache and dirty key outside of hls-graph]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Hls-graph contains its own internal running state for each key in the shakeDatabase.
-- ShakeExtras contains `state` field (rule result cache) and `dirtyKeys` (keys that became
-- dirty in between build sessions) that is not visible to the hls-graph
-- Essentially, we need to keep the rule cache and dirty key and hls-graph's internal state
-- in sync.

-- 1. A dirty key collected in a session should not be removed from dirty keys in the same session.
-- Since if we clean out the dirty key in the same session,
--     1.1. we will lose the chance to dirty its reverse dependencies. Since it only happens during session restart.
--     1.2. a key might be marked as dirty in ShakeExtras while it's being recomputed by hls-graph which could lead to it's premature removal from dirtyKeys.
--          See issue https://github.com/haskell/haskell-language-server/issues/4093 for more details.

-- 2. When a key is marked clean in the hls-graph's internal running
-- state, the rule cache and dirty keys are updated in the same transaction.
-- otherwise, some situations like the following can happen:
-- thread 1: hls-graph session run a key
-- thread 1: defineEarlyCutoff' run the action for the key
-- thread 1: the action is done, rule cache and dirty key are updated
-- thread 2: we restart the hls-graph session, thread 1 is killed, the
--           hls-graph's internal state is not updated.
-- This is problematic with early cut off because we are having a new rule cache matching the
-- old hls-graph's internal state, which might case it's reverse dependency to skip the recomputation.
-- See https://github.com/haskell/haskell-language-server/issues/4194 for more details.

traceA :: A v -> String
traceA (A Failed{})    = "Failed"
traceA (A Stale{})     = "Stale"
traceA (A Succeeded{}) = "Success"

updateFileDiagnostics :: MonadIO m
  => Recorder (WithPriority Log)
  -> NormalizedFilePath
  -> Maybe Int32
  -> Key
  -> ShakeExtras
  -> [(ShowDiagnostic,Diagnostic)] -- ^ current results
  -> m ()
updateFileDiagnostics recorder fp ver k ShakeExtras{diagnostics, hiddenDiagnostics, publishedDiagnostics, debouncer, lspEnv, ideTesting} current0 =
  liftIO $ withTrace ("update diagnostics " <> fromString(fromNormalizedFilePath fp)) $ \ addTag -> do
    addTag "key" (show k)
    let (currentShown, currentHidden) = partition ((== ShowDiag) . fst) current
        uri = filePathToUri' fp
        addTagUnsafe :: String -> String -> String -> a -> a
        addTagUnsafe msg t x v = unsafePerformIO(addTag (msg <> t) x) `seq` v
        update :: (forall a. String -> String -> a -> a) -> [Diagnostic] -> STMDiagnosticStore -> STM [Diagnostic]
        update addTagUnsafeMethod new store = addTagUnsafeMethod "count" (show $ Prelude.length new) $ setStageDiagnostics addTagUnsafeMethod uri ver (renderKey k) new store
        current = second diagsFromRule <$> current0
    addTag "version" (show ver)
    mask_ $ do
        -- Mask async exceptions to ensure that updated diagnostics are always
        -- published. Otherwise, we might never publish certain diagnostics if
        -- an exception strikes between modifyVar but before
        -- publishDiagnosticsNotification.
        newDiags <- liftIO $ atomicallyNamed "diagnostics - update" $ update (addTagUnsafe "shown ") (map snd currentShown) diagnostics
        _ <- liftIO $ atomicallyNamed "diagnostics - hidden" $ update (addTagUnsafe "hidden ") (map snd currentHidden) hiddenDiagnostics
        let uri' = filePathToUri' fp
        let delay = if null newDiags then 0.1 else 0
        registerEvent debouncer delay uri' $ withTrace ("report diagnostics " <> fromString (fromNormalizedFilePath fp)) $ \tag -> do
             join $ mask_ $ do
                 lastPublish <- atomicallyNamed "diagnostics - publish" $ STM.focus (Focus.lookupWithDefault [] <* Focus.insert newDiags) uri' publishedDiagnostics
                 let action = when (lastPublish /= newDiags) $ case lspEnv of
                        Nothing -> -- Print an LSP event.
                            logWith recorder Info $ LogDiagsDiffButNoLspEnv (map (fp, ShowDiag,) newDiags)
                        Just env -> LSP.runLspT env $ do
                            liftIO $ tag "count" (show $ Prelude.length newDiags)
                            liftIO $ tag "key" (show k)
                            LSP.sendNotification SMethod_TextDocumentPublishDiagnostics $
                                LSP.PublishDiagnosticsParams (fromNormalizedUri uri') (fmap fromIntegral ver) newDiags
                 return action
    where
        diagsFromRule :: Diagnostic -> Diagnostic
        diagsFromRule c@Diagnostic{_range}
            | coerce ideTesting = c & L.relatedInformation ?~
                         [
                        DiagnosticRelatedInformation
                            (Location
                                (filePathToUri $ fromNormalizedFilePath fp)
                                _range
                            )
                            (T.pack $ show k)
                            ]
            | otherwise = c


ideLogger :: IdeState -> Recorder (WithPriority Log)
ideLogger IdeState{shakeExtras=ShakeExtras{shakeRecorder}} = shakeRecorder

actionLogger :: Action (Recorder (WithPriority Log))
actionLogger = shakeRecorder <$> getShakeExtras

--------------------------------------------------------------------------------
type STMDiagnosticStore = STM.Map NormalizedUri StoreItem

getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) = concatMap SL.fromSortedList $ Map.elems diags

updateSTMDiagnostics ::
  (forall a. String -> String -> a -> a) ->
  STMDiagnosticStore ->
  NormalizedUri ->
  Maybe Int32 ->
  DiagnosticsBySource ->
  STM [LSP.Diagnostic]
updateSTMDiagnostics addTag store uri mv newDiagsBySource =
    getDiagnosticsFromStore . fromJust <$> STM.focus (Focus.alter update *> Focus.lookup) uri store
  where
    update (Just(StoreItem mvs dbs))
      | addTag "previous version" (show mvs) $
        addTag "previous count" (show $ Prelude.length $ filter (not.null) $ Map.elems dbs) False = undefined
      | mvs == mv = Just (StoreItem mv (newDiagsBySource <> dbs))
    update _ = Just (StoreItem mv newDiagsBySource)

-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setStageDiagnostics
    :: (forall a. String -> String -> a -> a)
    -> NormalizedUri
    -> Maybe Int32 -- ^ the time that the file these diagnostics originate from was last edited
    -> T.Text
    -> [LSP.Diagnostic]
    -> STMDiagnosticStore
    -> STM [LSP.Diagnostic]
setStageDiagnostics addTag uri ver stage diags ds = updateSTMDiagnostics addTag ds uri ver updatedDiags
  where
    !updatedDiags = Map.singleton (Just stage) $! SL.toSortedList diags

getAllDiagnostics ::
    STMDiagnosticStore ->
    STM [FileDiagnostic]
getAllDiagnostics =
    fmap (concatMap (\(k,v) -> map (fromUri k,ShowDiag,) $ getDiagnosticsFromStore v)) . ListT.toList . STM.listT

updatePositionMapping :: IdeState -> VersionedTextDocumentIdentifier -> [TextDocumentContentChangeEvent] -> STM ()
updatePositionMapping IdeState{shakeExtras = ShakeExtras{positionMapping}} VersionedTextDocumentIdentifier{..} changes =
    STM.focus (Focus.alter f) uri positionMapping
      where
        uri = toNormalizedUri _uri
        f = Just . updatePositionMappingHelper _version changes . fromMaybe mempty


updatePositionMappingHelper ::
    Int32
    -> [TextDocumentContentChangeEvent]
    -> EnumMap Int32 (PositionDelta, PositionMapping)
    -> EnumMap Int32 (PositionDelta, PositionMapping)
updatePositionMappingHelper ver changes mappingForUri = snd $
        -- Very important to use mapAccum here so that the tails of
        -- each mapping can be shared, otherwise quadratic space is
        -- used which is evident in long running sessions.
        EM.mapAccumRWithKey (\acc _k (delta, _) -> let new = addOldDelta delta acc in (new, (delta, acc)))
            zeroMapping
            (EM.insert ver (mkDelta changes, zeroMapping) mappingForUri)
