{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PackageImports     #-}
-- | A plugin that adds custom messages for use in tests
module Development.IDE.Plugin.Test
  ( TestRequest(..)
  , WaitForIdeRuleResult(..)
  , plugin
  , blockCommandDescriptor
  , blockCommandId
  ) where

import           Control.Concurrent                   (threadDelay)
import qualified Control.Exception                    as E
import           Control.Monad
import           Control.Monad.Except                 (ExceptT (..), throwError)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Data.Aeson                           (FromJSON (parseJSON),
                                                       ToJSON (toJSON), Value)
import qualified Data.Aeson.Types                     as A
import           Data.Bifunctor
import           Data.CaseInsensitive                 (CI, original)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.HashSet                         as Set
import           Data.Maybe                           (isJust)
import           Data.Proxy
import           Data.String
import           Data.Text                            (Text, pack)
import           Development.IDE.Core.OfInterest      (getFilesOfInterest)
import           Development.IDE.Core.RuleInput       (IsFileInput (inputFilePath),
                                                       ProjectHaskellInput,
                                                       SomeFileInput (SomeFileHaskellInput),
                                                       SomeHaskellInput (SomeProjectHaskellInput),
                                                       toProjectHaskellInput,
                                                       toSomeFileInput)
import           Development.IDE.Core.Rules
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph                (Action)
import qualified Development.IDE.Graph                as Graph
import           Development.IDE.Graph.Database       (ShakeDatabase,
                                                       shakeGetBuildEdges,
                                                       shakeGetBuildStep,
                                                       shakeGetCleanKeys)
import           Development.IDE.Graph.Internal.Types (Result (resultBuilt, resultChanged, resultVisited),
                                                       Step (Step))
import qualified Development.IDE.Graph.Internal.Types as Graph
import           Development.IDE.Session              (clearSessionLoaderPendingBarrier,
                                                       setSessionLoaderPendingBarrier)
import           Development.IDE.Types.Action
import           Development.IDE.Types.HscEnvEq       (HscEnvEq (hscEnv))
import           Development.IDE.Types.Location       (fromUri)
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified "list-t" ListT
import qualified StmContainers.Map                    as STM
import           System.Time.Extra

type Age = Int
data TestRequest
    = BlockSeconds Seconds           -- ^ :: Null
    | GetInterfaceFilesDir Uri       -- ^ :: String
    | GetShakeSessionQueueCount      -- ^ :: Number
    | WaitForShakeQueue -- ^ Block until the Shake queue is empty. Returns Null
    | WaitForIdeRule String Uri      -- ^ :: WaitForIdeRuleResult
    | WaitForIdeRules String [Uri]   -- ^ :: [WaitForIdeRuleResult]
    | GetBuildKeysVisited        -- ^ :: [(String]
    | GetBuildKeysBuilt          -- ^ :: [(String]
    | GetBuildKeysChanged        -- ^ :: [(String]
    | GetBuildEdgesCount         -- ^ :: Int
    | GarbageCollectDirtyKeys CheckParents Age    -- ^ :: [String] (list of keys collected)
    | GetStoredKeys                  -- ^ :: [String] (list of keys in store)
    | GetFilesOfInterest             -- ^ :: [FilePath]
    | GetRebuildsCount               -- ^ :: Int (number of times we recompiled with GHC)
    deriving Generic
    deriving anyclass (FromJSON, ToJSON)

newtype WaitForIdeRuleResult = WaitForIdeRuleResult { ideResultSuccess::Bool}
    deriving newtype (FromJSON, ToJSON)

plugin :: PluginDescriptor IdeState
plugin = (defaultPluginDescriptor "test" "") {
    pluginHandlers = mkPluginHandler (SMethod_CustomMethod (Proxy @"test")) $ \st _ ->
        testRequestHandler' st
    }
  where
      testRequestHandler' ide req
        | Just customReq <- A.parseMaybe parseJSON req
        = ExceptT $ testRequestHandler ide customReq
        | otherwise
        = throwError
        $ PluginInvalidParams "Cannot parse request"


testRequestHandler ::  IdeState
                -> TestRequest
                -> HandlerM config (Either PluginError Value)
testRequestHandler _ (BlockSeconds secs) = do
    pluginSendNotification (SMethod_CustomMethod (Proxy @"ghcide/blocking/request")) $
      toJSON secs
    liftIO $ sleep secs
    return (Right A.Null)
testRequestHandler s (GetInterfaceFilesDir file) = liftIO $ do
    let nfp = fromUri $ toNormalizedUri file
    case toProjectHaskellInput nfp of
      Nothing -> return $ Left $ PluginInvalidParams $ "Expected project Haskell file: " <> pack (show file)
      Just pHaskell -> do
        sess <- runAction "Test - GhcSession" s $ use_ GhcSession pHaskell
        let hiPath = hiDir $ hsc_dflags $ hscEnv sess
        return $ Right (toJSON hiPath)
testRequestHandler s GetShakeSessionQueueCount = liftIO $ do
    n <- atomically $ countQueue $ actionQueue $ shakeExtras s
    return $ Right (toJSON n)
testRequestHandler s WaitForShakeQueue = liftIO $ do
    atomically $ do
        n <- countQueue $ actionQueue $ shakeExtras s
        when (n>0) retry
    return $ Right A.Null
testRequestHandler s (WaitForIdeRule k file) = liftIO $ do
    let nfp = fromUri $ toNormalizedUri file
    success <- runAction ("WaitForIdeRule " <> k <> " " <> show file) s $ parseAction (fromString k) (toSomeFileInput nfp)
    let res = WaitForIdeRuleResult <$> success
    return $ bimap PluginInvalidParams toJSON res
testRequestHandler s (WaitForIdeRules k files) = liftIO $ do
    let nfps = fmap (fromUri . toNormalizedUri) files
        uniqueCount = Set.size (Set.fromList nfps)
        act = runAction ("WaitForIdeRules " <> k <> " " <> show files) s $ parseActions (fromString k) (toSomeFileInput <$> nfps)
    success <-
      if uniqueCount > 0
        then (setSessionLoaderPendingBarrier s uniqueCount >> act)
              `E.finally` clearSessionLoaderPendingBarrier s
        else act
    let res = fmap (fmap WaitForIdeRuleResult) success
    return $ bimap PluginInvalidParams toJSON res
testRequestHandler s GetBuildKeysBuilt = liftIO $ do
    keys <- getDatabaseKeys resultBuilt $ shakeDb s
    return $ Right $ toJSON $ map show keys
testRequestHandler s GetBuildKeysChanged = liftIO $ do
    keys <- getDatabaseKeys resultChanged $ shakeDb s
    return $ Right $ toJSON $ map show keys
testRequestHandler s GetBuildKeysVisited = liftIO $ do
    keys <- getDatabaseKeys resultVisited $ shakeDb s
    return $ Right $ toJSON $ map show keys
testRequestHandler s GetBuildEdgesCount = liftIO $ do
    count <- shakeGetBuildEdges $ shakeDb s
    return $ Right $ toJSON count
testRequestHandler s (GarbageCollectDirtyKeys parents age) = do
    res <- liftIO $ runAction "garbage collect dirty" s $ garbageCollectDirtyKeysOlderThan age parents
    return $ Right $ toJSON $ map show res
testRequestHandler s GetStoredKeys = do
    keys <- liftIO $ atomically $ map fst <$> ListT.toList (STM.listT $ state $ shakeExtras s)
    return $ Right $ toJSON $ map show keys
testRequestHandler s GetFilesOfInterest = do
    ff <- liftIO $ getFilesOfInterest s
    return $ Right $ toJSON $ map (fromNormalizedFilePath . inputFilePath) $ HM.keys ff
testRequestHandler s GetRebuildsCount = do
    count <- liftIO $ runAction "get build count" s getRebuildCount
    return $ Right $ toJSON count

getDatabaseKeys :: (Graph.Result -> Step)
    -> ShakeDatabase
    -> IO [Graph.Key]
getDatabaseKeys field db = do
    keys <- shakeGetCleanKeys db
    step <- shakeGetBuildStep db
    return [ k | (k, res) <- keys, field res == Step step]

withProjectFile :: SomeFileInput -> (ProjectHaskellInput -> Action (Either Text Bool)) -> Action (Either Text Bool)
withProjectFile (SomeFileHaskellInput (SomeProjectHaskellInput pFile)) action = action pFile
withProjectFile _ _ = pure $ Right False

withHaskellFile :: SomeFileInput -> (SomeHaskellInput -> Action (Either Text Bool)) -> Action (Either Text Bool)
withHaskellFile (SomeFileHaskellInput hFile) action = action hFile
withHaskellFile _ _                                 = pure $ Right False

parseAction :: CI String -> SomeFileInput -> Action (Either Text Bool)
parseAction "typecheck" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use TypeCheck pFile
parseAction "getLocatedImports" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use GetLocatedImports pFile
parseAction "getmodsummary" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use GetModSummary pFile
parseAction "getmodsummarywithouttimestamps" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use GetModSummaryWithoutTimestamps pFile
parseAction "getparsedmodule" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use GetParsedModule pFile
parseAction "ghcsession" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use GhcSession pFile
parseAction "ghcsessiondeps" fp = withProjectFile fp $ \pFile -> Right . isJust <$> use GhcSessionDeps pFile
parseAction "gethieast" fp = withHaskellFile fp $ \hFile -> Right . isJust <$> use GetHieAst hFile
parseAction "getFileContents" fp = Right . isJust <$> use GetFileContents fp
parseAction other _ = return $ Left $ "Cannot parse ide rule: " <> pack (original other)

parseActions :: CI String -> [SomeFileInput] -> Action (Either Text [Bool])
parseActions action fps
    | action == fromString "typecheck"
    , Just pFiles <- traverse projectFile fps =
        fmap (Right . map isJust) (uses TypeCheck pFiles)
  where
    projectFile (SomeFileHaskellInput (SomeProjectHaskellInput pFile)) = Just pFile
    projectFile _ = Nothing
parseActions action fps = sequence <$> traverse (parseAction action) fps

-- | a command that blocks forever. Used for testing
blockCommandId :: Text
blockCommandId = "ghcide.command.block"

blockCommandDescriptor :: PluginId -> PluginDescriptor state
blockCommandDescriptor plId = (defaultPluginDescriptor plId "") {
    pluginCommands = [PluginCommand (CommandId blockCommandId) "blocks forever" blockCommandHandler]
}

blockCommandHandler :: CommandFunction state ExecuteCommandParams
blockCommandHandler _ideState _ _params = do
  lift $ pluginSendNotification (SMethod_CustomMethod (Proxy @"ghcide/blocking/command")) A.Null
  liftIO $ threadDelay maxBound
  pure $ InR Null
