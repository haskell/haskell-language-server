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
import           Data.Maybe                           (isJust)
import           Data.Proxy
import           Data.String
import           Data.Text                            (Text, pack)
import           Development.IDE.Core.OfInterest      (getFilesOfInterest)
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
import           Development.IDE.Types.Action
import           Development.IDE.Types.HscEnvEq       (HscEnvEq (hscEnv))
import           Development.IDE.Types.Location       (fromUri)
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Server                  as LSP
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
                -> LSP.LspM c (Either PluginError Value)
testRequestHandler _ (BlockSeconds secs) = do
    LSP.sendNotification (SMethod_CustomMethod (Proxy @"ghcide/blocking/request")) $
      toJSON secs
    liftIO $ sleep secs
    return (Right A.Null)
testRequestHandler s (GetInterfaceFilesDir file) = liftIO $ do
    let nfp = fromUri $ toNormalizedUri file
    sess <- runAction "Test - GhcSession" s $ use_ GhcSession nfp
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
    success <- runAction ("WaitForIdeRule " <> k <> " " <> show file) s $ parseAction (fromString k) nfp
    let res = WaitForIdeRuleResult <$> success
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
    return $ Right $ toJSON $ map fromNormalizedFilePath $ HM.keys ff
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

parseAction :: CI String -> NormalizedFilePath -> Action (Either Text Bool)
parseAction "typecheck" fp = Right . isJust <$> use TypeCheck fp
parseAction "getLocatedImports" fp = Right . isJust <$> use GetLocatedImports fp
parseAction "getmodsummary" fp = Right . isJust <$> use GetModSummary fp
parseAction "getmodsummarywithouttimestamps" fp = Right . isJust <$> use GetModSummaryWithoutTimestamps fp
parseAction "getparsedmodule" fp = Right . isJust <$> use GetParsedModule fp
parseAction "ghcsession" fp = Right . isJust <$> use GhcSession fp
parseAction "ghcsessiondeps" fp = Right . isJust <$> use GhcSessionDeps fp
parseAction "gethieast" fp = Right . isJust <$> use GetHieAst fp
parseAction "getFileContents" fp = Right . isJust <$> use GetFileContents fp
parseAction other _ = return $ Left $ "Cannot parse ide rule: " <> pack (original other)

-- | a command that blocks forever. Used for testing
blockCommandId :: Text
blockCommandId = "ghcide.command.block"

blockCommandDescriptor :: PluginId -> PluginDescriptor state
blockCommandDescriptor plId = (defaultPluginDescriptor plId "") {
    pluginCommands = [PluginCommand (CommandId blockCommandId) "blocks forever" blockCommandHandler]
}

blockCommandHandler :: CommandFunction state ExecuteCommandParams
blockCommandHandler _ideState _ _params = do
  lift $ LSP.sendNotification (SMethod_CustomMethod (Proxy @"ghcide/blocking/command")) A.Null
  liftIO $ threadDelay maxBound
  pure $ InR Null
