{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Ide.Types
    where

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process                    as P (getCurrentProcessId)
#else
import           System.Posix.Signals
import qualified System.Posix.Process                    as P (getProcessID)
#endif
import           Data.Aeson                    hiding (defaultOptions)
import           GHC.Generics
import qualified Data.Map  as Map
import           Data.String
import qualified Data.Text                     as T
import           Development.Shake hiding (command)
import           Ide.Plugin.Config
import           Language.LSP.Types
import           Language.LSP.VFS
import           Language.LSP.Types.Lens as J hiding (id)
import           Language.LSP.Types.Capabilities
import           Language.LSP.Server (LspM, getVirtualFile)
import           Text.Regex.TDFA.Text()
import           Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.GADT.Compare
import Data.Maybe
import Data.Semigroup
import Control.Lens ((^.))
import qualified Data.DList as DList
import qualified Data.Default
import System.IO.Unsafe
import Control.Monad
import OpenTelemetry.Eventlog
import Data.Text.Encoding (encodeUtf8)

-- ---------------------------------------------------------------------

newtype IdePlugins ideState = IdePlugins
  { ipMap :: Map.Map PluginId (PluginDescriptor ideState)}

-- ---------------------------------------------------------------------

data PluginDescriptor ideState =
  PluginDescriptor { pluginId          :: !PluginId
                   , pluginRules       :: !(Rules ())
                   , pluginCommands    :: ![PluginCommand ideState]
                   , pluginHandlers    :: PluginHandlers ideState
                   }

-- | Methods that can be handled by plugins.
-- 'ExtraParams' captures any extra data the IDE passes to the handlers for this method
-- Only methods for which we know how to combine responses can be instances of 'PluginMethod'
class HasTracing (MessageParams m) => PluginMethod m where

  -- | Parse the configuration to check if this plugin is enabled
  pluginEnabled :: SMethod m -> PluginId -> Config -> Bool

  -- | How to combine responses from different plugins
  combineResponses
    :: SMethod m
    -> Config -- ^ IDE Configuration
    -> ClientCapabilities
    -> MessageParams m
    -> NonEmpty (ResponseResult m) -> ResponseResult m

  default combineResponses :: Semigroup (ResponseResult m)
    => SMethod m -> Config -> ClientCapabilities -> MessageParams m -> NonEmpty (ResponseResult m) -> ResponseResult m
  combineResponses _method _config _caps _params = sconcat

instance PluginMethod TextDocumentCodeAction where
  pluginEnabled _ = pluginEnabledConfig plcCodeActionsOn
  combineResponses _method _config (ClientCapabilities _ textDocCaps _ _) (CodeActionParams _ _ _ _ context) resps =
      fmap compat $ List $ filter wasRequested $ (\(List x) -> x) $ sconcat resps
    where

      compat :: (Command |? CodeAction) -> (Command |? CodeAction)
      compat x@(InL _) = x
      compat x@(InR action)
        | Just _ <- textDocCaps >>= _codeAction >>= _codeActionLiteralSupport
        = x
        | otherwise = InL cmd
        where
          cmd = mkLspCommand "hls" "fallbackCodeAction" (action ^. title) (Just cmdParams)
          cmdParams = [toJSON (FallbackCodeActionParams (action ^. edit) (action ^. command))]

      wasRequested :: (Command |? CodeAction) -> Bool
      wasRequested (InL _) = True
      wasRequested (InR ca)
        | Nothing <- _only context = True
        | Just (List allowed) <- _only context
        , Just caKind <- ca ^. kind = caKind `elem` allowed
        | otherwise = False

instance PluginMethod TextDocumentCodeLens where
  pluginEnabled _ = pluginEnabledConfig plcCodeLensOn
instance PluginMethod TextDocumentRename where
  pluginEnabled _ = pluginEnabledConfig plcRenameOn
instance PluginMethod TextDocumentHover where
  pluginEnabled _ = pluginEnabledConfig plcHoverOn
  combineResponses _ _ _ _ (catMaybes . toList -> hs) = h
    where
      r = listToMaybe $ mapMaybe (^. range) hs
      h = case foldMap (^. contents) hs of
            HoverContentsMS (List []) -> Nothing
            hh                        -> Just $ Hover hh r

instance PluginMethod TextDocumentDocumentSymbol where
  pluginEnabled _ = pluginEnabledConfig plcSymbolsOn
  combineResponses _ _ (ClientCapabilities _ tdc _ _) params xs = res
    where
      uri' = params ^. textDocument . uri
      supportsHierarchy = Just True == (tdc >>= _documentSymbol >>= _hierarchicalDocumentSymbolSupport)
      dsOrSi = fmap toEither xs
      res
        | supportsHierarchy = InL $ sconcat $ fmap (either id (fmap siToDs)) dsOrSi
        | otherwise = InR $ sconcat $ fmap (either (List . concatMap dsToSi) id) dsOrSi
      siToDs (SymbolInformation name kind dep (Location _uri range) cont)
        = DocumentSymbol name cont kind dep range range Nothing
      dsToSi = go Nothing
      go :: Maybe T.Text -> DocumentSymbol -> [SymbolInformation]
      go parent ds =
        let children' :: [SymbolInformation]
            children' = concatMap (go (Just name')) (fromMaybe mempty (ds ^. children))
            loc = Location uri' (ds ^. range)
            name' = ds ^. name
            si = SymbolInformation name' (ds ^. kind) (ds ^. deprecated) loc parent
        in [si] <> children'

instance PluginMethod TextDocumentCompletion where
  pluginEnabled _ = pluginEnabledConfig plcCompletionOn
  combineResponses _ conf _ _ (toList -> xs) = snd $ consumeCompletionResponse limit $ combine xs
      where
        limit = maxCompletions conf
        combine :: [List CompletionItem |? CompletionList] -> ((List CompletionItem) |? CompletionList)
        combine cs = go True mempty cs

        go !comp acc [] =
          InR (CompletionList comp (List $ DList.toList acc))
        go comp acc (InL (List ls) : rest) =
          go comp (acc <> DList.fromList ls) rest
        go comp acc (InR (CompletionList comp' (List ls)) : rest) =
          go (comp && comp') (acc <> DList.fromList ls) rest

        -- boolean disambiguators
        isCompleteResponse, isIncompleteResponse :: Bool
        isIncompleteResponse = True
        isCompleteResponse = False

        consumeCompletionResponse limit it@(InR (CompletionList _ (List xx))) =
          case splitAt limit xx of
            -- consumed all the items, return the result as is
            (_, []) -> (limit - length xx, it)
            -- need to crop the response, set the 'isIncomplete' flag
            (xx', _) -> (0, InR (CompletionList isIncompleteResponse (List xx')))
        consumeCompletionResponse n (InL (List xx)) =
          consumeCompletionResponse n (InR (CompletionList isCompleteResponse (List xx)))

instance PluginMethod TextDocumentFormatting where
  pluginEnabled _ pid conf = (PluginId $ formattingProvider conf) == pid
  combineResponses _ _ _ _ (x :| _) = x

instance PluginMethod TextDocumentRangeFormatting where
  pluginEnabled _ pid conf = (PluginId $ formattingProvider conf) == pid
  combineResponses _ _ _ _ (x :| _) = x

-- | Methods which have a PluginMethod instance
data IdeMethod (m :: Method FromClient Request) = PluginMethod m => IdeMethod (SMethod m)
instance GEq IdeMethod where
  geq (IdeMethod a) (IdeMethod b) = geq a b
instance GCompare IdeMethod where
  gcompare (IdeMethod a) (IdeMethod b) = gcompare a b

-- | Combine handlers for the
newtype PluginHandler a (m :: Method FromClient Request)
  = PluginHandler (PluginId -> a -> MessageParams m -> LspM Config (NonEmpty (Either ResponseError (ResponseResult m))))

newtype PluginHandlers a = PluginHandlers (DMap IdeMethod (PluginHandler a))

instance Semigroup (PluginHandlers a) where
  (PluginHandlers a) <> (PluginHandlers b) = PluginHandlers $ DMap.unionWithKey go a b
    where
      go _ (PluginHandler f) (PluginHandler g) = PluginHandler $ \pid ide params ->
        (<>) <$> f pid ide params <*> g pid ide params

instance Monoid (PluginHandlers a) where
  mempty = PluginHandlers mempty

type PluginMethodHandler a m = a -> PluginId -> MessageParams m -> LspM Config (Either ResponseError (ResponseResult m))

-- | Make a handler for plugins with no extra data
mkPluginHandler
  :: PluginMethod m
  => SClientMethod m
  -> PluginMethodHandler ideState m
  -> PluginHandlers ideState
mkPluginHandler m f = PluginHandlers $ DMap.singleton (IdeMethod m) (PluginHandler f')
  where
    f' pid ide params = pure <$> f ide pid params

defaultPluginDescriptor :: PluginId -> PluginDescriptor ideState
defaultPluginDescriptor plId =
  PluginDescriptor
    plId
    mempty
    mempty
    mempty

newtype CommandId = CommandId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString CommandId where
  fromString = CommandId . T.pack

data PluginCommand ideState = forall a. (FromJSON a) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: CommandFunction ideState a
                }

-- ---------------------------------------------------------------------

type CommandFunction ideState a
  = ideState
  -> a
  -> LspM Config (Either ResponseError Value)

newtype WithSnippets = WithSnippets Bool

-- ---------------------------------------------------------------------

newtype PluginId = PluginId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString PluginId where
  fromString = PluginId . T.pack

configForPlugin :: Config -> PluginId -> PluginConfig
configForPlugin config (PluginId plugin)
    = Map.findWithDefault Data.Default.def plugin (plugins config)

-- | Checks that a given plugin is both enabled and the specific feature is
-- enabled
pluginEnabledConfig :: (PluginConfig -> Bool) -> PluginId -> Config -> Bool
pluginEnabledConfig f pid config = plcGlobalOn pluginConfig && f pluginConfig
  where
    pluginConfig = configForPlugin config pid

-- ---------------------------------------------------------------------

-- | Format the given Text as a whole or only a @Range@ of it.
-- Range must be relative to the text to format.
-- To format the whole document, read the Text from the file and use 'FormatText'
-- as the FormattingType.
data FormattingType = FormatText
                    | FormatRange Range


type FormattingMethod m =
  ( J.HasOptions (MessageParams m) FormattingOptions
  , J.HasTextDocument (MessageParams m) TextDocumentIdentifier
  , ResponseResult m ~ List TextEdit
  )

type FormattingHandler a
  =  a
  -> FormattingType
  -> T.Text
  -> NormalizedFilePath
  -> FormattingOptions
  -> LspM Config (Either ResponseError (List TextEdit))

mkFormattingHandlers :: forall a. FormattingHandler a -> PluginHandlers a
mkFormattingHandlers f = mkPluginHandler STextDocumentFormatting (provider STextDocumentFormatting)
                      <> mkPluginHandler STextDocumentRangeFormatting (provider STextDocumentRangeFormatting)
  where
    provider :: forall m. FormattingMethod m => SMethod m -> PluginMethodHandler a m
    provider m ide _pid params
      | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
        mf <- getVirtualFile $ toNormalizedUri uri
        case mf of
          Just vf -> do
            let typ = case m of
                  STextDocumentFormatting -> FormatText
                  STextDocumentRangeFormatting -> FormatRange (params ^. J.range)
                  _ -> error "mkFormattingHandlers: impossible"
            f ide typ (virtualFileText vf) nfp opts
          Nothing -> pure $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri

      | otherwise = pure $ Left $ responseError $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri
      where
        uri = params ^. J.textDocument . J.uri
        opts = params ^. J.options

-- ---------------------------------------------------------------------

responseError :: T.Text -> ResponseError
responseError txt = ResponseError InvalidParams txt Nothing

-- ---------------------------------------------------------------------

data FallbackCodeActionParams =
  FallbackCodeActionParams
    { fallbackWorkspaceEdit :: Maybe WorkspaceEdit
    , fallbackCommand       :: Maybe Command
    }
  deriving (Generic, ToJSON, FromJSON)

-- ---------------------------------------------------------------------

otSetUri :: SpanInFlight -> Uri -> IO ()
otSetUri sp (Uri t) = setTag sp "uri" (encodeUtf8 t)

class HasTracing a where
  traceWithSpan :: SpanInFlight -> a -> IO ()
  traceWithSpan _ _ = pure ()

instance {-# OVERLAPPABLE #-} (HasTextDocument a doc, HasUri doc Uri) => HasTracing a where
  traceWithSpan sp a = otSetUri sp (a ^. J.textDocument . J.uri)

instance HasTracing Value
instance HasTracing ExecuteCommandParams
instance HasTracing DidChangeWatchedFilesParams
instance HasTracing DidChangeWorkspaceFoldersParams
instance HasTracing DidChangeConfigurationParams
instance HasTracing InitializeParams
instance HasTracing (Maybe InitializedParams)
instance HasTracing WorkspaceSymbolParams where
  traceWithSpan sp (WorkspaceSymbolParams _ _ query) = setTag sp "query" (encodeUtf8 query)

-- ---------------------------------------------------------------------

{-# NOINLINE pROCESS_ID #-}
pROCESS_ID :: T.Text
pROCESS_ID = unsafePerformIO getPid

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [Value] -> Command
mkLspCommand plid cn title args' = Command title cmdId args
  where
    cmdId = mkLspCmdId pROCESS_ID plid cn
    args = List <$> args'

mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

-- | Get the operating system process id for the running server
-- instance. This should be the same for the lifetime of the instance,
-- and different from that of any other currently running instance.
getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID

getProcessID :: IO Int
installSigUsr1Handler :: IO () -> IO ()

#ifdef mingw32_HOST_OS
getProcessID = fromIntegral <$> P.getCurrentProcessId
installSigUsr1Handler _ = return ()

#else
getProcessID = fromIntegral <$> P.getProcessID

installSigUsr1Handler h = void $ installHandler sigUSR1 (Catch h) Nothing
#endif
