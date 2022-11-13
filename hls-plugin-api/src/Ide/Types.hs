{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Ide.Types
( PluginDescriptor(..), defaultPluginDescriptor, defaultCabalPluginDescriptor
, defaultPluginPriority
, IdeCommand(..)
, IdeMethod(..)
, IdeNotification(..)
, IdePlugins(IdePlugins, ipMap)
, DynFlagsModifications(..)
, ConfigDescriptor(..), defaultConfigDescriptor, configForPlugin, pluginEnabledConfig
, CustomConfig(..), mkCustomConfig
, FallbackCodeActionParams(..)
, FormattingType(..), FormattingMethod, FormattingHandler, mkFormattingHandlers
, HasTracing(..)
, PluginCommand(..), CommandId(..), CommandFunction, mkLspCommand, mkLspCmdId
, PluginId(..)
, PluginHandler(..), mkPluginHandler
, PluginHandlers(..)
, PluginMethod(..)
, PluginMethodHandler
, PluginNotificationHandler(..), mkPluginNotificationHandler
, PluginNotificationHandlers(..)
, PluginRequestMethod(..)
, getProcessID, getPid
, installSigUsr1Handler
, responseError
, lookupCommandProvider
)
    where

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process            as P (getCurrentProcessId)
#else
import           Control.Monad                   (void)
import qualified System.Posix.Process            as P (getProcessID)
import           System.Posix.Signals
#endif
import           Control.Applicative             ((<|>))
import           Control.Arrow                   ((&&&))
import           Control.Lens                    ((^.))
import           Data.Aeson                      hiding (defaultOptions)
import qualified Data.Default
import           Data.Dependent.Map              (DMap)
import qualified Data.Dependent.Map              as DMap
import qualified Data.DList                      as DList
import           Data.GADT.Compare
import           Data.Hashable                   (Hashable)
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.List.Extra                 (find, sortOn)
import           Data.List.NonEmpty              (NonEmpty (..), toList)
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import qualified Data.Text                       as T
import           Data.Text.Encoding              (encodeUtf8)
import           Development.IDE.Graph
import           GHC                             (DynFlags)
import           GHC.Generics
import           Ide.Plugin.Config
import           Ide.Plugin.Properties
import           Language.LSP.Server             (LspM, getVirtualFile)
import           Language.LSP.Types              hiding
                                                 (SemanticTokenAbsolute (length, line),
                                                  SemanticTokenRelative (length),
                                                  SemanticTokensEdit (_start))
import           Language.LSP.Types.Capabilities (ClientCapabilities (ClientCapabilities),
                                                  TextDocumentClientCapabilities (_codeAction, _documentSymbol))
import           Language.LSP.Types.Lens         as J (HasChildren (children),
                                                       HasCommand (command),
                                                       HasContents (contents),
                                                       HasDeprecated (deprecated),
                                                       HasEdit (edit),
                                                       HasKind (kind),
                                                       HasName (name),
                                                       HasOptions (..),
                                                       HasRange (range),
                                                       HasTextDocument (..),
                                                       HasTitle (title),
                                                       HasUri (..))
import           Language.LSP.VFS
import           Numeric.Natural
import           OpenTelemetry.Eventlog
import           Options.Applicative             (ParserInfo)
import           System.FilePath
import           System.IO.Unsafe
import           Text.Regex.TDFA.Text            ()

-- ---------------------------------------------------------------------

data IdePlugins ideState = IdePlugins_
  { ipMap_                :: HashMap PluginId (PluginDescriptor ideState)
  , lookupCommandProvider :: CommandId -> Maybe PluginId
  }

-- | Smart constructor that deduplicates plugins
pattern IdePlugins :: [PluginDescriptor ideState] -> IdePlugins ideState
pattern IdePlugins{ipMap} <- IdePlugins_ (sortOn (Down . pluginPriority) . HashMap.elems -> ipMap) _
  where
    IdePlugins ipMap = IdePlugins_{ipMap_ = HashMap.fromList $ (pluginId &&& id) <$> ipMap
                                  , lookupCommandProvider = lookupPluginId ipMap
                                  }
{-# COMPLETE IdePlugins #-}

instance Semigroup (IdePlugins a) where
  (IdePlugins_ a f) <> (IdePlugins_ b g) = IdePlugins_ (a <> b) (\x -> f x <|> g x)

instance Monoid (IdePlugins a) where
  mempty = IdePlugins_ mempty (const Nothing)

-- | Lookup the plugin that exposes a particular command
lookupPluginId :: [PluginDescriptor a] -> CommandId -> Maybe PluginId
lookupPluginId ls cmd = pluginId <$> find go ls
  where
    go desc = cmd `elem` map commandId (pluginCommands desc)

-- | Hooks for modifying the 'DynFlags' at different times of the compilation
-- process. Plugins can install a 'DynFlagsModifications' via
-- 'pluginModifyDynflags' in their 'PluginDescriptor'.
data DynFlagsModifications =
  DynFlagsModifications
    { -- | Invoked immediately at the package level. Changes to the 'DynFlags'
      -- made in 'dynFlagsModifyGlobal' are guaranteed to be seen everywhere in
      -- the compilation pipeline.
      dynFlagsModifyGlobal :: DynFlags -> DynFlags
      -- | Invoked just before the parsing step, and reset immediately
      -- afterwards. 'dynFlagsModifyParser' allows plugins to enable language
      -- extensions only during parsing. for example, to let them enable
      -- certain pieces of syntax.
    , dynFlagsModifyParser :: DynFlags -> DynFlags
    }

instance Semigroup DynFlagsModifications where
  DynFlagsModifications g1 p1 <> DynFlagsModifications g2 p2 =
    DynFlagsModifications (g2 . g1) (p2 . p1)

instance Monoid DynFlagsModifications where
  mempty = DynFlagsModifications id id

-- ---------------------------------------------------------------------

newtype IdeCommand state = IdeCommand (state -> IO ())
instance Show (IdeCommand st) where show _ = "<ide command>"

-- ---------------------------------------------------------------------

data PluginDescriptor (ideState :: *) =
  PluginDescriptor { pluginId           :: !PluginId
                   -- ^ Unique identifier of the plugin.
                   , pluginPriority     :: Natural
                   -- ^ Plugin handlers are called in priority order, higher priority first
                   , pluginRules        :: !(Rules ())
                   , pluginCommands     :: ![PluginCommand ideState]
                   , pluginHandlers     :: PluginHandlers ideState
                   , pluginConfigDescriptor :: ConfigDescriptor
                   , pluginNotificationHandlers :: PluginNotificationHandlers ideState
                   , pluginModifyDynflags :: DynFlagsModifications
                   , pluginCli            :: Maybe (ParserInfo (IdeCommand ideState))
                   , pluginFileType       :: [T.Text]
                   -- ^ File extension of the files the plugin is responsible for.
                   --   The plugin is only allowed to handle files with these extensions
                   --   When writing handlers, etc. for this plugin it can be assumed that all handled files are of this type.
                   --   The file extension must have a leading '.'.
                   }

-- | Check whether the given plugin descriptor is responsible for the file with the given path.
--   Compares the file extension of the file at the given path with the file extension
--   the plugin is responsible for.
pluginResponsible :: Uri -> PluginDescriptor c -> Bool
pluginResponsible uri pluginDesc
    | Just fp <- mfp
    , T.pack (takeExtension fp) `elem` pluginFileType pluginDesc = True
    | otherwise = False
    where
      mfp = uriToFilePath uri

-- | An existential wrapper of 'Properties'
data CustomConfig = forall r. CustomConfig (Properties r)

-- | Describes the configuration a plugin.
-- A plugin may be configurable in such form:
--
-- @
-- {
--  "plugin-id": {
--    "globalOn": true,
--    "codeActionsOn": true,
--    "codeLensOn": true,
--    "config": {
--      "property1": "foo"
--     }
--   }
-- }
-- @
--
-- @globalOn@, @codeActionsOn@, and @codeLensOn@ etc. are called generic configs,
-- which can be inferred from handlers registered by the plugin.
-- @config@ is called custom config, which is defined using 'Properties'.
data ConfigDescriptor = ConfigDescriptor {
  -- | Whether or not to generate generic configs.
  configEnableGenericConfig :: Bool,
  -- | Whether or not to generate @diagnosticsOn@ config.
  -- Diagnostics emit in arbitrary shake rules,
  -- so we can't know statically if the plugin produces diagnostics
  configHasDiagnostics      :: Bool,
  -- | Custom config.
  configCustomConfig        :: CustomConfig
}

mkCustomConfig :: Properties r -> CustomConfig
mkCustomConfig = CustomConfig

defaultConfigDescriptor :: ConfigDescriptor
defaultConfigDescriptor = ConfigDescriptor True False (mkCustomConfig emptyProperties)

-- | Methods that can be handled by plugins.
-- 'ExtraParams' captures any extra data the IDE passes to the handlers for this method
-- Only methods for which we know how to combine responses can be instances of 'PluginMethod'
class HasTracing (MessageParams m) => PluginMethod (k :: MethodType) (m :: Method FromClient k) where

  -- | Parse the configuration to check if this plugin is enabled.
  -- Perform sanity checks on the message to see whether plugin is enabled
  -- for this message in particular.
  -- If a plugin is not enabled, its handlers, commands, etc... will not be
  -- run for the given message.
  --
  -- Semantically, this method described whether a Plugin is enabled configuration wise
  -- and is allowed to respond to the message. This might depend on the URI that is
  -- associated to the Message Parameters, but doesn't have to. There are requests
  -- with no associated URI that, consequentially, can't inspect the URI.
  --
  -- Common reason why a plugin might not be allowed to respond although it is enabled:
  --   * Plugin can not handle requests associated to the specific URI
  --     * Since the implementation of [cabal plugins](https://github.com/haskell/haskell-language-server/issues/2940)
  --       HLS knows plugins specific for Haskell and specific for [Cabal file descriptions](https://cabal.readthedocs.io/en/3.6/cabal-package.html)
  --
  -- Strictly speaking, we are conflating two concepts here:
  --   * Dynamically enabled (e.g. enabled on a per-message basis)
  --   * Statically enabled (e.g. by configuration in the lsp-client)
  --     * Strictly speaking, this might also change dynamically
  --
  -- But there is no use to split it up currently into two different methods for now.
  pluginEnabled
    :: SMethod m
    -- ^ Method type.
    -> MessageParams m
    -- ^ Whether a plugin is enabled might depend on the message parameters
    --   eg 'pluginFileType' specifies what file extension a plugin is allowed to handle
    -> PluginDescriptor c
    -- ^ Contains meta information such as PluginId and what file types this
    -- plugin is able to handle.
    -> Config
    -- ^ Generic config description, expected to hold 'PluginConfig' configuration
    -- for this plugin
    -> Bool
    -- ^ Is this plugin enabled and allowed to respond to the given request
    -- with the given parameters?

  default pluginEnabled :: (HasTextDocument (MessageParams m) doc, HasUri doc Uri)
                              => SMethod m -> MessageParams m -> PluginDescriptor c -> Config -> Bool
  pluginEnabled _ params desc conf = pluginResponsible uri desc && plcGlobalOn (configForPlugin conf (pluginId desc))
    where
        uri = params ^. J.textDocument . J.uri

-- ---------------------------------------------------------------------
-- Plugin Requests
-- ---------------------------------------------------------------------

class PluginMethod Request m => PluginRequestMethod (m :: Method FromClient Request) where
  -- | How to combine responses from different plugins.
  --
  -- For example, for Hover requests, we might have multiple producers of
  -- Hover information, we do not want to decide which one to display to the user
  -- but allow here to define how to merge two hover request responses into one
  -- glorious hover box.
  --
  -- However, sometimes only one handler of a request can realistically exist,
  -- such as TextDocumentFormatting, it is safe to just unconditionally report
  -- back one arbitrary result (arbitrary since it should only be one anyway).
  combineResponses
    :: SMethod m
    -> Config -- ^ IDE Configuration
    -> ClientCapabilities
    -> MessageParams m
    -> NonEmpty (ResponseResult m) -> ResponseResult m

  default combineResponses :: Semigroup (ResponseResult m)
    => SMethod m -> Config -> ClientCapabilities -> MessageParams m -> NonEmpty (ResponseResult m) -> ResponseResult m
  combineResponses _method _config _caps _params = sconcat

instance PluginMethod Request TextDocumentCodeAction where
  pluginEnabled _ msgParams pluginDesc config =
    pluginResponsible uri pluginDesc && pluginEnabledConfig plcCodeActionsOn (pluginId pluginDesc) config
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginRequestMethod TextDocumentCodeAction where
  combineResponses _method _config (ClientCapabilities _ textDocCaps _ _ _) (CodeActionParams _ _ _ _ context) resps =
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
        -- See https://github.com/microsoft/language-server-protocol/issues/970
        -- This is somewhat vague, but due to the hierarchical nature of action kinds, we
        -- should check whether the requested kind is a *prefix* of the action kind.
        -- That means, for example, we will return actions with kinds `quickfix.import` and
        -- `quickfix.somethingElse` if the requested kind is `quickfix`.
        , Just caKind <- ca ^. kind = any (\k -> k `codeActionKindSubsumes` caKind) allowed
        | otherwise = False

instance PluginMethod Request TextDocumentDefinition where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentTypeDefinition where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentDocumentHighlight where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentReferences where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request WorkspaceSymbol where
  -- Unconditionally enabled, but should it really be?
  pluginEnabled _ _ _ _ = True

instance PluginMethod Request TextDocumentCodeLens where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcCodeLensOn (pluginId pluginDesc) config
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentRename where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcRenameOn (pluginId pluginDesc) config
   where
      uri = msgParams ^. J.textDocument . J.uri
instance PluginMethod Request TextDocumentHover where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcHoverOn (pluginId pluginDesc) config
   where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentDocumentSymbol where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcSymbolsOn (pluginId pluginDesc) config
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentCompletion where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcCompletionOn (pluginId pluginDesc) config
    where
      uri = msgParams ^. J.textDocument . J.uri

instance PluginMethod Request TextDocumentFormatting where
  pluginEnabled STextDocumentFormatting msgParams pluginDesc conf =
    pluginResponsible uri pluginDesc
      && (PluginId (formattingProvider conf) == pid || PluginId (cabalFormattingProvider conf) == pid)
    where
      uri = msgParams ^. J.textDocument . J.uri
      pid = pluginId pluginDesc

instance PluginMethod Request TextDocumentRangeFormatting where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && (PluginId (formattingProvider conf) == pid || PluginId (cabalFormattingProvider conf) == pid)
    where
      uri = msgParams ^. J.textDocument . J.uri
      pid = pluginId pluginDesc

instance PluginMethod Request TextDocumentPrepareCallHierarchy where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcCallHierarchyOn pid conf
    where
      uri = msgParams ^. J.textDocument . J.uri
      pid = pluginId pluginDesc

instance PluginMethod Request TextDocumentSelectionRange where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcSelectionRangeOn pid conf
    where
      uri = msgParams ^. J.textDocument . J.uri
      pid = pluginId pluginDesc

instance PluginMethod Request TextDocumentFoldingRange where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcFoldingRangeOn pid conf
    where
      uri = msgParams ^. J.textDocument . J.uri
      pid = pluginId pluginDesc

instance PluginMethod Request CallHierarchyIncomingCalls where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'
  pluginEnabled _ _ pluginDesc conf = pluginEnabledConfig plcCallHierarchyOn pid conf
    where
      pid = pluginId pluginDesc

instance PluginMethod Request CallHierarchyOutgoingCalls where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'
  pluginEnabled _ _ pluginDesc conf = pluginEnabledConfig plcCallHierarchyOn pid conf
    where
      pid = pluginId pluginDesc

instance PluginMethod Request CustomMethod where
  pluginEnabled _ _ _ _ = True

---
instance PluginRequestMethod TextDocumentDefinition where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod TextDocumentTypeDefinition where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod TextDocumentDocumentHighlight where

instance PluginRequestMethod TextDocumentReferences where

instance PluginRequestMethod WorkspaceSymbol where

instance PluginRequestMethod TextDocumentCodeLens where

instance PluginRequestMethod TextDocumentRename where

instance PluginRequestMethod TextDocumentHover where
  combineResponses _ _ _ _ (catMaybes . toList -> hs) = h
    where
      r = listToMaybe $ mapMaybe (^. range) hs
      h = case foldMap (^. contents) hs of
            HoverContentsMS (List []) -> Nothing
            hh                        -> Just $ Hover hh r

instance PluginRequestMethod TextDocumentDocumentSymbol where
  combineResponses _ _ (ClientCapabilities _ tdc _ _ _) params xs = res
    where
      uri' = params ^. textDocument . uri
      supportsHierarchy = Just True == (tdc >>= _documentSymbol >>= _hierarchicalDocumentSymbolSupport)
      dsOrSi = fmap toEither xs
      res
        | supportsHierarchy = InL $ sconcat $ fmap (either id (fmap siToDs)) dsOrSi
        | otherwise = InR $ sconcat $ fmap (either (List . concatMap dsToSi) id) dsOrSi
      siToDs (SymbolInformation name kind _tags dep (Location _uri range) cont)
        = DocumentSymbol name cont kind Nothing dep range range Nothing
      dsToSi = go Nothing
      go :: Maybe T.Text -> DocumentSymbol -> [SymbolInformation]
      go parent ds =
        let children' :: [SymbolInformation]
            children' = concatMap (go (Just name')) (fromMaybe mempty (ds ^. children))
            loc = Location uri' (ds ^. range)
            name' = ds ^. name
            si = SymbolInformation name' (ds ^. kind) Nothing (ds ^. deprecated) loc parent
        in [si] <> children'

instance PluginRequestMethod TextDocumentCompletion where
  combineResponses _ conf _ _ (toList -> xs) = snd $ consumeCompletionResponse limit $ combine xs
      where
        limit = maxCompletions conf
        combine :: [List CompletionItem |? CompletionList] -> (List CompletionItem |? CompletionList)
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

instance PluginRequestMethod TextDocumentFormatting where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod TextDocumentRangeFormatting where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod TextDocumentPrepareCallHierarchy where

instance PluginRequestMethod TextDocumentSelectionRange where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod TextDocumentFoldingRange where
  combineResponses _ _ _ _ x = sconcat x

instance PluginRequestMethod CallHierarchyIncomingCalls where

instance PluginRequestMethod CallHierarchyOutgoingCalls where

instance PluginRequestMethod CustomMethod where
  combineResponses _ _ _ _ (x :| _) = x

-- ---------------------------------------------------------------------
-- Plugin Notifications
-- ---------------------------------------------------------------------

-- | Plugin Notification methods. No specific methods at the moment, but
-- might contain more in the future.
class PluginMethod Notification m => PluginNotificationMethod (m :: Method FromClient Notification)  where


instance PluginMethod Notification TextDocumentDidOpen where

instance PluginMethod Notification TextDocumentDidChange where

instance PluginMethod Notification TextDocumentDidSave where

instance PluginMethod Notification TextDocumentDidClose where

instance PluginMethod Notification WorkspaceDidChangeWatchedFiles where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf (pluginId desc)

instance PluginMethod Notification WorkspaceDidChangeWorkspaceFolders where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf (pluginId desc)

instance PluginMethod Notification WorkspaceDidChangeConfiguration where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf (pluginId desc)

instance PluginMethod Notification Initialized where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf (pluginId desc)


instance PluginNotificationMethod TextDocumentDidOpen where

instance PluginNotificationMethod TextDocumentDidChange where

instance PluginNotificationMethod TextDocumentDidSave where

instance PluginNotificationMethod TextDocumentDidClose where

instance PluginNotificationMethod WorkspaceDidChangeWatchedFiles where

instance PluginNotificationMethod WorkspaceDidChangeWorkspaceFolders where

instance PluginNotificationMethod WorkspaceDidChangeConfiguration where

instance PluginNotificationMethod Initialized where

-- ---------------------------------------------------------------------

-- | Methods which have a PluginMethod instance
data IdeMethod (m :: Method FromClient Request) = PluginRequestMethod m => IdeMethod (SMethod m)
instance GEq IdeMethod where
  geq (IdeMethod a) (IdeMethod b) = geq a b
instance GCompare IdeMethod where
  gcompare (IdeMethod a) (IdeMethod b) = gcompare a b

-- | Methods which have a PluginMethod instance
data IdeNotification (m :: Method FromClient Notification) = PluginNotificationMethod m => IdeNotification (SMethod m)
instance GEq IdeNotification where
  geq (IdeNotification a) (IdeNotification b) = geq a b
instance GCompare IdeNotification where
  gcompare (IdeNotification a) (IdeNotification b) = gcompare a b

-- | Combine handlers for the
newtype PluginHandler a (m :: Method FromClient Request)
  = PluginHandler (PluginId -> a -> MessageParams m -> LspM Config (NonEmpty (Either ResponseError (ResponseResult m))))

newtype PluginNotificationHandler a (m :: Method FromClient Notification)
  = PluginNotificationHandler (PluginId -> a -> VFS -> MessageParams m -> LspM Config ())

newtype PluginHandlers a             = PluginHandlers             (DMap IdeMethod       (PluginHandler a))
newtype PluginNotificationHandlers a = PluginNotificationHandlers (DMap IdeNotification (PluginNotificationHandler a))
instance Semigroup (PluginHandlers a) where
  (PluginHandlers a) <> (PluginHandlers b) = PluginHandlers $ DMap.unionWithKey go a b
    where
      go _ (PluginHandler f) (PluginHandler g) = PluginHandler $ \pid ide params ->
        (<>) <$> f pid ide params <*> g pid ide params

instance Monoid (PluginHandlers a) where
  mempty = PluginHandlers mempty

instance Semigroup (PluginNotificationHandlers a) where
  (PluginNotificationHandlers a) <> (PluginNotificationHandlers b) = PluginNotificationHandlers $ DMap.unionWithKey go a b
    where
      go _ (PluginNotificationHandler f) (PluginNotificationHandler g) = PluginNotificationHandler $ \pid ide vfs params ->
        f pid ide vfs params >> g pid ide vfs params

instance Monoid (PluginNotificationHandlers a) where
  mempty = PluginNotificationHandlers mempty

type PluginMethodHandler a m = a -> PluginId -> MessageParams m -> LspM Config (Either ResponseError (ResponseResult m))

type PluginNotificationMethodHandler a m = a -> VFS -> PluginId -> MessageParams m -> LspM Config ()

-- | Make a handler for plugins with no extra data
mkPluginHandler
  :: PluginRequestMethod m
  => SClientMethod m
  -> PluginMethodHandler ideState m
  -> PluginHandlers ideState
mkPluginHandler m f = PluginHandlers $ DMap.singleton (IdeMethod m) (PluginHandler f')
  where
    f' pid ide params = pure <$> f ide pid params

-- | Make a handler for plugins with no extra data
mkPluginNotificationHandler
  :: PluginNotificationMethod m
  => SClientMethod (m :: Method FromClient Notification)
  -> PluginNotificationMethodHandler ideState m
  -> PluginNotificationHandlers ideState
mkPluginNotificationHandler m f
    = PluginNotificationHandlers $ DMap.singleton (IdeNotification m) (PluginNotificationHandler f')
  where
    f' pid ide vfs = f ide vfs pid

defaultPluginPriority :: Natural
defaultPluginPriority = 1000

-- | Set up a plugin descriptor, initialized with default values.
-- This is plugin descriptor is prepared for @haskell@ files, such as
--
--   * @.hs@
--   * @.lhs@
--   * @.hs-boot@
--
-- and handlers will be enabled for files with the appropriate file
-- extensions.
defaultPluginDescriptor :: PluginId -> PluginDescriptor ideState
defaultPluginDescriptor plId =
  PluginDescriptor
    plId
    defaultPluginPriority
    mempty
    mempty
    mempty
    defaultConfigDescriptor
    mempty
    mempty
    Nothing
    [".hs", ".lhs", ".hs-boot"]

-- | Set up a plugin descriptor, initialized with default values.
-- This is plugin descriptor is prepared for @.cabal@ files and as such,
-- will only respond / run when @.cabal@ files are currently in scope.
--
-- Handles files with the following extensions:
--   * @.cabal@
defaultCabalPluginDescriptor :: PluginId -> PluginDescriptor ideState
defaultCabalPluginDescriptor plId =
  PluginDescriptor
    plId
    defaultPluginPriority
    mempty
    mempty
    mempty
    defaultConfigDescriptor
    mempty
    mempty
    Nothing
    [".cabal"]

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

-- ---------------------------------------------------------------------

newtype PluginId = PluginId T.Text
  deriving (Show, Read, Eq, Ord)
  deriving newtype (FromJSON, Hashable)

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
instance HasTracing DidChangeWatchedFilesParams where
  traceWithSpan sp DidChangeWatchedFilesParams{_changes} =
      setTag sp "changes" (encodeUtf8 $ fromString $ show _changes)
instance HasTracing DidChangeWorkspaceFoldersParams
instance HasTracing DidChangeConfigurationParams
instance HasTracing InitializeParams
instance HasTracing (Maybe InitializedParams)
instance HasTracing WorkspaceSymbolParams where
  traceWithSpan sp (WorkspaceSymbolParams _ _ query) = setTag sp "query" (encodeUtf8 query)
instance HasTracing CallHierarchyIncomingCallsParams
instance HasTracing CallHierarchyOutgoingCallsParams

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
