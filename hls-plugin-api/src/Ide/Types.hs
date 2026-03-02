{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE CUSKs                 #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Ide.Types
( PluginDescriptor(..), defaultPluginDescriptor, defaultCabalPluginDescriptor
, defaultPluginPriority
, describePlugin
, IdeCommand(..)
, IdeMethod(..)
, IdeNotification(..)
, IdePlugins(IdePlugins, ipMap)
, DynFlagsModifications(..)
, Config(..), PluginConfig(..), CheckParents(..), SessionLoadingPreferenceConfig(..)
, ConfigDescriptor(..), defaultConfigDescriptor, configForPlugin
, CustomConfig(..), mkCustomConfig
, FallbackCodeActionParams(..)
, FormattingType(..), FormattingMethod, FormattingHandler
, HasTracing(..)
, PluginCommand(..), CommandId(..), CommandFunction, mkLspCommand, mkLspCmdId
, PluginId(..)
, PluginHandler(..), mkPluginHandler
, HandlerM, runHandlerM, pluginGetClientCapabilities, pluginSendNotification, pluginSendRequest, pluginWithIndefiniteProgress
, PluginHandlers(..)
, PluginMethod(..)
, PluginMethodHandler
, PluginNotificationHandler(..), mkPluginNotificationHandler
, PluginNotificationHandlers(..)
, PluginRequestMethod(..)
, getProcessID, getPid
, getVirtualFileFromVFS
, installSigUsr1Handler
, lookupCommandProvider
, ResolveFunction
, mkResolveHandler
)
    where

#ifdef mingw32_HOST_OS

import qualified System.Win32.Process          as P (getCurrentProcessId)

#else

import qualified System.Posix.Process          as P (getProcessID)
import           System.Posix.Signals

#endif

import           Control.Applicative           ((<|>))
import           Control.Arrow                 ((&&&))
import           Control.Lens                  (_Just, view, (.~), (?~), (^.),
                                                (^?))
import           Control.Monad                 (void)
import           Control.Monad.Error.Class     (MonadError (throwError))
import           Control.Monad.IO.Class        (MonadIO)
import           Control.Monad.Trans.Except    (ExceptT, runExceptT)
import           Data.Aeson                    hiding (Null, defaultOptions)
import qualified Data.Aeson.Types              as A
import           Data.Default
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DMap
import qualified Data.DList                    as DList
import           Data.GADT.Compare
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
import           Data.Kind                     (Type)
import           Data.List.Extra               (find, sortOn)
import           Data.List.NonEmpty            (NonEmpty (..), toList)
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import           Development.IDE.Graph
import           GHC                           (DynFlags)
import           GHC.Generics
import           Ide.Plugin.Error
import           Ide.Plugin.HandleRequestTypes
import           Ide.Plugin.Properties
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types   as J
import           Language.LSP.Server
import           Language.LSP.VFS
import           Numeric.Natural
import           OpenTelemetry.Eventlog
import           Options.Applicative           (ParserInfo)
import           Prettyprinter                 as PP
import           System.IO.Unsafe
import           Text.Regex.TDFA.Text          ()
import           UnliftIO                      (MonadUnliftIO)

#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable                 (foldl')
#endif

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

-- | We (initially anyway) mirror the hie configuration, so that existing
-- clients can simply switch executable and not have any nasty surprises.  There
-- will initially be surprises relating to config options being ignored though.
data Config =
  Config
    { checkParents            :: CheckParents
    , checkProject            :: !Bool
    , formattingProvider      :: !T.Text
    , cabalFormattingProvider :: !T.Text
    , maxCompletions          :: !Int
    , sessionLoading          :: !SessionLoadingPreferenceConfig
    , plugins                 :: !(Map.Map PluginId PluginConfig)
    } deriving (Show,Eq)

instance ToJSON Config where
  toJSON Config{..} =
    object [ "checkParents"                .= checkParents
           , "checkProject"                .= checkProject
           , "formattingProvider"          .= formattingProvider
           , "cabalFormattingProvider"     .= cabalFormattingProvider
           , "maxCompletions"              .= maxCompletions
           , "sessionLoading"              .= sessionLoading
           , "plugin"                      .= Map.mapKeysMonotonic (\(PluginId p) -> p) plugins
           ]

instance Default Config where
  def = Config
    { checkParents                = CheckOnSave
    , checkProject                = True
    , formattingProvider          = "ormolu"
    -- , formattingProvider          = "stylish-haskell"
    , cabalFormattingProvider     = "cabal-gild"
    -- , cabalFormattingProvider     = "cabal-fmt"
    -- this string value needs to kept in sync with the value provided in HlsPlugins
    , maxCompletions              = 40
    , sessionLoading              = PreferSingleComponentLoading
    , plugins                     = mempty
    }

data CheckParents
    -- Note that ordering of constructors is meaningful and must be monotonically
    -- increasing in the scenarios where parents are checked
    = NeverCheck
    | CheckOnSave
    | AlwaysCheck
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)


data SessionLoadingPreferenceConfig
    = PreferSingleComponentLoading
    -- ^ Always load only a singleComponent when a new component
    -- is discovered.
    | PreferMultiComponentLoading
    -- ^ Always prefer loading multiple components in the cradle
    -- at once. This might not be always possible, if the tool doesn't
    -- support multiple components loading.
    --
    -- The cradle can decide how to handle these situations, and whether
    -- to honour the preference at all.
  deriving stock (Eq, Ord, Show, Generic)

instance Pretty SessionLoadingPreferenceConfig where
    pretty PreferSingleComponentLoading = "Prefer Single Component Loading"
    pretty PreferMultiComponentLoading  = "Prefer Multiple Components Loading"

instance ToJSON SessionLoadingPreferenceConfig where
    toJSON PreferSingleComponentLoading =
        String "singleComponent"
    toJSON PreferMultiComponentLoading =
        String "multipleComponents"

instance FromJSON SessionLoadingPreferenceConfig where
    parseJSON (String val) = case val of
        "singleComponent"    -> pure PreferSingleComponentLoading
        "multipleComponents" -> pure PreferMultiComponentLoading
        _ -> A.prependFailure "parsing SessionLoadingPreferenceConfig failed, "
            (A.parseFail $ "Expected one of \"singleComponent\" or \"multipleComponents\" but got " <> T.unpack val )
    parseJSON o = A.prependFailure "parsing SessionLoadingPreferenceConfig failed, "
            (A.typeMismatch "String" o)

-- | A PluginConfig is a generic configuration for a given HLS plugin.  It
-- provides a "big switch" to turn it on or off as a whole, as well as small
-- switches per feature, and a slot for custom config.
-- This provides a regular naming scheme for all plugin config.
data PluginConfig =
    PluginConfig
      { plcGlobalOn         :: !Bool
      , plcCallHierarchyOn  :: !Bool
      , plcCodeActionsOn    :: !Bool
      , plcCodeLensOn       :: !Bool
      , plcInlayHintsOn     :: !Bool
      , plcDiagnosticsOn    :: !Bool
      , plcHoverOn          :: !Bool
      , plcSymbolsOn        :: !Bool
      , plcSignatureHelpOn  :: !Bool
      , plcCompletionOn     :: !Bool
      , plcRenameOn         :: !Bool
      , plcSelectionRangeOn :: !Bool
      , plcFoldingRangeOn   :: !Bool
      , plcSemanticTokensOn :: !Bool
      , plcConfig           :: !Object
      } deriving (Show,Eq)

instance Default PluginConfig where
  def = PluginConfig
      { plcGlobalOn         = True
      , plcCallHierarchyOn  = True
      , plcCodeActionsOn    = True
      , plcCodeLensOn       = True
      , plcInlayHintsOn     = True
      , plcDiagnosticsOn    = True
      , plcHoverOn          = True
      , plcSymbolsOn        = True
      , plcSignatureHelpOn  = True
      , plcCompletionOn     = True
      , plcRenameOn         = True
      , plcSelectionRangeOn = True
      , plcFoldingRangeOn   = True
      , plcSemanticTokensOn = True
      , plcConfig           = mempty
      }

instance ToJSON PluginConfig where
    toJSON (PluginConfig g ch ca ih cl d h s sh c rn sr fr st cfg) = r
      where
        r = object [ "globalOn"         .= g
                   , "callHierarchyOn"  .= ch
                   , "codeActionsOn"    .= ca
                   , "codeLensOn"       .= cl
                   , "inlayHintsOn"     .= ih
                   , "diagnosticsOn"    .= d
                   , "hoverOn"          .= h
                   , "symbolsOn"        .= s
                   , "signatureHelpOn"  .= sh
                   , "completionOn"     .= c
                   , "renameOn"         .= rn
                   , "selectionRangeOn" .= sr
                   , "foldingRangeOn"   .= fr
                   , "semanticTokensOn" .= st
                   , "config"           .= cfg
                   ]

-- ---------------------------------------------------------------------

data PluginDescriptor (ideState :: Type) =
  PluginDescriptor { pluginId           :: !PluginId
                   , pluginDescription  :: !T.Text
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
                   , pluginLanguageIds    :: [J.LanguageKind]
                   -- ^ File extension of the files the plugin is responsible for.
                   --   The plugin is only allowed to handle files with these extensions.
                   --   When writing handlers, etc. for this plugin it can be assumed that all handled files are of this type.
                   --   The file extension must have a leading '.'.
                   }

describePlugin :: PluginDescriptor c -> Doc ann
describePlugin p =
  let
    PluginId pid = pluginId p
    pdesc = pluginDescription p
  in pretty pid <> ":" <> nest 4 (PP.line <> pretty pdesc)


-- | An existential wrapper of 'Properties'
data CustomConfig = forall r. CustomConfig (Properties r)

-- | Describes the configuration of a plugin.
-- A plugin may be configurable as can be seen below:
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
-- @globalOn@, @codeActionsOn@, and @codeLensOn@ etc. are called generic configs
-- which can be inferred from handlers registered by the plugin.
-- @config@ is called custom config, which is defined using 'Properties'.
data ConfigDescriptor = ConfigDescriptor {
  -- | Initial values for the generic config
  configInitialGenericConfig :: PluginConfig,
  -- | Whether or not to generate @diagnosticsOn@ config.
  -- Diagnostics emit in arbitrary shake rules,
  -- so we can't know statically if the plugin produces diagnostics
  configHasDiagnostics       :: Bool,
  -- | Custom config.
  configCustomConfig         :: CustomConfig
}

mkCustomConfig :: Properties r -> CustomConfig
mkCustomConfig = CustomConfig

defaultConfigDescriptor :: ConfigDescriptor
defaultConfigDescriptor =
    ConfigDescriptor Data.Default.def False (mkCustomConfig emptyProperties)

-- | Lookup the current config for a plugin
configForPlugin :: Config -> PluginDescriptor c -> PluginConfig
configForPlugin config PluginDescriptor{..}
    = Map.findWithDefault (configInitialGenericConfig pluginConfigDescriptor) pluginId (plugins config)

-- | Checks that a specific plugin is globally enabled in order to respond to
-- requests
pluginEnabledGlobally :: PluginDescriptor c -> Config -> HandleRequestResult
pluginEnabledGlobally desc conf = if plcGlobalOn (configForPlugin conf desc)
                           then HandlesRequest
                           else DoesNotHandleRequest DisabledGlobally

-- | Checks that a specific feature for a given plugin is enabled in order
-- to respond to requests
pluginFeatureEnabled :: (PluginConfig -> Bool) -> PluginDescriptor c -> Config -> HandleRequestResult
pluginFeatureEnabled f desc conf = if f (configForPlugin conf desc)
                                      then HandlesRequest
                                      else DoesNotHandleRequest FeatureDisabled

-- |Determine whether this request should be routed to the plugin. Fails closed
-- if we can't determine which plugin it should be routed to.
pluginResolverResponsible :: L.HasData_ m (Maybe Value) => m -> PluginDescriptor c -> HandleRequestResult
pluginResolverResponsible
  (view L.data_ -> (Just (fromJSON -> (Success (PluginResolveData o@(PluginId ot) _ _)))))
  pluginDesc =
  if pluginId pluginDesc == o
    then HandlesRequest
    else DoesNotHandleRequest $ NotResolveOwner ot
-- If we can't determine who this request belongs to, then we don't want any plugin
-- to handle it.
pluginResolverResponsible _ _ = DoesNotHandleRequest $ NotResolveOwner "(unable to determine resolve owner)"

-- | Check whether the given plugin descriptor supports the file with
-- the given path. Compares the file extension from the msgParams with the
-- file extension the plugin is responsible for.
-- We are passing the msgParams here even though we only need the URI URI here.
-- If in the future we need to be able to provide only an URI it can be
-- separated again.
pluginSupportsFileType :: (L.HasTextDocument m doc, L.HasUri doc Uri) => VFS -> m -> PluginDescriptor c -> HandleRequestResult
pluginSupportsFileType (VFS vfs) msgParams pluginDesc =
  case languageKindM of
    Just languageKind | languageKind `elem` pluginLanguageIds pluginDesc -> HandlesRequest
    _ -> DoesNotHandleRequest $ DoesNotSupportFileType (maybe "(unable to determine file type)" (T.pack . show) languageKindM)
    where
      mVFE = getVirtualFileFromVFSIncludingClosed (VFS vfs) uri
      uri = toNormalizedUri $ msgParams ^. L.textDocument . L.uri
      languageKindM =
        case mVFE of
          Just x -> virtualFileEntryLanguageKind x
          _      -> Nothing

-- | Methods that can be handled by plugins.
-- 'ExtraParams' captures any extra data the IDE passes to the handlers for this method
-- Only methods for which we know how to combine responses can be instances of 'PluginMethod'
class HasTracing (MessageParams m) => PluginMethod (k :: MessageKind) (m :: Method ClientToServer k) where

  -- | Parse the configuration to check if this plugin is globally enabled, and
  -- if the feature which handles this method is enabled. Perform sanity checks
  -- on the message to see whether the plugin handles this message in particular.
  -- This class is only used to determine whether a plugin can handle a specific
  -- request. Commands and rules do not use this logic to determine whether or
  -- not they are run.
  --
  --
  -- A common reason why a plugin won't handle a request even though it is enabled:
  --   * The plugin cannot handle requests associated with the specific URI
  --     * Since the implementation of [cabal plugins](https://github.com/haskell/haskell-language-server/issues/2940)
  --       HLS knows plugins specific to Haskell and specific to [Cabal file descriptions](https://cabal.readthedocs.io/en/3.6/cabal-package.html)
  --   * The resolve request is not routed to that specific plugin. Each resolve
  --     request needs to be routed to only one plugin.
  --
  -- Strictly speaking, we are conflating two concepts here:
  --   * Dynamically enabled (e.g. on a per-message basis)
  --   * Statically enabled (e.g. by configuration in the lsp-client)
  --     * Strictly speaking, this might also change dynamically
  --
  -- But there is no use to split it up into two different methods for now.
  handlesRequest
    :: VFS
    -- ^ The virtual file system, contains the language kind of the file.
    -> SMethod m
    -- ^ Method type.
    -> MessageParams m
    -- ^ Whether a plugin is enabled might depend on the message parameters
    --   e.g. 'pluginFileType' specifies which file extensions a plugin is allowed to handle
    -> PluginDescriptor c
    -- ^ Contains meta information such as PluginId and which file types this
    -- plugin is able to handle.
    -> Config
    -- ^ Generic config description, expected to contain 'PluginConfig' configuration
    -- for this plugin
    -> HandleRequestResult
    -- ^ Is this plugin enabled and allowed to respond to the given request
    -- with the given parameters?

  default handlesRequest :: (L.HasTextDocument (MessageParams m) doc, L.HasUri doc Uri)
                              => VFS -> SMethod m -> MessageParams m -> PluginDescriptor c -> Config -> HandleRequestResult
  handlesRequest vfs _ params desc conf =
    pluginEnabledGlobally desc conf <> pluginSupportsFileType vfs params desc

-- | Check if a plugin is enabled, if one of it's specific config's is enabled,
-- and if it supports the file
pluginEnabledWithFeature :: (L.HasTextDocument (MessageParams m) doc, L.HasUri doc Uri)
                              => (PluginConfig -> Bool) -> VFS -> SMethod m -> MessageParams m
                              -> PluginDescriptor c -> Config -> HandleRequestResult
pluginEnabledWithFeature feature vfs _ msgParams pluginDesc config =
  pluginEnabledGlobally pluginDesc config
  <> pluginFeatureEnabled feature pluginDesc config
  <> pluginSupportsFileType vfs msgParams pluginDesc

-- | Check if a plugin is enabled, if one of it's specific configs is enabled,
-- and if it's the plugin responsible for a resolve request.
pluginEnabledResolve :: L.HasData_ s (Maybe Value) => (PluginConfig -> Bool) -> VFS -> p -> s -> PluginDescriptor c -> Config -> HandleRequestResult
pluginEnabledResolve feature _ _ msgParams pluginDesc config =
    pluginEnabledGlobally pluginDesc config
    <> pluginFeatureEnabled feature pluginDesc config
    <> pluginResolverResponsible msgParams pluginDesc

instance PluginMethod Request Method_TextDocumentCodeAction where
  handlesRequest = pluginEnabledWithFeature plcCodeActionsOn

instance PluginMethod Request Method_CodeActionResolve where
  -- See Note [Resolve in PluginHandlers]
  handlesRequest = pluginEnabledResolve plcCodeActionsOn

instance PluginMethod Request Method_TextDocumentDefinition where
  handlesRequest vfs _ msgParams pluginDesc _ = pluginSupportsFileType vfs msgParams pluginDesc

instance PluginMethod Request Method_TextDocumentTypeDefinition where
  handlesRequest vfs _ msgParams pluginDesc _ = pluginSupportsFileType vfs msgParams pluginDesc

instance PluginMethod Request Method_TextDocumentImplementation where
  handlesRequest vfs _ msgParams pluginDesc _ = pluginSupportsFileType vfs msgParams pluginDesc

instance PluginMethod Request Method_TextDocumentDocumentHighlight where
  handlesRequest vfs _ msgParams pluginDesc _ = pluginSupportsFileType vfs msgParams pluginDesc

instance PluginMethod Request Method_TextDocumentReferences where
  handlesRequest vfs _ msgParams pluginDesc _ = pluginSupportsFileType vfs msgParams pluginDesc

instance PluginMethod Request Method_WorkspaceSymbol where
  -- Unconditionally enabled, but should it really be?
  handlesRequest _ _ _ _ _ = HandlesRequest

instance PluginMethod Request Method_TextDocumentInlayHint where
  handlesRequest = pluginEnabledWithFeature plcInlayHintsOn

instance PluginMethod Request Method_InlayHintResolve where
  handlesRequest = pluginEnabledResolve plcInlayHintsOn

instance PluginMethod Request Method_TextDocumentCodeLens where
  handlesRequest = pluginEnabledWithFeature plcCodeLensOn

instance PluginMethod Request Method_CodeLensResolve where
  -- See Note [Resolve in PluginHandlers]
  handlesRequest = pluginEnabledResolve plcCodeLensOn

instance PluginMethod Request Method_TextDocumentRename where
  handlesRequest = pluginEnabledWithFeature plcRenameOn

instance PluginMethod Request Method_TextDocumentPrepareRename where
  handlesRequest = pluginEnabledWithFeature plcRenameOn

instance PluginMethod Request Method_TextDocumentHover where
  handlesRequest = pluginEnabledWithFeature plcHoverOn

instance PluginMethod Request Method_TextDocumentDocumentSymbol where
  handlesRequest = pluginEnabledWithFeature plcSymbolsOn

instance PluginMethod Request Method_TextDocumentSignatureHelp where
  handlesRequest = pluginEnabledWithFeature plcSignatureHelpOn

instance PluginMethod Request Method_CompletionItemResolve where
  -- See Note [Resolve in PluginHandlers]
  handlesRequest = pluginEnabledResolve plcCompletionOn

instance PluginMethod Request Method_TextDocumentCompletion where
  handlesRequest = pluginEnabledWithFeature plcCompletionOn

instance PluginMethod Request Method_TextDocumentFormatting where
  handlesRequest vfs _ msgParams pluginDesc conf =
    (if PluginId (formattingProvider conf) == pid
          || PluginId (cabalFormattingProvider conf) == pid
        then HandlesRequest
        else DoesNotHandleRequest (NotFormattingProvider (formattingProvider conf)) )
    <> pluginSupportsFileType vfs msgParams pluginDesc
    where
      pid = pluginId pluginDesc

instance PluginMethod Request Method_TextDocumentRangeFormatting where
  handlesRequest vfs _ msgParams pluginDesc conf =
    (if PluginId (formattingProvider conf) == pid
          || PluginId (cabalFormattingProvider conf) == pid
        then HandlesRequest
        else DoesNotHandleRequest (NotFormattingProvider (formattingProvider conf)))
    <> pluginSupportsFileType vfs msgParams pluginDesc
    where
      pid = pluginId pluginDesc

instance PluginMethod Request Method_TextDocumentSemanticTokensFull where
  handlesRequest = pluginEnabledWithFeature plcSemanticTokensOn

instance PluginMethod Request Method_TextDocumentSemanticTokensFullDelta where
  handlesRequest = pluginEnabledWithFeature plcSemanticTokensOn

instance PluginMethod Request Method_TextDocumentPrepareCallHierarchy where
  handlesRequest = pluginEnabledWithFeature plcCallHierarchyOn

instance PluginMethod Request Method_TextDocumentSelectionRange where
  handlesRequest = pluginEnabledWithFeature plcSelectionRangeOn

instance PluginMethod Request Method_TextDocumentFoldingRange where
  handlesRequest = pluginEnabledWithFeature plcFoldingRangeOn

instance PluginMethod Request Method_CallHierarchyIncomingCalls where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'
  handlesRequest _ _ _ pluginDesc conf =
      pluginEnabledGlobally pluginDesc conf
    <> pluginFeatureEnabled plcCallHierarchyOn pluginDesc conf

instance PluginMethod Request Method_CallHierarchyOutgoingCalls where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'
  handlesRequest _ _ _ pluginDesc conf =
      pluginEnabledGlobally pluginDesc conf
    <> pluginFeatureEnabled plcCallHierarchyOn pluginDesc conf

instance PluginMethod Request Method_WorkspaceExecuteCommand where
  handlesRequest _ _ _ _ _ = HandlesRequest

instance PluginMethod Request (Method_CustomMethod m) where
  handlesRequest _ _ _ _ _ = HandlesRequest

-- Plugin Notifications

instance PluginMethod Notification Method_TextDocumentDidOpen where

instance PluginMethod Notification Method_TextDocumentDidChange where

instance PluginMethod Notification Method_TextDocumentDidSave where

instance PluginMethod Notification Method_TextDocumentDidClose where

instance PluginMethod Notification Method_WorkspaceDidChangeWatchedFiles where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  handlesRequest _ _ _ desc conf = pluginEnabledGlobally desc conf

instance PluginMethod Notification Method_WorkspaceDidChangeWorkspaceFolders where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  handlesRequest _ _ _ desc conf = pluginEnabledGlobally desc conf

instance PluginMethod Notification Method_WorkspaceDidChangeConfiguration where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  handlesRequest _ _ _ desc conf = pluginEnabledGlobally desc conf

instance PluginMethod Notification Method_Initialized where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  handlesRequest _ _ _ desc conf = pluginEnabledGlobally desc conf


-- ---------------------------------------------------------------------
-- Plugin Requests
-- ---------------------------------------------------------------------

class PluginMethod Request m => PluginRequestMethod (m :: Method ClientToServer Request) where
  -- | How to combine responses from different plugins.
  --
  -- For example, for Hover requests, we might have multiple producers of
  -- Hover information. We do not want to decide which one to display to the user
  -- but instead allow to define how to merge two hover request responses into one
  -- glorious hover box.
  --
  -- However, as sometimes only one handler of a request can realistically exist
  -- (such as TextDocumentFormatting), it is safe to just unconditionally report
  -- back one arbitrary result (arbitrary since it should only be one anyway).
  combineResponses
    :: SMethod m
    -> Config -- ^ IDE Configuration
    -> ClientCapabilities
    -> MessageParams m
    -> NonEmpty (MessageResult m) -> MessageResult m

  default combineResponses :: Semigroup (MessageResult m)
    => SMethod m -> Config -> ClientCapabilities -> MessageParams m -> NonEmpty (MessageResult m) -> MessageResult m
  combineResponses _method _config _caps _params = sconcat



---
instance PluginRequestMethod Method_TextDocumentCodeAction where
  combineResponses _method _config (ClientCapabilities _ textDocCaps _ _ _ _) (CodeActionParams _ _ _ _ context) resps =
      InL $ fmap compat $ concatMap (filter wasRequested) $ mapMaybe nullToMaybe $ toList resps
    where
      compat :: (Command |? CodeAction) -> (Command |? CodeAction)
      compat x@(InL _) = x
      compat x@(InR action)
        | Just _ <- textDocCaps >>= _codeAction >>= _codeActionLiteralSupport
        = x
        | otherwise = InL cmd
        where
          cmd = mkLspCommand "hls" "fallbackCodeAction" (action ^. L.title) (Just cmdParams)
          cmdParams = [toJSON (FallbackCodeActionParams (action ^. L.edit) (action ^. L.command))]

      wasRequested :: (Command |? CodeAction) -> Bool
      wasRequested (InL _) = True
      wasRequested (InR ca)
        | Nothing <- _only context = True
        | Just allowed <- _only context
        -- See https://github.com/microsoft/language-server-protocol/issues/970
        -- This is somewhat vague, but due to the hierarchical nature of action kinds, we
        -- should check whether the requested kind is a *prefix* of the action kind.
        -- That means, for example, we will return actions with kinds `quickfix.import` and
        -- `quickfix.somethingElse` if the requested kind is `quickfix`.
        , Just caKind <- ca ^. L.kind = any (`codeActionKindSubsumes` caKind) allowed
        | otherwise = False

instance PluginRequestMethod Method_CodeActionResolve where
    -- A resolve request should only have one response.
    -- See Note [Resolve in PluginHandlers].
    combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentDefinition where
    combineResponses _ _ caps _ (x :| xs)
        | Just (Just True) <- caps ^? (L.textDocument . _Just . L.definition . _Just . L.linkSupport) = foldl' mergeDefinitions x xs
        | otherwise = downgradeLinks $ foldl' mergeDefinitions x xs

instance PluginRequestMethod Method_TextDocumentTypeDefinition where
    combineResponses _ _ caps _ (x :| xs)
        | Just (Just True) <- caps ^? (L.textDocument . _Just . L.typeDefinition . _Just . L.linkSupport) = foldl' mergeDefinitions x xs
        | otherwise = downgradeLinks $ foldl' mergeDefinitions x xs

instance PluginRequestMethod Method_TextDocumentImplementation where
    combineResponses _ _ caps _ (x :| xs)
        | Just (Just True) <- caps ^? (L.textDocument . _Just . L.implementation . _Just . L.linkSupport) = foldl' mergeDefinitions x xs
        | otherwise = downgradeLinks $ foldl' mergeDefinitions x xs

instance PluginRequestMethod Method_TextDocumentDocumentHighlight where

instance PluginRequestMethod Method_TextDocumentReferences where

instance PluginRequestMethod Method_WorkspaceSymbol where
    -- TODO: combine WorkspaceSymbol. Currently all WorkspaceSymbols are dumped
    -- as it is new of lsp-types 2.0.0.0
    combineResponses _ _ _ _ xs = InL $ mconcat $ takeLefts $ toList xs

instance PluginRequestMethod Method_TextDocumentCodeLens where

instance PluginRequestMethod Method_CodeLensResolve where
    -- A resolve request should only ever get one response.
    -- See note Note [Resolve in PluginHandlers]
    combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentRename where

instance PluginRequestMethod Method_TextDocumentPrepareRename where
    -- TODO more intelligent combining?
    combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentHover where
  combineResponses _ _ _ _ (mapMaybe nullToMaybe . toList -> (hs :: [Hover])) =
    if null hs
        then InR Null
        else InL $ Hover (InL mcontent) r
    where
      r = listToMaybe $ mapMaybe (^. L.range) hs
      -- We are only taking MarkupContent here, because MarkedStrings have been
      -- deprecated for a while and don't occur in the hls codebase
      mcontent :: MarkupContent
      mcontent = mconcat $ takeLefts $ map (^. L.contents) hs

instance PluginRequestMethod Method_TextDocumentDocumentSymbol where
  combineResponses _ _ (ClientCapabilities _ tdc _ _ _ _) params xs = res
    where
      uri' = params ^. L.textDocument . L.uri
      supportsHierarchy = Just True == (tdc >>= _documentSymbol >>= _hierarchicalDocumentSymbolSupport)
      dsOrSi :: [Either [SymbolInformation] [DocumentSymbol]]
      dsOrSi =  toEither <$> mapMaybe nullToMaybe' (toList xs)
      res :: [SymbolInformation] |? ([DocumentSymbol] |? Null)
      res
        | supportsHierarchy = InR $ InL $ concatMap (either (fmap siToDs) id) dsOrSi
        | otherwise = InL $ concatMap (either id ( concatMap dsToSi)) dsOrSi
      -- Is this actually a good conversion? It's what there was before, but some
      -- things such as tags are getting lost
      siToDs :: SymbolInformation -> DocumentSymbol
      siToDs (SymbolInformation name kind _tags cont dep (Location _uri range) )
        = DocumentSymbol name cont kind Nothing dep range range Nothing
      dsToSi = go Nothing
      go :: Maybe T.Text -> DocumentSymbol -> [SymbolInformation]
      go parent ds =
        let children' :: [SymbolInformation]
            children' = concatMap (go (Just name')) (fromMaybe mempty (ds ^. L.children))
            loc = Location uri' (ds ^. L.range)
            name' = ds ^. L.name
            si = SymbolInformation name' (ds ^. L.kind) Nothing parent (ds ^. L.deprecated) loc
        in [si] <> children'

instance PluginRequestMethod Method_TextDocumentSignatureHelp where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_CompletionItemResolve where
  -- A resolve request should only have one response.
  -- See Note [Resolve in PluginHandlers]
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentCompletion where
  combineResponses _ conf _ _ (toList -> xs) = snd $ consumeCompletionResponse limit $ combine xs
      where
        limit = maxCompletions conf
        combine :: [[CompletionItem] |? (CompletionList |? Null)] -> ([CompletionItem] |? (CompletionList |? Null))
        combine cs = go True mempty cs

        go :: Bool -> DList.DList CompletionItem -> [[CompletionItem] |? (CompletionList |? Null)] -> ([CompletionItem] |? (CompletionList |? Null))
        go !comp acc [] =
           InR (InL (CompletionList comp Nothing ( DList.toList acc)))
        go comp acc ((InL ls) : rest) =
          go comp (acc <> DList.fromList ls) rest
        go comp acc ( (InR (InL (CompletionList comp' _ ls))) : rest) =
          go (comp && comp') (acc <> DList.fromList ls) rest
        go comp acc ( (InR (InR Null)) : rest) =
          go comp acc rest
        -- boolean disambiguators
        isCompleteResponse, isIncompleteResponse :: Bool
        isIncompleteResponse = True
        isCompleteResponse = False
        consumeCompletionResponse :: Int -> ([CompletionItem] |? (CompletionList |? Null)) -> (Int, [CompletionItem] |? (CompletionList |? Null))
        consumeCompletionResponse limit it@(InR (InL (CompletionList _ _ xx))) =
          case splitAt limit xx of
            -- consumed all the items, return the result as is
            (_, []) -> (limit - length xx, it)
            -- need to crop the response, set the 'isIncomplete' flag
            (xx', _) -> (0, InR (InL (CompletionList isIncompleteResponse Nothing xx')))
        consumeCompletionResponse n (InL xx) =
          consumeCompletionResponse n (InR (InL (CompletionList isCompleteResponse Nothing xx)))
        consumeCompletionResponse n (InR (InR Null)) = (n, InR (InR Null))
instance PluginRequestMethod Method_TextDocumentFormatting where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentRangeFormatting where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentPrepareCallHierarchy where

instance PluginRequestMethod Method_TextDocumentSelectionRange where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentFoldingRange where
  combineResponses _ _ _ _ x = sconcat x

instance PluginRequestMethod Method_CallHierarchyIncomingCalls where

instance PluginRequestMethod Method_CallHierarchyOutgoingCalls where

instance PluginRequestMethod (Method_CustomMethod m) where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentSemanticTokensFull where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentSemanticTokensFullDelta where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentInlayHint where
  combineResponses _ _ _ _ x = sconcat x

takeLefts :: [a |? b] -> [a]
takeLefts = mapMaybe (\x -> [res | (InL res) <- Just x])

nullToMaybe' :: (a |? (b |? Null)) -> Maybe (a |? b)
nullToMaybe' (InL x)       = Just $ InL x
nullToMaybe' (InR (InL x)) = Just $ InR x
nullToMaybe' (InR (InR _)) = Nothing

type Definitions = (Definition |? ([DefinitionLink] |? Null))

-- | Merges two definition responses (TextDocumentDefinition | TextDocumentTypeDefinition)
-- into one preserving all locations and their order (including order of the responses).
-- Upgrades Location(s) into LocationLink(s) when one of the responses is LocationLink(s). With following fields:
--  * LocationLink.originSelectionRange = Nothing
--  * LocationLink.targetUri = Location.Uri
--  * LocationLink.targetRange = Location.Range
--  * LocationLink.targetSelectionRange = Location.Range
-- Ignores Null responses.
mergeDefinitions :: Definitions -> Definitions -> Definitions
mergeDefinitions definitions1 definitions2 = case (definitions1, definitions2) of
    (InR (InR Null), def2)               -> def2
    (def1, InR (InR Null))               -> def1
    (InL def1, InL def2)                 -> InL $ mergeDefs def1 def2
    (InL def1, InR (InL links))          -> InR $ InL (defToLinks def1 ++ links)
    (InR (InL links), InL def2)          -> InR $ InL (links ++ defToLinks def2)
    (InR (InL links1), InR (InL links2)) -> InR $ InL (links1 ++ links2)
    where
        defToLinks :: Definition -> [DefinitionLink]
        defToLinks (Definition (InL location)) = [locationToDefinitionLink location]
        defToLinks (Definition (InR locations)) = map locationToDefinitionLink locations

        locationToDefinitionLink :: Location -> DefinitionLink
        locationToDefinitionLink Location{_uri, _range} = DefinitionLink LocationLink{_originSelectionRange = Nothing, _targetUri = _uri, _targetRange = _range, _targetSelectionRange = _range}

        mergeDefs :: Definition -> Definition -> Definition
        mergeDefs (Definition (InL loc1)) (Definition (InL loc2)) = Definition $ InR [loc1, loc2]
        mergeDefs (Definition (InR locs1)) (Definition (InL loc2)) = Definition $ InR (locs1 ++ [loc2])
        mergeDefs (Definition (InL loc1)) (Definition (InR locs2)) = Definition $ InR (loc1 : locs2)
        mergeDefs (Definition (InR locs1)) (Definition (InR locs2)) = Definition $ InR (locs1 ++ locs2)

downgradeLinks :: Definitions -> Definitions
downgradeLinks (InR (InL links)) = InL . Definition . InR . map linkToLocation $ links
    where
        linkToLocation :: DefinitionLink -> Location
        linkToLocation (DefinitionLink LocationLink{_targetUri, _targetRange}) = Location {_uri = _targetUri, _range = _targetRange}
downgradeLinks defs = defs
-- ---------------------------------------------------------------------
-- Plugin Notifications
-- ---------------------------------------------------------------------

-- | Plugin Notification methods. No specific methods at the moment, but
-- might contain more in the future.
class PluginMethod Notification m => PluginNotificationMethod (m :: Method ClientToServer Notification)  where


instance PluginNotificationMethod Method_TextDocumentDidOpen where

instance PluginNotificationMethod Method_TextDocumentDidChange where

instance PluginNotificationMethod Method_TextDocumentDidSave where

instance PluginNotificationMethod Method_TextDocumentDidClose where

instance PluginNotificationMethod Method_WorkspaceDidChangeWatchedFiles where

instance PluginNotificationMethod Method_WorkspaceDidChangeWorkspaceFolders where

instance PluginNotificationMethod Method_WorkspaceDidChangeConfiguration where

instance PluginNotificationMethod Method_Initialized where

-- ---------------------------------------------------------------------

-- | Methods which have a PluginMethod instance
data IdeMethod (m :: Method ClientToServer Request) = PluginRequestMethod m => IdeMethod (SMethod m)
instance GEq IdeMethod where
  geq (IdeMethod a) (IdeMethod b) = geq a b
instance GCompare IdeMethod where
  gcompare (IdeMethod a) (IdeMethod b) = gcompare a b

-- | Methods which have a PluginMethod instance
data IdeNotification (m :: Method ClientToServer Notification) = PluginNotificationMethod m => IdeNotification (SMethod m)
instance GEq IdeNotification where
  geq (IdeNotification a) (IdeNotification b) = geq a b
instance GCompare IdeNotification where
  gcompare (IdeNotification a) (IdeNotification b) = gcompare a b

-- | Restricted version of 'LspM' specific to plugins.
--
-- We use this monad for running plugins instead of 'LspM', since there are
-- parts of the LSP server state which plugins should not access directly, but
-- instead only via the build system.
newtype HandlerM config a = HandlerM { _runHandlerM :: LspM config a }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadUnliftIO)

runHandlerM :: HandlerM config a -> LspM config a
runHandlerM = _runHandlerM

-- | Wrapper of 'getClientCapabilities' for HandlerM
pluginGetClientCapabilities :: HandlerM config ClientCapabilities
pluginGetClientCapabilities = HandlerM getClientCapabilities

-- | Wrapper of 'sendNotification for HandlerM
--
-- TODO: Return notification in result instead of calling `sendNotification` directly
pluginSendNotification :: forall (m :: Method ServerToClient Notification) config. SServerMethod m -> MessageParams m -> HandlerM config ()
pluginSendNotification smethod params = HandlerM $ sendNotification smethod params

-- | Wrapper of 'sendRequest' for HandlerM
--
-- TODO: Return request in result instead of calling `sendRequest` directly
pluginSendRequest :: forall (m :: Method ServerToClient Request) config. SServerMethod m -> MessageParams m -> (Either (TResponseError m) (MessageResult m) -> HandlerM config ()) -> HandlerM config (LspId m)
pluginSendRequest smethod params action = HandlerM $ sendRequest smethod params (runHandlerM . action)

-- | Wrapper of 'withIndefiniteProgress' for HandlerM
pluginWithIndefiniteProgress :: T.Text -> Maybe ProgressToken -> ProgressCancellable -> ((T.Text -> HandlerM config ()) -> HandlerM config a) -> HandlerM config a
pluginWithIndefiniteProgress title progressToken cancellable updateAction =
  HandlerM $
    withIndefiniteProgress title progressToken cancellable $ \putUpdate ->
      runHandlerM $ updateAction (HandlerM . putUpdate)

-- | Combine handlers for the
newtype PluginHandler a (m :: Method ClientToServer Request)
  = PluginHandler (PluginId -> a -> MessageParams m -> HandlerM Config (NonEmpty (Either PluginError (MessageResult m))))

newtype PluginNotificationHandler a (m :: Method ClientToServer Notification)
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

type PluginMethodHandler a m = a -> PluginId -> MessageParams m -> ExceptT PluginError (HandlerM Config) (MessageResult m)

type PluginNotificationMethodHandler a m = a -> VFS -> PluginId -> MessageParams m -> LspM Config ()

-- | Make a handler for plugins. For how resolve works with this see
-- Note [Resolve in PluginHandlers]
mkPluginHandler
  :: forall ideState m. PluginRequestMethod m
  => SClientMethod m
  -> PluginMethodHandler ideState m
  -> PluginHandlers ideState
mkPluginHandler m f = PluginHandlers $ DMap.singleton (IdeMethod m) (PluginHandler (f' m))
  where
    f' :: SMethod m -> PluginId -> ideState -> MessageParams m -> HandlerM Config (NonEmpty (Either PluginError (MessageResult m)))
    -- We need to have separate functions for each method that supports resolve, so far we only support CodeActions
    -- CodeLens, and Completion methods.
    f' SMethod_TextDocumentCodeAction pid ide params@CodeActionParams{_textDocument=TextDocumentIdentifier {_uri}} =
      pure . fmap (wrapCodeActions pid _uri) <$> runExceptT (f ide pid params)
    f' SMethod_TextDocumentCodeLens pid ide params@CodeLensParams{_textDocument=TextDocumentIdentifier {_uri}} =
      pure . fmap (wrapCodeLenses pid _uri) <$> runExceptT (f ide pid params)
    f' SMethod_TextDocumentCompletion pid ide params@CompletionParams{_textDocument=TextDocumentIdentifier {_uri}} =
      pure . fmap (wrapCompletions pid _uri) <$> runExceptT (f ide pid params)

    -- This is the default case for all other methods
    f' _ pid ide params = pure <$> runExceptT (f ide pid params)

    -- Todo: use fancy pancy lenses to make this a few lines
    wrapCodeActions pid uri (InL ls) =
      let wrapCodeActionItem pid uri (InR c) = InR $ wrapResolveData pid uri c
          wrapCodeActionItem _ _ command@(InL _) = command
      in InL $ wrapCodeActionItem pid uri <$> ls
    wrapCodeActions _ _ (InR r) = InR r

    wrapCodeLenses pid uri (InL ls) = InL $ wrapResolveData pid uri <$> ls
    wrapCodeLenses _ _ (InR r)      = InR r

    wrapCompletions pid uri (InL ls) = InL $ wrapResolveData pid uri <$> ls
    wrapCompletions pid uri (InR (InL cl@(CompletionList{_items}))) =
      InR $ InL $ cl & L.items .~ (wrapResolveData pid uri <$> _items)
    wrapCompletions _ _ (InR (InR r)) = InR $ InR r

-- | Make a handler for plugins with no extra data
mkPluginNotificationHandler
  :: PluginNotificationMethod m
  => SClientMethod (m :: Method ClientToServer Notification)
  -> PluginNotificationMethodHandler ideState m
  -> PluginNotificationHandlers ideState
mkPluginNotificationHandler m f
    = PluginNotificationHandlers $ DMap.singleton (IdeNotification m) (PluginNotificationHandler f')
  where
    f' pid ide vfs = f ide vfs pid

defaultPluginPriority :: Natural
defaultPluginPriority = 1000

-- | Set up a plugin descriptor, initialized with default values.
-- This plugin descriptor is prepared for @haskell@ files, such as
--
--   * @.hs@
--   * @.lhs@
--   * @.hs-boot@
--
-- and handlers will be enabled for files with the appropriate file
-- extensions.
defaultPluginDescriptor :: PluginId -> T.Text -> PluginDescriptor ideState
defaultPluginDescriptor plId desc =
  PluginDescriptor
    plId
    desc
    defaultPluginPriority
    mempty
    mempty
    mempty
    defaultConfigDescriptor
    mempty
    mempty
    Nothing
    [J.LanguageKind_Haskell, J.LanguageKind_Custom "literate haskell"]

-- | Set up a plugin descriptor, initialized with default values.
-- This plugin descriptor is prepared for @.cabal@ files and as such,
-- will only respond / run when @.cabal@ files are currently in scope.
--
-- Handles files with the following extensions:
--   * @.cabal@
defaultCabalPluginDescriptor :: PluginId -> T.Text -> PluginDescriptor ideState
defaultCabalPluginDescriptor plId desc =
  PluginDescriptor
    plId
    desc
    defaultPluginPriority
    mempty
    mempty
    mempty
    defaultConfigDescriptor
    mempty
    mempty
    Nothing
    [J.LanguageKind_Custom "cabal"]

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
  -> Maybe ProgressToken
  -> a
  -> ExceptT PluginError (HandlerM Config) (Value |? Null)

-- ---------------------------------------------------------------------

type ResolveFunction ideState a (m :: Method ClientToServer Request) =
  ideState
  -> PluginId
  -> MessageParams m
  -> Uri
  -> a
  -> ExceptT PluginError (HandlerM Config) (MessageResult m)

-- | Make a handler for resolve methods. In here we take your provided ResolveFunction
-- and turn it into a PluginHandlers. See Note [Resolve in PluginHandlers]
mkResolveHandler
  :: forall ideState a m. (FromJSON a,  PluginRequestMethod m, L.HasData_ (MessageParams m) (Maybe Value))
  =>  SClientMethod m
  -> ResolveFunction ideState a m
  -> PluginHandlers ideState
mkResolveHandler m f = mkPluginHandler m $ \ideState plId params -> do
  case fromJSON <$> (params ^. L.data_) of
    (Just (Success (PluginResolveData owner@(PluginId ownerName) uri value) )) -> do
      if owner == plId
      then
        case fromJSON value of
          Success decodedValue ->
            let newParams = params & L.data_ ?~ value
            in f ideState plId newParams uri decodedValue
          Error msg ->
            -- We are assuming that if we can't decode the data, that this
            -- request belongs to another resolve handler for this plugin.
            throwError (PluginRequestRefused
                           (NotResolveOwner (ownerName <> ": error decoding payload:" <> T.pack msg)))
      -- If we are getting an owner that isn't us, this means that there is an
      -- error, as we filter these our in `pluginEnabled`
      else throwError $ PluginInternalError invalidRequest
    -- If we are getting params without a decodable data field, this means that
    -- there is an error, as we filter these our in `pluginEnabled`
    (Just (Error err)) -> throwError $ PluginInternalError (parseError (params ^. L.data_) err)
    -- If there are no params at all, this also means that there is an error,
    -- as this is filtered out in `pluginEnabled`
    _ -> throwError $ PluginInternalError invalidRequest
  where invalidRequest = "The resolve request incorrectly got routed to the wrong resolve handler!"
        parseError value err = "Unable to decode: " <> T.pack (show value) <> ". Error: " <> T.pack (show err)

wrapResolveData :: L.HasData_ a (Maybe Value) => PluginId -> Uri -> a -> a
wrapResolveData pid uri hasData =
  hasData & L.data_ .~  (toJSON . PluginResolveData pid uri <$> data_)
  where data_ = hasData ^? L.data_ . _Just

-- |Allow plugins to "own" resolve data, allowing only them to be queried for
-- the resolve action. This design has added flexibility at the cost of nested
-- Value types
data PluginResolveData = PluginResolveData {
  resolvePlugin :: PluginId
, resolveURI    :: Uri
, resolveValue  :: Value
}
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype PluginId = PluginId T.Text
  deriving (Show, Read, Eq, Ord)
  deriving newtype (ToJSON, FromJSON, Hashable)

instance IsString PluginId where
  fromString = PluginId . T.pack


-- ---------------------------------------------------------------------

-- | Format the given Text as a whole or only a @Range@ of it.
-- Range must be relative to the text to format.
-- To format the whole document, read the Text from the file and use 'FormatText'
-- as the FormattingType.
data FormattingType = FormatText
                    | FormatRange Range


type FormattingMethod m =
  ( L.HasOptions (MessageParams m) FormattingOptions
  , L.HasTextDocument (MessageParams m) TextDocumentIdentifier
  , MessageResult m ~ ([TextEdit] |? Null)
  )

type FormattingHandler a
  =  a
  -> Maybe ProgressToken
  -> FormattingType
  -> T.Text
  -> NormalizedFilePath
  -> FormattingOptions
  -> ExceptT PluginError (HandlerM Config) ([TextEdit] |? Null)

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

instance {-# OVERLAPPABLE #-} (L.HasTextDocument a doc, L.HasUri doc Uri) => HasTracing a where
  traceWithSpan sp a = otSetUri sp (a ^. L.textDocument . L.uri)

instance HasTracing Value
instance HasTracing ExecuteCommandParams
instance HasTracing DidChangeWatchedFilesParams where
  traceWithSpan sp DidChangeWatchedFilesParams{_changes} =
      setTag sp "changes" (encodeUtf8 $ fromString $ show _changes)
instance HasTracing DidChangeWorkspaceFoldersParams
instance HasTracing DidChangeConfigurationParams
instance HasTracing InitializeParams
instance HasTracing InitializedParams
instance HasTracing WorkspaceSymbolParams where
  traceWithSpan sp (WorkspaceSymbolParams _ _ query) = setTag sp "query" (encodeUtf8 query)
instance HasTracing CallHierarchyIncomingCallsParams
instance HasTracing CallHierarchyOutgoingCallsParams

-- Instances for resolve types
instance HasTracing CodeAction
instance HasTracing CodeLens
instance HasTracing CompletionItem
instance HasTracing DocumentLink
instance HasTracing InlayHint
instance HasTracing WorkspaceSymbol
-- ---------------------------------------------------------------------
--Experimental resolve refactoring
{-# NOINLINE pROCESS_ID #-}
pROCESS_ID :: T.Text
pROCESS_ID = unsafePerformIO getPid

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [Value] -> Command
mkLspCommand plid cn title args = Command title cmdId args
  where
    cmdId = mkLspCmdId pROCESS_ID plid cn

mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

-- | Get the operating system process id for the running server
-- instance. This should be the same for the lifetime of the instance,
-- and different from that of any other currently running instance.
getPid :: IO T.Text
getPid = T.pack . show <$> getProcessID

getVirtualFileFromVFS :: VFS -> NormalizedUri -> Maybe VirtualFile
getVirtualFileFromVFS (VFS vfs) uri =
  case Map.lookup uri vfs of
    Just (Open x)   -> Just x
    Just (Closed _) -> Nothing
    Nothing         -> Nothing

getVirtualFileFromVFSIncludingClosed :: VFS -> NormalizedUri -> Maybe VirtualFileEntry
getVirtualFileFromVFSIncludingClosed (VFS vfs) uri =
  case Map.lookup uri vfs of
    Just x  -> Just x
    Nothing -> Nothing


getProcessID :: IO Int
installSigUsr1Handler :: IO () -> IO ()

#ifdef mingw32_HOST_OS
getProcessID = fromIntegral <$> P.getCurrentProcessId
installSigUsr1Handler _ = return ()

#else
getProcessID = fromIntegral <$> P.getProcessID

installSigUsr1Handler h = void $ installHandler sigUSR1 (Catch h) Nothing
#endif

{- Note [Resolve in PluginHandlers]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Resolve methods have a few guarantees that need to be made by HLS,
  specifically they need to only be called once, as neither their errors nor
  their responses can be easily combined. Whereas commands, which similarly have
  the same requirements have their own codepaths for execution, for resolve
  methods we are relying on the standard PluginHandlers codepath.
  That isn't a problem, but it does mean we need to do some things extra for
  these methods.
    - First of all, whenever a handler that can be resolved sets the data_ field
    in their response, we need to intercept it, and wrap it in a data type
    PluginResolveData that allows us to route the future resolve request to the
    specific plugin which is responsible for it. (We also throw in the URI for
    convenience, because everyone needs that). We do that in mkPluginHandler.
    - When we get any resolve requests we check their data field for our
    PluginResolveData that will allow us to route the request to the right
    plugin. If we can't find out which plugin to route the request to, then we
    just don't route it at all. This is done in pluginEnabled, and
    pluginResolverResponsible.
    - Finally we have mkResolveHandler, which takes the resolve request and
    unwraps the plugins data from our PluginResolveData, parses it and passes it
    it on to the registered handler.
  It should be noted that there are some restrictions with this approach: First,
  if a plugin does not set the data_ field, than the request will not be able
  to be resolved. This is because we only wrap data_ fields that have been set
  with our PluginResolvableData tag. Second, if a plugin were to register two
  resolve handlers for the same method, than our assumptions that we never have
  two responses break, and behavior is undefined.
  -}
