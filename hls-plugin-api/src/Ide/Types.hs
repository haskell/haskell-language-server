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
, Config(..), PluginConfig(..), CheckParents(..)
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
import qualified System.Win32.Process          as P (getCurrentProcessId)
#else
import           Control.Monad                 (void)
import qualified System.Posix.Process          as P (getProcessID)
import           System.Posix.Signals
#endif
import           Control.Applicative           ((<|>))
import           Control.Arrow                 ((&&&))
import           Control.Lens                  ((.~), (^.))
import           Data.Aeson                    hiding (Null, defaultOptions)
import           Data.Default
import           Data.Dependent.Map            (DMap)
import qualified Data.Dependent.Map            as DMap
import qualified Data.DList                    as DList
import           Data.GADT.Compare
import           Data.Hashable                 (Hashable)
import           Data.HashMap.Strict           (HashMap)
import qualified Data.HashMap.Strict           as HashMap
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
import           Ide.Plugin.Properties
import           Ide.TempLSPTypeFunctions
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server           (LspM, getVirtualFile)
import           Language.LSP.VFS
import           Numeric.Natural
import           OpenTelemetry.Eventlog
import           Options.Applicative           (ParserInfo)
import           System.FilePath
import           System.IO.Unsafe
import           Text.Regex.TDFA.Text          ()
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
-- will be surprises relating to config options being ignored, initially though.
data Config =
  Config
    { checkParents            :: CheckParents
    , checkProject            :: !Bool
    , formattingProvider      :: !T.Text
    , cabalFormattingProvider :: !T.Text
    , maxCompletions          :: !Int
    , plugins                 :: !(Map.Map PluginId PluginConfig)
    } deriving (Show,Eq)

instance ToJSON Config where
  toJSON Config{..} =
      object [ "haskell" .= r ]
    where
      r = object [ "checkParents"                .= checkParents
                 , "checkProject"                .= checkProject
                 , "formattingProvider"          .= formattingProvider
                 , "maxCompletions"              .= maxCompletions
                 , "plugin"                      .= Map.mapKeysMonotonic (\(PluginId p) -> p) plugins
                 ]

instance Default Config where
  def = Config
    { checkParents                = CheckOnSave
    , checkProject                = True
    , formattingProvider          = "ormolu"
    -- , formattingProvider          = "floskell"
    -- , formattingProvider          = "stylish-haskell"
    , cabalFormattingProvider     = "cabal-fmt"
    , maxCompletions              = 40
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
      , plcDiagnosticsOn    :: !Bool
      , plcHoverOn          :: !Bool
      , plcSymbolsOn        :: !Bool
      , plcCompletionOn     :: !Bool
      , plcRenameOn         :: !Bool
      , plcSelectionRangeOn :: !Bool
      , plcFoldingRangeOn   :: !Bool
      , plcConfig           :: !Object
      } deriving (Show,Eq)

instance Default PluginConfig where
  def = PluginConfig
      { plcGlobalOn         = True
      , plcCallHierarchyOn  = True
      , plcCodeActionsOn    = True
      , plcCodeLensOn       = True
      , plcDiagnosticsOn    = True
      , plcHoverOn          = True
      , plcSymbolsOn        = True
      , plcCompletionOn     = True
      , plcRenameOn         = True
      , plcSelectionRangeOn = True
      , plcFoldingRangeOn = True
      , plcConfig           = mempty
      }

instance ToJSON PluginConfig where
    toJSON (PluginConfig g ch ca cl d h s c rn sr fr cfg) = r
      where
        r = object [ "globalOn"         .= g
                   , "callHierarchyOn"  .= ch
                   , "codeActionsOn"    .= ca
                   , "codeLensOn"       .= cl
                   , "diagnosticsOn"    .= d
                   , "hoverOn"          .= h
                   , "symbolsOn"        .= s
                   , "completionOn"     .= c
                   , "renameOn"         .= rn
                   , "selectionRangeOn" .= sr
                   , "foldingRangeOn"   .= fr
                   , "config"           .= cfg
                   ]

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

-- | Methods that can be handled by plugins.
-- 'ExtraParams' captures any extra data the IDE passes to the handlers for this method
-- Only methods for which we know how to combine responses can be instances of 'PluginMethod'
class HasTracing (MessageParams m) => PluginMethod (k :: MessageKind) (m :: Method ClientToServer k) where

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

  default pluginEnabled :: (L.HasTextDocument (MessageParams m) doc, L.HasUri doc Uri)
                              => SMethod m -> MessageParams m -> PluginDescriptor c -> Config -> Bool
  pluginEnabled _ params desc conf = pluginResponsible uri desc && plcGlobalOn (configForPlugin conf desc)
    where
        uri = params ^. L.textDocument . L.uri

-- ---------------------------------------------------------------------
-- Plugin Requests
-- ---------------------------------------------------------------------

class PluginMethod Request m => PluginRequestMethod (m :: Method ClientToServer Request) where
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
    -> NonEmpty (MessageResult m) -> MessageResult m

  default combineResponses :: Semigroup (MessageResult m)
    => SMethod m -> Config -> ClientCapabilities -> MessageParams m -> NonEmpty (MessageResult m) -> MessageResult m
  combineResponses _method _config _caps _params = sconcat

instance PluginMethod Request Method_TextDocumentCodeAction where
  pluginEnabled _ msgParams pluginDesc config =
    pluginResponsible uri pluginDesc && pluginEnabledConfig plcCodeActionsOn (configForPlugin config pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginRequestMethod Method_TextDocumentCodeAction where
  combineResponses _method _config (ClientCapabilities _ textDocCaps _ _ _ _) (CodeActionParams _ _ _ _ context) resps =
      InL $ fmap compat $ filter wasRequested $ concat $ dumpNulls resps
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
        , Just caKind <- ca ^. L.kind = any (\k -> k `codeActionKindSubsumes` caKind) allowed
        | otherwise = False

      -- Copied form lsp-types 1.6 to get compilation working. May make more
      -- sense to add it back to lsp-types 2.0
      -- | Does the first 'CodeActionKind' subsume the other one, hierarchically. Reflexive.
      codeActionKindSubsumes :: CodeActionKind -> CodeActionKind -> Bool
      -- Simple but ugly implementation: prefix on the string representation
      codeActionKindSubsumes parent child = toEnumBaseType parent `T.isPrefixOf` toEnumBaseType child

instance PluginMethod Request Method_TextDocumentDefinition where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentTypeDefinition where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentDocumentHighlight where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentReferences where
  pluginEnabled _ msgParams pluginDesc _ =
    pluginResponsible uri pluginDesc
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_WorkspaceSymbol where
  -- Unconditionally enabled, but should it really be?
  pluginEnabled _ _ _ _ = True

instance PluginMethod Request Method_TextDocumentCodeLens where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcCodeLensOn (configForPlugin config pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentRename where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcRenameOn (configForPlugin config pluginDesc)
   where
      uri = msgParams ^. L.textDocument . L.uri
instance PluginMethod Request Method_TextDocumentHover where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcHoverOn (configForPlugin config pluginDesc)
   where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentDocumentSymbol where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcSymbolsOn (configForPlugin config pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_CompletionItemResolve where
  pluginEnabled _ msgParams pluginDesc config = pluginEnabledConfig plcCompletionOn (configForPlugin config pluginDesc)

instance PluginMethod Request Method_TextDocumentCompletion where
  pluginEnabled _ msgParams pluginDesc config = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcCompletionOn (configForPlugin config pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentFormatting where
  pluginEnabled SMethod_TextDocumentFormatting msgParams pluginDesc conf =
    pluginResponsible uri pluginDesc
      && (PluginId (formattingProvider conf) == pid || PluginId (cabalFormattingProvider conf) == pid)
    where
      uri = msgParams ^. L.textDocument . L.uri
      pid = pluginId pluginDesc

instance PluginMethod Request Method_TextDocumentRangeFormatting where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && (PluginId (formattingProvider conf) == pid || PluginId (cabalFormattingProvider conf) == pid)
    where
      uri = msgParams ^. L.textDocument . L.uri
      pid = pluginId pluginDesc

instance PluginMethod Request Method_TextDocumentPrepareCallHierarchy where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcCallHierarchyOn (configForPlugin conf pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentSelectionRange where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcSelectionRangeOn (configForPlugin conf pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_TextDocumentFoldingRange where
  pluginEnabled _ msgParams pluginDesc conf = pluginResponsible uri pluginDesc
      && pluginEnabledConfig plcFoldingRangeOn (configForPlugin conf pluginDesc)
    where
      uri = msgParams ^. L.textDocument . L.uri

instance PluginMethod Request Method_CallHierarchyIncomingCalls where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'
  pluginEnabled _ _ pluginDesc conf = pluginEnabledConfig plcCallHierarchyOn (configForPlugin conf pluginDesc)

instance PluginMethod Request Method_CallHierarchyOutgoingCalls where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'
  pluginEnabled _ _ pluginDesc conf = pluginEnabledConfig plcCallHierarchyOn (configForPlugin conf pluginDesc)

instance PluginMethod Request (Method_CustomMethod m) where
  pluginEnabled _ _ _ _ = True

---
instance PluginRequestMethod Method_TextDocumentDefinition where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentTypeDefinition where
  combineResponses _ _ _ _ (x :| _) = x

instance PluginRequestMethod Method_TextDocumentDocumentHighlight where

instance PluginRequestMethod Method_TextDocumentReferences where

instance PluginRequestMethod Method_WorkspaceSymbol where
    -- TODO: combine WorkspaceSymbol. Currently all WorkspaceSymbols are dumped
    -- as it is new of lsp-types 2.0.0.0
    combineResponses _ _ _ _ xs = InL $ mconcat $ takeLefts xs

instance PluginRequestMethod Method_TextDocumentCodeLens where

instance PluginRequestMethod Method_TextDocumentRename where

instance PluginRequestMethod Method_TextDocumentHover where
  combineResponses _ _ _ _ (dumpNulls -> hs :: [Hover]) =
    if mcontent ^. L.value == ""
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
      dsOrSi =  toEither <$> dumpNulls xs
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

instance PluginRequestMethod Method_CompletionItemResolve where
  -- resolving completions can only change the detail, additionalTextEdit or documentation fields
  combineResponses _ _ _ _ (x :| xs) = go x xs
    where go :: CompletionItem -> [CompletionItem] -> CompletionItem
          go !comp [] = comp
          go !comp1 (comp2:xs)
            = go (comp1
                 & L.detail              .~ comp1 ^. L.detail <> comp2 ^. L.detail
                 & L.documentation       .~ ((comp1 ^. L.documentation) <|> (comp2 ^. L.documentation)) -- difficult to write generic concatentation for docs
                 & L.additionalTextEdits .~ comp1 ^. L.additionalTextEdits <> comp2 ^. L.additionalTextEdits)
                 xs

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

-- ---------------------------------------------------------------------
-- Plugin Notifications
-- ---------------------------------------------------------------------

-- | Plugin Notification methods. No specific methods at the moment, but
-- might contain more in the future.
class PluginMethod Notification m => PluginNotificationMethod (m :: Method ClientToServer Notification)  where


instance PluginMethod Notification Method_TextDocumentDidOpen where

instance PluginMethod Notification Method_TextDocumentDidChange where

instance PluginMethod Notification Method_TextDocumentDidSave where

instance PluginMethod Notification Method_TextDocumentDidClose where

instance PluginMethod Notification Method_WorkspaceDidChangeWatchedFiles where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf desc

instance PluginMethod Notification Method_WorkspaceDidChangeWorkspaceFolders where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf desc

instance PluginMethod Notification Method_WorkspaceDidChangeConfiguration where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf desc

instance PluginMethod Notification Method_Initialized where
  -- This method has no URI parameter, thus no call to 'pluginResponsible'.
  pluginEnabled _ _ desc conf = plcGlobalOn $ configForPlugin conf desc


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

-- | Combine handlers for the
newtype PluginHandler a (m :: Method ClientToServer Request)
  = PluginHandler (PluginId -> a -> MessageParams m -> LspM Config (NonEmpty (Either ResponseError (MessageResult m))))

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

type PluginMethodHandler a m = a -> PluginId -> MessageParams m -> LspM Config (Either ResponseError (MessageResult m))

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

-- | Lookup the current config for a plugin
configForPlugin :: Config -> PluginDescriptor c -> PluginConfig
configForPlugin config PluginDescriptor{..}
    = Map.findWithDefault (configInitialGenericConfig pluginConfigDescriptor) pluginId (plugins config)

-- | Checks that a given plugin is both enabled and the specific feature is
-- enabled
pluginEnabledConfig :: (PluginConfig -> Bool) -> PluginConfig -> Bool
pluginEnabledConfig f pluginConfig = plcGlobalOn pluginConfig && f pluginConfig

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
  -> FormattingType
  -> T.Text
  -> NormalizedFilePath
  -> FormattingOptions
  -> LspM Config (Either ResponseError ([TextEdit] |? Null))

mkFormattingHandlers :: forall a. FormattingHandler a -> PluginHandlers a
mkFormattingHandlers f = mkPluginHandler SMethod_TextDocumentFormatting ( provider SMethod_TextDocumentFormatting)
                      <> mkPluginHandler SMethod_TextDocumentRangeFormatting (provider SMethod_TextDocumentRangeFormatting)
  where
    provider :: forall m. FormattingMethod m => SMethod m -> PluginMethodHandler a m
    provider m ide _pid params
      | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
        mf <- getVirtualFile $ toNormalizedUri uri
        case mf of
          Just vf -> do
            let typ = case m of
                  SMethod_TextDocumentFormatting -> FormatText
                  SMethod_TextDocumentRangeFormatting -> FormatRange (params ^. L.range)
                  _ -> Prelude.error "mkFormattingHandlers: impossible"
            f ide typ (virtualFileText vf) nfp opts
          Nothing -> pure $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri

      | otherwise = pure $ Left $ responseError $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri
      where
        uri = params ^. L.textDocument . L.uri
        opts = params ^. L.options

-- ---------------------------------------------------------------------

responseError :: T.Text -> ResponseError
responseError txt = ResponseError (InR ErrorCodes_InvalidParams) txt Nothing

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
instance HasTracing CompletionItem

-- ---------------------------------------------------------------------

{-# NOINLINE pROCESS_ID #-}
pROCESS_ID :: T.Text
pROCESS_ID = unsafePerformIO getPid

mkLspCommand :: PluginId -> CommandId -> T.Text -> Maybe [Value] -> Command
mkLspCommand plid cn title args' = Command title cmdId args
  where
    cmdId = mkLspCmdId pROCESS_ID plid cn
    args = args'

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
