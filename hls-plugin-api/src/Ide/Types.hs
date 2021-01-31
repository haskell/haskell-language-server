{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Ide.Types
    where

import           Data.Aeson                    hiding (defaultOptions)
import qualified Data.Map  as Map
import           Data.String
import qualified Data.Text                     as T
import           Development.Shake
import           Ide.Plugin.Config
import           Language.LSP.Types
import           Language.LSP.VFS
import           Language.LSP.Types.Lens hiding (id)
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
class PluginMethod m where

  -- | Extra data associated with requests of this type, to be passed to the handler
  type ExtraParams m :: *
  type ExtraParams m = () -- no extra data by default

  -- | How to generate the extra data
  getExtraParams :: SMethod m -> MessageParams m -> LspM Config (Either ResponseError (ExtraParams m))

  default getExtraParams :: (ExtraParams m ~ ()) => SMethod m -> MessageParams m -> LspM Config (Either ResponseError (ExtraParams m))
  getExtraParams _ _ = pure $ Right ()

  -- | Parse the configuration to check if this plugin is enabled
  pluginEnabled :: SMethod m -> PluginId -> Config -> Bool

  -- | How to combine responses from different plugins
  combineResponses :: SMethod m -> Config -> ClientCapabilities -> MessageParams m -> NonEmpty (ResponseResult m) -> ResponseResult m

  default combineResponses :: Semigroup (ResponseResult m) => SMethod m -> Config -> ClientCapabilities -> MessageParams m -> NonEmpty (ResponseResult m) -> ResponseResult m
  combineResponses _method _config _caps _params = sconcat

instance PluginMethod TextDocumentCodeAction where
  pluginEnabled _ = pluginEnabledConfig plcCodeActionsOn
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
      siToDs (SymbolInformation name kind dep (Location uri range) cont)
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
  combineResponses _ conf _ _ (toList -> xs) = consumeCompletionResponse limit $ combine xs
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

        consumeCompletionResponse limit it@(InR (CompletionList _ (List xx))) =
          case splitAt limit xx of
            (_, []) -> it
            (xx', _) -> InR (CompletionList False (List xx'))
        consumeCompletionResponse n (InL (List xx)) =
          consumeCompletionResponse n (InR (CompletionList False (List xx)))

instance PluginMethod TextDocumentFormatting where
  type ExtraParams TextDocumentFormatting = (FormattingType, T.Text)
  getExtraParams _ (DocumentFormattingParams _ (TextDocumentIdentifier uri) params) = do
    mf <- getVirtualFile $ toNormalizedUri uri
    case mf of
      Just vf -> pure $ Right (FormatText, virtualFileText vf)
      Nothing -> pure $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri

  pluginEnabled _ pid conf = (PluginId $ formattingProvider conf) == pid
  combineResponses _ _ _ _ (x :| _) = x

instance PluginMethod TextDocumentRangeFormatting where
  type ExtraParams TextDocumentRangeFormatting = (FormattingType, T.Text)
  getExtraParams _ (DocumentRangeFormattingParams _ (TextDocumentIdentifier uri) range params) = do
    mf <- getVirtualFile $ toNormalizedUri uri
    case mf of
      Just vf -> pure $ Right (FormatRange range, virtualFileText vf)
      Nothing -> pure $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri

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
  = PluginHandler (PluginId -> a -> ExtraParams m -> MessageParams m -> LspM Config (NonEmpty (Either ResponseError (ResponseResult m))))

newtype PluginHandlers a = PluginHandlers (DMap IdeMethod (PluginHandler a))

instance Semigroup (PluginHandlers a) where
  (PluginHandlers a) <> (PluginHandlers b) = PluginHandlers $ DMap.unionWithKey go a b
    where
      go _ (PluginHandler f) (PluginHandler g) = PluginHandler $ \pid ide extra params ->
        (<>) <$> f pid ide extra params <*> g pid ide extra params

instance Monoid (PluginHandlers a) where
  mempty = PluginHandlers mempty

-- | Make a handler for plugins with no extra data
mkPluginHandler
  :: PluginMethod m
  => SClientMethod m
  -> (ideState -> PluginId -> MessageParams m -> LspM Config (Either ResponseError (ResponseResult m)))
  -> PluginHandlers ideState
mkPluginHandler m f = PluginHandlers $ DMap.singleton (IdeMethod m) (PluginHandler f')
  where
    f' pid ide _ params = pure <$> f ide pid params

mkPluginHandlerExtra
  :: PluginMethod m
  => SClientMethod m
  -> (ideState -> PluginId -> ExtraParams m -> MessageParams m -> LspM Config (Either ResponseError (ResponseResult m)))
  -> PluginHandlers ideState
mkPluginHandlerExtra m f = PluginHandlers $ DMap.singleton (IdeMethod m) (PluginHandler f')
  where
    f' pid ide extra params = pure <$> f ide pid extra params

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

responseError :: T.Text -> ResponseError
responseError txt = ResponseError InvalidParams txt Nothing


