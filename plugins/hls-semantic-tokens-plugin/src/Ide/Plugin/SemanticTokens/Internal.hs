{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.SemanticTokens.Internal where

import           Control.Lens                         ((^.))
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (..), hoistMaybe)
import           Data.Data                            (Data)
import           Data.Generics                        (Generic, Typeable,
                                                       everything, mkQ)
import           Data.List                            (sortBy)
import qualified Data.List                            as List
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import           Development.IDE                      (Action,
                                                       DocAndKindMap (DKMap),
                                                       GetBindings (GetBindings),
                                                       GetDocMap (GetDocMap),
                                                       GetHieAst (GetHieAst),
                                                       GetModIface (GetModIface),
                                                       GhcSessionDeps (GhcSessionDeps, GhcSessionDeps_),
                                                       HieAstResult (HAR, hieAst, hieModule, refMap),
                                                       IdeAction, IdeState,
                                                       Priority (..), Recorder,
                                                       RuleResult,
                                                       TypeCheck (TypeCheck),
                                                       WithPriority,
                                                       catchSrcErrors,
                                                       cmapWithPrio, ideLogger,
                                                       logPriority, realSpan,
                                                       use, useWithStaleFast,
                                                       uses)
import           Development.IDE.Core.Compile         (TcModuleResult (..),
                                                       lookupName)
import           Development.IDE.Core.Rules           (Log (LogShake),
                                                       getSourceFileSource,
                                                       runAction)
import           Development.IDE.Core.Shake           (ShakeExtras (..),
                                                       getShakeExtras,
                                                       withHieDb)
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Error                     (getNormalizedFilePathE)
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
-- import Language.LSP.Protocol.Types.Common
import qualified Data.Text                            as T
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
-- import System.FilePath (takeExtension)
import           Control.Arrow                        ((&&&), (+++))
import           Control.Monad                        (forM, forM_)
import           Control.Monad.Trans.Class            (lift)
import           Data.ByteString                      (ByteString, unpack)
import           Data.Either                          (fromRight, rights)
import           Data.Either.Extra                    (lefts)
import           Data.Function                        (on)
import qualified Data.HashSet                         as HashSet
import           Data.List.Extra                      (chunksOf, (!?))
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as Map
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import           Data.Traversable                     (for)
import           Data.Typeable                        (cast)
import           Debug.Trace                          (trace)
import           Development.IDE.Core.PluginUtils     (toCurrentPositionMT,
                                                       useWithStaleFastMT,
                                                       useWithStaleMT, usesMT)
import           Development.IDE.Core.PositionMapping (toCurrentPosition,
                                                       zeroMapping)
import           Development.IDE.Spans.Documentation  (mkDocMap)
import           Development.IDE.Spans.LocalBindings  (getDefiningBindings,
                                                       getLocalScope)
import           Development.IDE.Types.Exports        (ExportsMap (..),
                                                       createExportsMapHieDb)
import           Development.IDE.Types.HscEnvEq       (hscEnv)

import qualified Data.Map                             as M
import qualified Data.Set                             as S
import           Development.IDE                      (Rules, define,
                                                       useWithStale_)
import           Development.IDE.Core.PluginUtils     (useMT)
import           Development.IDE.Core.PositionMapping (fromCurrentRange,
                                                       toCurrentRange)
import           Development.IDE.Core.Rules           (Log)
import           Development.IDE.Graph.Classes        (Hashable, NFData (rnf))
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Utils

logWith :: (MonadIO m) => IdeState -> Priority -> String -> m ()
logWith st prior = liftIO . logPriority (ideLogger st) prior. T.pack

-----------------------
---- the api
-----------------------

-- | computeSemanticTokens
-- collect from various source
-- imported name token type from GetGlobalNameSemantic
-- local names tokenType from hieAst
-- name locations from hieAst
-- visible names from renamedSource
computeSemanticTokens :: IdeState -> NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens state nfp =
    runMaybeT $ do
    let logError = logWith state Debug
    (HAR{hieAst, refMap}, mapping) <- useWithStaleMT GetHieAst nfp
    (TcModuleResult{..}, _) <- useWithStaleMT TypeCheck nfp
    (GTTMap{getNameSemanticMap}, _) <- useWithStaleMT GetGlobalNameSemantic nfp
    (_, ast) <- MaybeT $ return $ listToMaybe $ Map.toList $ getAsts hieAst
    let nameSet = nameGetter tmrRenamed
    -- get current location from the old ones
    let spanNames = mapMaybe (\(span, name) -> (,name) <$> toCurrentRange mapping span) $ hieAstSpanNames nameSet ast
    -- because the names get from ast might contain derived name
    sMap <- mkNameSemanticMap hieAst refMap getNameSemanticMap
    let moduleAbsTks = extractSemanticTokensFromNames sMap spanNames
    case semanticTokenAbsoluteSemanticTokens moduleAbsTks of
        Right tokens -> do
            source :: ByteString <- lift $ getSourceFileSource nfp
            pure tokens
        Left err -> do
            logError $ "computeSemanticTokens: " <> show err
            MaybeT . pure $ Nothing

semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
    -- let dbg = logWith state
    nfp <-  getNormalizedFilePathE (param ^. L.textDocument . L.uri)
    logWith state Info $ "computeSemanticTokens: " <> show nfp
    items <- liftIO
        $ runAction "SemanticTokens.semanticTokensFull" state
        $ computeSemanticTokens state nfp
    case items of
        Nothing    -> pure $ InR Null
        Just items -> pure $ InL items


getImportedNameSemanticRule :: Recorder (WithPriority Log) -> Rules ()
getImportedNameSemanticRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetGlobalNameSemantic nfp -> do
      (TcModuleResult{..}, _) <- useWithStale_ TypeCheck nfp
      (hscEnv -> hsc, _) <- useWithStale_ GhcSessionDeps nfp
      (HAR{hieAst, refMap}, _) <- useWithStale_ GetHieAst nfp
      let x = mkLocalNameSemanticMap hieAst refMap
      let nameSet = nameGetter tmrRenamed
      gMap <- liftIO $ mkImportedNameSemanticMap hsc tmrTypechecked nameSet
    --   sMap <- fmap (mkNameSemanticMap hieAst refMap hsc) gMap
      return ([], GTTMap <$> gMap)

mkImportedNameSemanticMap :: MonadIO m => HscEnv -> TcGblEnv -> NameSet -> m (Maybe NameSemanticMap)
mkImportedNameSemanticMap hsc tmrTypechecked nameSet = do
      tm <- liftIO $ foldrM (getTypeExclude (tcg_type_env tmrTypechecked) hsc) emptyNameEnv $ nameSetElemsStable nameSet
      return $ Just tm
      where
      -- ignore one already in current module
      getTypeExclude localMap env n nameMap
        | Nothing <- lookupNameEnv localMap n
        = do tyThing <- lookupImported env n
             pure $ maybe nameMap (extendNameEnv nameMap n . tyThingSemantic) tyThing
        | otherwise = pure nameMap
      lookupImported :: HscEnv -> Name -> IO (Maybe TyThing)
      lookupImported env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env


-- mkNameSemanticMap :: HieASTs a -> RefMap a -> HscEnv -> TcGblEnv -> NameSet -> Maybe NameSemanticMap
-- mkNameSemanticMap :: (MonadIO m, Typeable a1) => HieASTs a1 -> RefMap a1 -> HscEnv -> TcGblEnv -> NameSet -> m (Maybe NameSemanticMap)
mkNameSemanticMap :: (Typeable a, Monad m) => HieASTs a -> RefMap a -> NameEnv SemanticTokenType -> MaybeT m (NameEnv SemanticTokenType)
mkNameSemanticMap hieAst rm gMap = do
    let lMap = mkLocalNameSemanticMap hieAst rm
    localNameSemanticMapFromTypes <- hoistMaybe lMap
    let combineMap = plusNameEnv_C (<>) localNameSemanticMapFromTypes gMap
    return combineMap

mkLocalNameSemanticMap :: (Typeable a) => HieASTs a -> RefMap a -> Maybe NameSemanticMap
mkLocalNameSemanticMap hieAst rm = do
    typedAst :: HieASTs Type <- cast hieAst
    (_, ast) <- listToMaybe $ Map.toList $ getAsts typedAst
    return $ mkLocalNameSemanticFromAst ast rm

