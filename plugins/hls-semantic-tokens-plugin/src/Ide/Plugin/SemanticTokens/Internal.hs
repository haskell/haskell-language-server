{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.SemanticTokens.Internal where

import           Control.Lens                         ((^.))
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Data                            (Data)
import           Data.Generics                        (Typeable, everything,
                                                       mkQ)
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
                                                       Priority (..),
                                                       TypeCheck (TypeCheck),
                                                       catchSrcErrors,
                                                       ideLogger, logPriority,
                                                       realSpan, use,
                                                       useWithStaleFast, uses)
import           Development.IDE.Core.Compile         (TcModuleResult (..),
                                                       lookupName)
import           Development.IDE.Core.Rules           (getSourceFileSource,
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
import           Development.IDE.Core.PluginUtils     (useWithStaleFastMT,
                                                       useWithStaleMT, usesMT)
import           Development.IDE.Core.PositionMapping (zeroMapping)
import           Development.IDE.Spans.Documentation  (mkDocMap)
import           Development.IDE.Spans.LocalBindings  (getDefiningBindings,
                                                       getLocalScope)
import           Development.IDE.Types.Exports        (ExportsMap (..),
                                                       createExportsMapHieDb)
import           Development.IDE.Types.HscEnvEq       (hscEnv)

import           Ide.Plugin.SemanticTokens.Mappings

logWith :: (MonadIO m) => IdeState -> Priority -> String -> m ()
logWith st prior = liftIO . logPriority (ideLogger st) prior. T.pack

-- logWith :: (MonadIO m) => IdeState -> String -> m ()
-- logWith st = liftIO . print


-----------------------
---- the api
-----------------------

-- | computeSemanticTokens
-- collect from various source
-- global names and imported name from typechecked and hscEnv
-- local names from refMap
-- name locations from hieAst
-- visible names from renamedSource
computeSemanticTokens :: IdeState -> NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens state nfp =
    runMaybeT $ do
    -- let dbg = logWith state Debug
    let logError = logWith state Debug
    -- let getAst HAR{hieAst, refMap} = hieAst
    (HAR{hieAst, refMap, hieModule}, _) <- useWithStaleMT GetHieAst nfp
    (_, ast) <- MaybeT $ return $ listToMaybe $ Map.toList $ getAsts hieAst
    (TcModuleResult{..}, _) <- useWithStaleMT TypeCheck nfp
    (hscEnv -> hsc, _) <- useWithStaleMT GhcSessionDeps nfp
    -- because the names get from ast might contain derived name
    let nameSet = nameGetter tmrRenamed
    -- only take names we cares about from ast
    let names = hieAstSpanNames nameSet ast
    -- ask hscEnv for none local types, some from typechecked can avoid asking hscEnv
    km <- liftIO $ foldrM (getType hsc) (tcg_type_env tmrTypechecked) nameSet
    -- let km = tcg_type_env tmrTypechecked
    -- global name map from type typecheck and hscEnv
    let globalAndImportedModuleNameSemanticMap = Map.fromList $ flip mapMaybe (Set.toList nameSet) $ \name -> do
            ty <- lookupNameEnv km name
            return (name, tyThingSemantic ty)
    -- local name map from refMap
    let localNameSemanticMap = toNameSemanticMap $ Map.filterWithKey (\k _ ->
                either (const False) (`Set.member` nameSet) k)
                refMap
    let combineMap = Map.unionWith (<>) localNameSemanticMap globalAndImportedModuleNameSemanticMap
    -- print all names
    -- deriving method locate in the same position as the class name
    -- liftIO $ mapM_ (\ (name, tokenType) -> dbg ("debug semanticMap: " <> showClearName name <> ":" <> show tokenType )) $ Map.toList importedModuleNameSemanticMap
    -- liftIO $ mapM_ (\ (span, name) -> dbg ("debug names: " <> showClearName name <> ":" <> showCompactRealSrc span ) ) names
    let moduleAbsTks = extractSemanticTokensFromNames combineMap names
    case semanticTokenAbsoluteSemanticTokens moduleAbsTks of
        Right tokens -> do
            source :: ByteString <- lift $ getSourceFileSource nfp
            -- liftIO $ mapM_ (\x -> mapM_ (dbg . show) x) $ recoverSemanticTokens (bytestringString source) tokens
            pure tokens
        Left err -> do
            logError $ "computeSemanticTokens: " <> show err
            MaybeT . pure $ Nothing
    where
    getType env n nameMap
      | Nothing <- lookupNameEnv nameMap n
      = do kind <- lookupKind env n
           pure $ maybe nameMap (extendNameEnv nameMap n) kind
      | otherwise = pure nameMap
    lookupKind :: HscEnv -> Name -> IO (Maybe TyThing)
    lookupKind env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env


semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
    -- let dbg = logWith state
    nfp <-  getNormalizedFilePathE (param ^. L.textDocument . L.uri)
    -- dbg $ "semanticTokensFull: " <> show nfp
    -- source :: ByteString <- lift $ getSourceFileSource nfp
    items <- liftIO
        $ runAction "SemanticTokens.semanticTokensFull" state
        $ computeSemanticTokens state nfp
    case items of
        Nothing -> pure $ InR Null
        Just items -> do
            content <- liftIO $ readFile $ fromNormalizedFilePath nfp
            -- mapM_ (mapM_ (dbg . show)) $ recoverSemanticTokens content items
            pure $ InL items


