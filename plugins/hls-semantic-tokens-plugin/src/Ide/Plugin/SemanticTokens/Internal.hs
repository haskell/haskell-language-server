{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.SemanticTokens.Internal where

import           Control.Lens                         ((^.))
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
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
import           Development.IDE.Core.PluginUtils     (useWithStaleFastMT,
                                                       useWithStaleMT, usesMT)
import           Development.IDE.Core.PositionMapping (zeroMapping)
import           Development.IDE.Spans.Documentation  (mkDocMap)
import           Development.IDE.Spans.LocalBindings  (getDefiningBindings,
                                                       getLocalScope)
import           Development.IDE.Types.Exports        (ExportsMap (..),
                                                       createExportsMapHieDb)
import           Development.IDE.Types.HscEnvEq       (hscEnv)

import           Development.IDE                      (Rules, define,
                                                       useWithStale_)
import           Development.IDE.Core.PluginUtils     (useMT)
import           Development.IDE.Core.Rules           (Log)
import           Development.IDE.Graph.Classes        (Hashable, NFData (rnf))
import           Ide.Plugin.SemanticTokens.Mappings

logWith :: (MonadIO m) => IdeState -> Priority -> String -> m ()
logWith st prior = liftIO . logPriority (ideLogger st) prior. T.pack

-----------------------
---- the api
-----------------------

-- | computeSemanticTokens
-- collect from various source
-- imported name from GetGlobalNameSemantic
-- local names from refMap
-- name locations from hieAst
-- visible names from renamedSource
computeSemanticTokens :: IdeState -> NormalizedFilePath -> Action (Maybe SemanticTokens)
computeSemanticTokens state nfp =
    runMaybeT $ do
    -- let dbg = logWith state Debug
    let logError = logWith state Debug
    -- let getAst HAR{hieAst, refMap} = hieAst
    (HAR{hieAst, refMap}) <- useMT GetHieAst nfp
    (TcModuleResult{..}, _) <- useWithStaleMT TypeCheck nfp
    (_, ast) <- MaybeT $ return $ listToMaybe $ Map.toList $ getAsts hieAst
    (GTTMap{getNameSemanticMap}, _) <- useWithStaleMT GetGlobalNameSemantic nfp
    -- because the names get from ast might contain derived name
    let nameSet = nameGetter tmrRenamed
    -- only take names we cares about from ast
    let names = hieAstSpanNames nameSet ast
    -- let globalAndImportedModuleNameSemanticMap = Map.fromList $ flip mapMaybe (Set.toList nameSet) $ \name -> do
    -- local name map from refMap
    let localNameSemanticMap = toNameSemanticMap nameSet refMap

    let combineMap = plusNameEnv_C (<>) localNameSemanticMap getNameSemanticMap
    let moduleAbsTks = extractSemanticTokensFromNames combineMap names
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
    -- dbg $ "semanticTokensFull: " <> show nfp
    -- source :: ByteString <- lift $ getSourceFileSource nfp
    items <- liftIO
        $ runAction "SemanticTokens.semanticTokensFull" state
        $ computeSemanticTokens state nfp
    case items of
        Nothing -> pure $ InR Null
        Just items -> do
            -- content <- liftIO $ readFile $ fromNormalizedFilePath nfp
            -- mapM_ (mapM_ (dbg . show)) $ recoverSemanticTokens content items
            pure $ InL items


getImportedNameSemanticRule :: Recorder (WithPriority Log) -> Rules ()
getImportedNameSemanticRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetGlobalNameSemantic nfp -> do
      (TcModuleResult{..}, _) <- useWithStale_ TypeCheck nfp
      (hscEnv -> hsc, _) <- useWithStale_ GhcSessionDeps nfp
      let nameSet = nameGetter tmrRenamed
      km <- liftIO $ foldrM (getTypeExclude (tcg_type_env tmrTypechecked) hsc) emptyNameEnv $ nameSetElemsStable nameSet
      return ([],Just (GTTMap $ fmap tyThingSemantic km))
      where
      -- ignore one already in current module
      getTypeExclude localMap env n nameMap
        | Nothing <- lookupNameEnv localMap n
        = do tyThing <- lookupImported env n
             pure $ maybe nameMap (extendNameEnv nameMap n) tyThing
        | otherwise = pure nameMap
      lookupImported :: HscEnv -> Name -> IO (Maybe TyThing)
      lookupImported env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env

