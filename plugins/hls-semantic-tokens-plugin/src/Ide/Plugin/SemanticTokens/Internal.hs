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

-- import Language.LSP.Protocol.Types.Common

-- import System.FilePath (takeExtension)
import           Control.Arrow                        ((&&&), (+++))
import           Control.Lens                         ((^.))
import           Control.Monad                        (forM, forM_)
import           Control.Monad.Except                 (MonadError (..),
                                                       liftEither, withExceptT)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (ExceptT (ExceptT),
                                                       runExceptT)
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.ByteString                      (ByteString, unpack)
import           Data.Data                            (Data)
import           Data.Either                          (fromRight, rights)
import           Data.Either.Extra                    (lefts)
import           Data.Function                        (on)
import           Data.Generics                        (Generic, Typeable,
                                                       everything, mkQ)
import qualified Data.HashSet                         as HashSet
import           Data.List                            (sortBy)
import qualified Data.List                            as List
import           Data.List.Extra                      (chunksOf, (!?))
import qualified Data.List.NonEmpty                   as NonEmpty
import qualified Data.Map                             as M
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import qualified Data.Set                             as S
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Traversable                     (for)
import           Data.Typeable                        (cast)
import           Debug.Trace                          (trace)
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
                                                       RuleResult, Rules,
                                                       TypeCheck (TypeCheck),
                                                       WithPriority,
                                                       catchSrcErrors,
                                                       cmapWithPrio, define,
                                                       ideLogger, logPriority,
                                                       realSpan, use,
                                                       useWithStaleFast,
                                                       useWithStale_, uses)
import           Development.IDE.Core.Compile         (TcModuleResult (..),
                                                       lookupName)
import           Development.IDE.Core.PluginUtils     (runActionE,
                                                       toCurrentPositionMT,
                                                       useMT, useWithStaleE,
                                                       useWithStaleFastMT,
                                                       useWithStaleMT, usesMT)
import           Development.IDE.Core.PositionMapping (fromCurrentRange,
                                                       toCurrentPosition,
                                                       toCurrentRange,
                                                       zeroMapping)
import           Development.IDE.Core.Rules           (Log (LogShake),
                                                       getSourceFileSource,
                                                       runAction)
import           Development.IDE.Core.Shake           (ShakeExtras (..),
                                                       getShakeExtras,
                                                       withHieDb)
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph.Classes        (Hashable, NFData (rnf))
import           Development.IDE.Spans.Documentation  (mkDocMap)
import           Development.IDE.Spans.LocalBindings  (getDefiningBindings,
                                                       getLocalScope)
import           Development.IDE.Types.Exports        (ExportsMap (..),
                                                       createExportsMapHieDb)
import           Development.IDE.Types.HscEnvEq       (hscEnv)
import           Ide.Plugin.Error                     (PluginError (PluginInternalError, PluginRuleFailed),
                                                       getNormalizedFilePathE)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Plugin.SemanticTokens.Utils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

logWith :: (MonadIO m) => IdeState -> Priority -> String -> m ()
logWith st prior = liftIO . logPriority (ideLogger st) prior . T.pack

liftMaybeEither :: (MonadError e m) => e -> Maybe a -> m a
liftMaybeEither e = liftEither . maybe (Left e) Right

-----------------------
---- the api
-----------------------

-- | computeSemanticTokens
-- collect from various source
-- imported name token type from GetGlobalNameSemantic
-- local names tokenType from hieAst
-- name locations from hieAst
-- visible names from renamedSource
-- computeSemanticTokens :: IdeState -> NormalizedFilePath -> Action (SemanticTokens)
computeSemanticTokens :: p -> NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeSemanticTokens state nfp = do
    (HAR {hieAst, refMap}, mapping) <- useWithStaleE GetHieAst nfp
    typedAst :: HieASTs Type <- liftMaybeEither (PluginInternalError "HieAst a convert cast to HieASTs Type") $ cast hieAst
    (TcModuleResult {..}, _) <- useWithStaleE TypeCheck nfp
    (GTTMap {importedNameSemanticMap}, _) <- useWithStaleE GetGlobalNameSemantic nfp
    (_, ast) <-
        liftMaybeEither
          (PluginRuleFailed $ "hieAst does not contains ast for:" <> T.pack (show nfp))
        $ listToMaybe
        $ Map.toList
        $ getAsts typedAst
    let nameSet = nameGetter tmrRenamed
    -- get current location from the old ones
    let spanNames = mapMaybe (\(span, name) -> (,name) <$> toCurrentRange mapping span) $ hieAstSpanNames nameSet ast
    let sMap = plusNameEnv_C (<>) importedNameSemanticMap $ mkLocalNameSemanticFromAst ast refMap
    let moduleAbsTks = extractSemanticTokensFromNames sMap spanNames
    withExceptT PluginInternalError $ liftEither $ semanticTokenAbsoluteSemanticTokens moduleAbsTks

semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
  -- let dbg = logWith state
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
--   logWith state Info $ "computeSemanticTokens: " <> show nfp
  items <- runActionE "SemanticTokens.semanticTokensFull" state $ computeSemanticTokens state nfp
  return $ InL items

getImportedNameSemanticRule :: Recorder (WithPriority Log) -> Rules ()
getImportedNameSemanticRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetGlobalNameSemantic nfp -> do
    (TcModuleResult {..}, _) <- useWithStale_ TypeCheck nfp
    (hscEnv -> hsc, _) <- useWithStale_ GhcSessionDeps nfp
    (HAR {hieAst, refMap}, _) <- useWithStale_ GetHieAst nfp
    let nameSet = nameGetter tmrRenamed
    tm <- liftIO $ foldrM (getTypeExclude (tcg_type_env tmrTypechecked) hsc) emptyNameEnv $ nameSetElemsStable nameSet
    return ([], Just $ GTTMap tm)
    where
        -- ignore one already in current module
        getTypeExclude localMap env n nameMap
            | Nothing <- lookupNameEnv localMap n =
                do
                    tyThing <- lookupImported env n
                    pure $ maybe nameMap (extendNameEnv nameMap n . tyThingSemantic) tyThing
            | otherwise = pure nameMap
        lookupImported :: HscEnv -> Name -> IO (Maybe TyThing)
        lookupImported env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env

