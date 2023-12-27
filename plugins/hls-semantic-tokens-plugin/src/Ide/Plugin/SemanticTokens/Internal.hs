-----------------------------------------------------------------------------
-- |
-- This module provides the core functionality of the plugin.
--
-----------------------------------------------------------------------------

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

module Ide.Plugin.SemanticTokens.Internal (semanticTokensFull, getImportedNameSemanticRule, persistentSemanticMapRule) where

import           Control.Lens                         ((^.))
import           Control.Monad.Except                 (ExceptT, MonadError (..),
                                                       liftEither, withExceptT)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Either                          (fromRight)
import qualified Data.Map                             as Map
import           Data.Maybe                           (listToMaybe, mapMaybe)
import qualified Data.Text                            as T
import           Development.IDE                      (Action,
                                                       GetHieAst (GetHieAst),
                                                       GhcSessionDeps (GhcSessionDeps),
                                                       HieAstResult (HAR, hieAst, hieModule, refMap),
                                                       IdeState, Priority (..),
                                                       Recorder, Rules,
                                                       TypeCheck (TypeCheck),
                                                       WithPriority,
                                                       catchSrcErrors,
                                                       cmapWithPrio, define,
                                                       hieKind, ideLogger,
                                                       logPriority,
                                                       useWithStale_)
import           Development.IDE.Core.Compile         (TcModuleResult (..),
                                                       lookupName)
import           Development.IDE.Core.PluginUtils     (runActionE,
                                                       useWithStaleE)
import           Development.IDE.Core.PositionMapping (idDelta, toCurrentRange)
import           Development.IDE.Core.Rules           (Log (LogShake))
import           Development.IDE.Core.Shake           (addPersistentRule)
import           Development.IDE.GHC.Compat
import           Development.IDE.Types.HscEnvEq       (hscEnv)
import           Ide.Plugin.Error                     (PluginError (PluginInternalError, PluginRuleFailed),
                                                       getNormalizedFilePathE)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        (Method (Method_TextDocumentSemanticTokensFull))
import           Language.LSP.Protocol.Types          (NormalizedFilePath,
                                                       SemanticTokens,
                                                       type (|?) (InL))
import           Prelude                              hiding (span)

logWith :: (MonadIO m) => IdeState -> Priority -> String -> m ()
logWith st prior = liftIO . logPriority (ideLogger st) prior . T.pack

liftMaybeEither :: (MonadError e m) => e -> Maybe a -> m a
liftMaybeEither e = liftEither . maybe (Left e) Right

-----------------------
---- the api
-----------------------

-- | Compute semantic tokens for a Haskell source file.
--
-- This function collects information from various sources, including:
--
-- Imported name token type from Rule 'GetGlobalNameSemantic'
-- Local names token type from 'hieAst'
-- Name locations from 'hieAst'
-- Visible names from 'tmrRenamed'
--
-- It then combines this information to compute the semantic tokens for the file.
--
computeSemanticTokens :: IdeState -> NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeSemanticTokens state nfp = do
    let dbg = logWith state Debug
    dbg $ "Computing semantic tokens for: " <> show nfp
    (HAR {..}, mapping) <- useWithStaleE GetHieAst nfp
    (TcModuleResult {..}, _) <- useWithStaleE TypeCheck nfp
    (GTTMap {importedNameSemanticMap}, _) <- useWithStaleE GetGlobalNameSemantic nfp
    (_, ast) <-
        liftMaybeEither
          (PluginRuleFailed $ "hieAst does not contains ast for:" <> T.pack (show nfp))
        $ listToMaybe
        $ Map.toList
        $ getAsts hieAst
    let nameSet = nameGetter tmrRenamed
    -- get current location from the old ones
    let spanNames = mapMaybe (\(span, name) -> (,name) <$> toCurrentRange mapping span) $ hieAstSpanNames nameSet ast
    let sMap = plusNameEnv_C (<>) importedNameSemanticMap $ mkLocalNameSemanticFromAst hieKind refMap
    let moduleAbsTks = extractSemanticTokensFromNames sMap spanNames
    withExceptT PluginInternalError $ liftEither $ semanticTokenAbsoluteSemanticTokens moduleAbsTks

semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
  items <- runActionE "SemanticTokens.semanticTokensFull" state $ computeSemanticTokens state nfp
  return $ InL items

-- | Defines the 'getImportedNameSemanticRule' function, which is used to record semantic tokens for imported names.
getImportedNameSemanticRule :: Recorder (WithPriority Log) -> Rules ()
getImportedNameSemanticRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetGlobalNameSemantic nfp -> do
    (TcModuleResult {..}, _) <- useWithStale_ TypeCheck nfp
    (hscEnv -> hsc, _) <- useWithStale_ GhcSessionDeps nfp
    let nameSet = nameGetter tmrRenamed
    tm <- liftIO $ foldrM (getTypeExclude (tcg_type_env tmrTypechecked) hsc) emptyNameEnv $ nameSetElemsStable nameSet
    return ([], Just $ GTTMap tm)
    where
        -- ignore one already in current module
        getTypeExclude localMap env n nameMap
            | Nothing <- lookupNameEnv localMap n =
                do  tyThing <- lookupImported env n
                    pure $ maybe nameMap (extendNameEnv nameMap n . tyThingSemantic) tyThing
            | otherwise = pure nameMap
        lookupImported :: HscEnv -> Name -> IO (Maybe TyThing)
        lookupImported env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env


-- | Persistent rule to ensure that semantic tokens doesn't block on startup
persistentSemanticMapRule :: Rules ()
persistentSemanticMapRule = addPersistentRule GetGlobalNameSemantic $ \_ -> pure $ Just (GTTMap mempty, idDelta, Nothing)
