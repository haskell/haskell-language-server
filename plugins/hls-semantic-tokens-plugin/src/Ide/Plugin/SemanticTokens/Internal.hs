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

module Ide.Plugin.SemanticTokens.Internal (semanticTokensFull, getSemanticTokensRule, persistentGetSemanticTokensRule) where

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
                                                       useWithStale_, use_)
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

-----------------------
---- the api
-----------------------

computeSemanticTokens :: IdeState -> NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeSemanticTokens state nfp = do
    let dbg = logWith state Debug
    dbg $ "Computing semantic tokens for: " <> show nfp
    (RangeHsSemanticTokenTypes {tokens}, mapping) <- useWithStaleE GetSemanticTokens nfp
    let rangeTokens = mapMaybe (\(span, name) -> (,name) <$> toCurrentRange mapping span) tokens
    withExceptT PluginInternalError $ liftEither $ semanticTokenAbsoluteSemanticTokens rangeTokens

semanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull state _ param = do
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
  items <- runActionE "SemanticTokens.semanticTokensFull" state $ computeSemanticTokens state nfp
  return $ InL items

-- | Defines the 'getSemanticTokensRule' function, compute semantic tokens for a Haskell source file.
--
-- This Rule collects information from various sources, including:
--
-- Imported name token type from Rule 'GetSemanticTokens'
-- Local names token type from 'hieAst'
-- Name locations from 'hieAst'
-- Visible names from 'tmrRenamed'
--
-- It then combines this information to compute the semantic tokens for the file.
--
getSemanticTokensRule :: Recorder (WithPriority Log) -> Rules ()
getSemanticTokensRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetSemanticTokens nfp -> do
    (hscEnv -> hsc) <- use_ GhcSessionDeps nfp
    (HAR {..}) <- use_ GetHieAst nfp
    Just (_, ast) <- return $ listToMaybe $ Map.toList $ getAsts hieAst
    -- get current location from the old ones
    let spanNamesMap = hieAstSpanNames ast
    let nameSet = unionNameSets $ Map.elems spanNamesMap
    -- get imported name semantic map
    importedNameSemanticMap <- liftIO $ foldrM (getTypeExclude hsc) emptyNameEnv $ nameSetElemsStable nameSet
    let sMap = plusNameEnv_C (<>) importedNameSemanticMap $ mkLocalNameSemanticFromAst hieKind refMap
    let rangeTokenType = extractSemanticTokensFromNames sMap spanNamesMap
    return ([], Just $ RangeHsSemanticTokenTypes rangeTokenType)
    where
        -- ignore one already in current module
        getTypeExclude env n nameMap =
                do  tyThing <- lookupImported env n
                    pure $ maybe nameMap (extendNameEnv nameMap n) (tyThing >>= tyThingSemantic)
        lookupImported :: HscEnv -> Name -> IO (Maybe TyThing)
        lookupImported env = fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env


-- | Persistent rule to ensure that semantic tokens doesn't block on startup
persistentGetSemanticTokensRule :: Rules ()
persistentGetSemanticTokensRule = addPersistentRule GetSemanticTokens $ \_ -> pure $ Just (RangeHsSemanticTokenTypes mempty, idDelta, Nothing)
