{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- |
-- This module provides the core functionality of the plugin.
module Ide.Plugin.SemanticTokens.Internal (semanticTokensFull, getSemanticTokensRule, persistentGetSemanticTokensRule) where

import           Control.Lens                         ((^.))
import           Control.Monad.Except                 (ExceptT, liftEither,
                                                       withExceptT)
import           Control.Monad.Trans                  (lift)
import           Control.Monad.Trans.Except           (runExceptT)
import           Data.Aeson                           (ToJSON (toJSON))
import qualified Data.Map                             as Map
import           Development.IDE                      (Action,
                                                       GetDocMap (GetDocMap),
                                                       GetHieAst (GetHieAst),
                                                       HieAstResult (HAR, hieAst, hieModule, refMap),
                                                       IdeResult, IdeState,
                                                       Priority (..), Recorder,
                                                       Rules, WithPriority,
                                                       cmapWithPrio, define,
                                                       fromNormalizedFilePath,
                                                       hieKind, logPriority,
                                                       usePropertyAction, use_)
import           Development.IDE.Core.PluginUtils     (runActionE,
                                                       useWithStaleE)
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.Rules           (toIdeResult)
import           Development.IDE.Core.RuleTypes       (DocAndTyThingMap (..))
import           Development.IDE.Core.Shake           (addPersistentRule,
                                                       getVirtualFile,
                                                       useWithStale_)
import           Development.IDE.GHC.Compat           hiding (Warning)
import           Development.IDE.GHC.Compat.Util      (mkFastString)
import           Ide.Logger                           (logWith)
import           Ide.Plugin.Error                     (PluginError (PluginInternalError),
                                                       getNormalizedFilePathE,
                                                       handleMaybe,
                                                       handleMaybeM)
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


-----------------------
---- the api
-----------------------

computeSemanticTokens :: Recorder (WithPriority SemanticLog) -> PluginId -> IdeState -> NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeSemanticTokens recorder pid _ nfp = do
  logWith recorder Debug (LogMsg "computeSemanticTokens start")
  config :: SemanticTokensConfig  <- lift $ usePropertyAction #tokenMapping pid semanticConfigProperties
  logWith recorder Debug (LogMsg $ show $ toJSON config)
  logWith recorder Debug (LogConfig config)
  (RangeHsSemanticTokenTypes {rangeSemanticMap}, mapping) <- useWithStaleE GetSemanticTokens nfp
  withExceptT PluginInternalError $ liftEither $ rangeSemanticMapSemanticTokens config mapping rangeSemanticMap

semanticTokensFull :: Recorder (WithPriority SemanticLog) -> PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull recorder state pid param = do
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
  items <- runActionE "SemanticTokens.semanticTokensFull" state $ computeSemanticTokens recorder pid state nfp
  return $ InL items

-- | Defines the 'getSemanticTokensRule' function, compute semantic tokens for a Haskell source file.
--
-- This Rule collects information from various sources, including:
--
-- Imported name token type from Rule 'GetDocMap'
-- Local names token type from 'hieAst'
-- Name locations from 'hieAst'
-- Visible names from 'tmrRenamed'
--
-- It then combines this information to compute the semantic tokens for the file.
getSemanticTokensRule :: Recorder (WithPriority SemanticLog) -> Rules ()
getSemanticTokensRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetSemanticTokens nfp -> handleError recorder $ do
    (HAR {..}) <- lift $ use_ GetHieAst nfp
    (DKMap {getTyThingMap}, _) <- lift $ useWithStale_ GetDocMap nfp
    ast <- handleMaybe (LogNoAST $ show nfp) $ getAsts hieAst Map.!? (HiePath . mkFastString . fromNormalizedFilePath) nfp
    virtualFile <- handleMaybeM LogNoVF $ getVirtualFile nfp
    -- get current location from the old ones
    let spanNamesMap = hieAstSpanNames virtualFile ast
    let names = nameSetElemsStable $ unionNameSets $ Map.elems spanNamesMap
    let localSemanticMap = mkLocalNameSemanticFromAst names (hieKindFunMasksKind hieKind) refMap
    -- get imported name semantic map
    let importedNameSemanticMap = foldr (getTypeExclude localSemanticMap getTyThingMap) emptyNameEnv names
    let sMap = plusNameEnv_C (<>) importedNameSemanticMap localSemanticMap
    let rangeTokenType = extractSemanticTokensFromNames sMap spanNamesMap
    return $ RangeHsSemanticTokenTypes rangeTokenType
  where
    -- ignore one already in discovered in local
    getTypeExclude ::
      NameEnv a ->
      NameEnv TyThing ->
      Name ->
      NameEnv HsSemanticTokenType ->
      NameEnv HsSemanticTokenType
    getTypeExclude localEnv tyThingMap n nameMap
      | n `elemNameEnv` localEnv = nameMap
      | otherwise =
          let tyThing = lookupNameEnv tyThingMap n
           in maybe nameMap (extendNameEnv nameMap n) (tyThing >>= tyThingSemantic)

-- | Persistent rule to ensure that semantic tokens doesn't block on startup
persistentGetSemanticTokensRule :: Rules ()
persistentGetSemanticTokensRule = addPersistentRule GetSemanticTokens $ \_ -> pure $ Just (RangeHsSemanticTokenTypes mempty, idDelta, Nothing)

-- taken from /haskell-language-server/plugins/hls-code-range-plugin/src/Ide/Plugin/CodeRange/Rules.hs

-- | Handle error in 'Action'. Returns an 'IdeResult' with no value and no diagnostics on error. (but writes log)
handleError :: Recorder (WithPriority msg) -> ExceptT msg Action a -> Action (IdeResult a)
handleError recorder action' = do
  valueEither <- runExceptT action'
  case valueEither of
    Left msg -> do
      logWith recorder Warning msg
      pure $ toIdeResult (Left [])
    Right value -> pure $ toIdeResult (Right value)
