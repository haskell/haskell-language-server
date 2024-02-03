{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- |
-- This module provides the core functionality of the plugin.
module Ide.Plugin.SemanticTokens.Internal (semanticTokensFull, getSemanticTokensRule, persistentGetSemanticTokensRule, semanticConfigProperties) where

import           Control.Lens                             ((^.))
import           Control.Monad.Except                     (ExceptT, liftEither,
                                                           withExceptT)
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Trans.Except               (runExceptT)
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as M
import qualified Data.Set                                 as S
import           Development.IDE                          (Action,
                                                           GetDocMap (GetDocMap),
                                                           GetHieAst (GetHieAst),
                                                           HieAstResult (HAR, hieAst, hieModule, refMap),
                                                           IdeResult, IdeState,
                                                           Priority (..),
                                                           Recorder, Rules,
                                                           WithPriority,
                                                           cmapWithPrio, define,
                                                           fromNormalizedFilePath,
                                                           hieKind, logPriority,
                                                           use_)
import           Development.IDE.Core.PluginUtils         (runActionE,
                                                           useWithStaleE)
import           Development.IDE.Core.PositionMapping     (idDelta)
import           Development.IDE.Core.Rules               (toIdeResult)
import           Development.IDE.Core.RuleTypes           (DocAndTyThingMap (..))
import           Development.IDE.Core.Shake               (addPersistentRule,
                                                           getVirtualFile,
                                                           useWithStale_)
import           Development.IDE.GHC.Compat               hiding (Warning)
import           Development.IDE.GHC.Compat.Util          (mkFastString)
import           Ide.Logger                               (logWith)
import           Ide.Plugin.Error                         (PluginError (PluginInternalError),
                                                           getNormalizedFilePathE,
                                                           handleMaybe,
                                                           handleMaybeM)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.SemanticConfig (mkSemanticConfigFunctions)
import           Ide.Plugin.SemanticTokens.Tokenize       (hieAstSpanIdentifiers)
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message            (Method (Method_TextDocumentSemanticTokensFull))
import           Language.LSP.Protocol.Types              (NormalizedFilePath,
                                                           SemanticTokens,
                                                           type (|?) (InL))
import           Prelude                                  hiding (span)


$mkSemanticConfigFunctions

-----------------------
---- the api
-----------------------

computeSemanticTokens :: Recorder (WithPriority SemanticLog) -> PluginId -> IdeState -> NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeSemanticTokens recorder pid _ nfp = do
  config <- lift $ useSemanticConfigAction pid
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
    ast <- handleMaybe (LogNoAST $ show nfp) $ getAsts hieAst M.!? (HiePath . mkFastString . fromNormalizedFilePath) nfp
    virtualFile <- handleMaybeM LogNoVF $ getVirtualFile nfp
    -- get current location from the old ones
    let spanIdMap = M.filter (not . null) $ hieAstSpanIdentifiers virtualFile ast
    let names = S.unions $ M.elems spanIdMap
    let localSemanticMap = mkLocalIdSemanticFromAst names (hieKindFunMasksKind hieKind) refMap
    -- get imported name semantic map
    let importedIdSemanticMap = M.mapMaybe id
            $ M.fromSet (getTypeThing getTyThingMap) (names `S.difference` M.keysSet localSemanticMap)
    let sMap = M.unionWith (<>) importedIdSemanticMap localSemanticMap
    let rangeTokenType = extractSemanticTokensFromNames sMap spanIdMap
    return $ RangeHsSemanticTokenTypes rangeTokenType
  where
    getTypeThing ::
      NameEnv TyThing ->
      Identifier ->
      Maybe HsSemanticTokenType
    getTypeThing tyThingMap n
      | (Right name) <- n =
          let tyThing = lookupNameEnv tyThingMap name
           in (tyThing >>= tyThingSemantic)
     | otherwise = Nothing

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
