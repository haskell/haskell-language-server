{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE LiberalTypeSynonyms   #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- |
-- This module provides the core functionality of the plugin.
module Ide.Plugin.SemanticTokens.Internal (semanticTokensFull, getSemanticTokensRule, getSyntacticTokensRule, semanticConfigProperties, semanticTokensFullDelta) where

import           Control.Concurrent.STM                   (stateTVar)
import           Control.Concurrent.STM.Stats             (atomically)
import           Control.Lens                             ((^.))
import           Control.Monad.Except                     (ExceptT, liftEither,
                                                           withExceptT)
import           Control.Monad.IO.Class                   (MonadIO (..))
import           Control.Monad.Trans                      (lift)
import           Control.Monad.Trans.Except               (runExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Data                                (Data (..))
import           Data.List
import qualified Data.Map.Strict                          as M
import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Development.IDE                          (Action,
                                                           GetDocMap (GetDocMap),
                                                           GetHieAst (GetHieAst),
                                                           GetParsedModuleWithComments (..),
                                                           HieAstResult (HAR, hieAst, hieModule, refMap),
                                                           IdeResult, IdeState,
                                                           Priority (..),
                                                           Recorder, Rules,
                                                           WithPriority,
                                                           cmapWithPrio, define,
                                                           hieKind,
                                                           srcSpanToRange,
                                                           useWithStale)
import           Development.IDE.Core.PluginUtils         (runActionE, useE,
                                                           useWithStaleE)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Rules               (toIdeResult)
import           Development.IDE.Core.RuleTypes           (DocAndTyThingMap (..))
import           Development.IDE.Core.Shake               (ShakeExtras (..),
                                                           getShakeExtras,
                                                           getVirtualFile)
import           Development.IDE.GHC.Compat               hiding (Warning)
import           Development.IDE.GHC.Compat.Util          (mkFastString)
import           GHC.Parser.Annotation
import           Ide.Logger                               (logWith)
import           Ide.Plugin.Error                         (PluginError (PluginInternalError, PluginRuleFailed),
                                                           getNormalizedFilePathE,
                                                           handleMaybe,
                                                           handleMaybeM)
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Query
import           Ide.Plugin.SemanticTokens.SemanticConfig (mkSemanticConfigFunctions)
import           Ide.Plugin.SemanticTokens.Tokenize       (computeRangeHsSemanticTokenTypeList)
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message            (MessageResult,
                                                           Method (Method_TextDocumentSemanticTokensFull, Method_TextDocumentSemanticTokensFullDelta))
import           Language.LSP.Protocol.Types              (NormalizedFilePath,
                                                           Range,
                                                           SemanticTokens,
                                                           fromNormalizedFilePath,
                                                           type (|?) (InL, InR))
import           Prelude                                  hiding (span)
import qualified StmContainers.Map                        as STM
import           Type.Reflection                          (Typeable, eqTypeRep,
                                                           pattern App,
                                                           type (:~~:) (HRefl),
                                                           typeOf, typeRep,
                                                           withTypeable)


$mkSemanticConfigFunctions

-----------------------
---- the api
-----------------------

computeSemanticTokens :: Recorder (WithPriority SemanticLog) -> PluginId -> IdeState -> NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeSemanticTokens recorder pid _ nfp = do
  config <- lift $ useSemanticConfigAction pid
  logWith recorder Debug (LogConfig config)
  semanticId <- lift getAndIncreaseSemanticTokensId

  tokenList <- sortOn fst <$> do
    rangesyntacticTypes <- lift $ useWithStale GetSyntacticTokens nfp
    rangesemanticTypes <- lift $ useWithStale GetSemanticTokens nfp
    let mk w u (toks, mapping) = map (\(ran, tok) -> (toCurrentRange mapping ran, w tok)) $ u toks
    maybeToExceptT (PluginRuleFailed "no syntactic nor semantic tokens") $ hoistMaybe $
      (mk HsSyntacticTokenType rangeSyntacticList <$> rangesyntacticTypes)
      <>  (mk HsSemanticTokenType rangeSemanticList <$> rangesemanticTypes)

  -- NOTE: rangeSemanticsSemanticTokens actually assumes that the tokesn are in order. that means they have to be sorted by position
  withExceptT PluginInternalError $ liftEither $ rangeSemanticsSemanticTokens semanticId config tokenList

semanticTokensFull :: Recorder (WithPriority SemanticLog) -> PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
semanticTokensFull recorder state pid param = runActionE "SemanticTokens.semanticTokensFull" state computeSemanticTokensFull
  where
    computeSemanticTokensFull :: ExceptT PluginError Action (MessageResult Method_TextDocumentSemanticTokensFull)
    computeSemanticTokensFull = do
      nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
      items <- computeSemanticTokens recorder pid state nfp
      lift $ setSemanticTokens nfp items
      return $ InL items


semanticTokensFullDelta :: Recorder (WithPriority SemanticLog) -> PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFullDelta
semanticTokensFullDelta recorder state pid param = do
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
  let previousVersionFromParam = param ^. L.previousResultId
  runActionE "SemanticTokens.semanticTokensFullDelta" state $ computeSemanticTokensFullDelta recorder previousVersionFromParam  pid state nfp
  where
    computeSemanticTokensFullDelta :: Recorder (WithPriority SemanticLog) -> Text -> PluginId -> IdeState -> NormalizedFilePath -> ExceptT PluginError Action (MessageResult Method_TextDocumentSemanticTokensFullDelta)
    computeSemanticTokensFullDelta recorder previousVersionFromParam  pid state nfp = do
      semanticTokens <- computeSemanticTokens recorder pid state nfp
      previousSemanticTokensMaybe <- lift $ getPreviousSemanticTokens nfp
      lift $ setSemanticTokens nfp semanticTokens
      case previousSemanticTokensMaybe of
          Nothing -> return $ InL semanticTokens
          Just previousSemanticTokens ->
              if Just previousVersionFromParam == previousSemanticTokens^.L.resultId
              then return $ InR $ InL $ makeSemanticTokensDeltaWithId (semanticTokens^.L.resultId) previousSemanticTokens semanticTokens
              else do
                logWith recorder Warning (LogSemanticTokensDeltaMisMatch previousVersionFromParam (previousSemanticTokens^.L.resultId))
                return $ InL semanticTokens

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
    (HAR {..}) <- withExceptT LogDependencyError $ useE GetHieAst nfp
    (DKMap {getTyThingMap}, _) <- withExceptT LogDependencyError $ useWithStaleE GetDocMap nfp
    ast <- handleMaybe (LogNoAST $ show nfp) $ getAsts hieAst M.!? (HiePath . mkFastString . fromNormalizedFilePath) nfp
    virtualFile <- handleMaybeM LogNoVF $ getVirtualFile nfp
    let hsFinder = idSemantic getTyThingMap (hieKindFunMasksKind hieKind) refMap
    return $ computeRangeHsSemanticTokenTypeList hsFinder virtualFile ast

getSyntacticTokensRule :: Recorder (WithPriority SemanticLog) -> Rules ()
getSyntacticTokensRule recorder =
  define (cmapWithPrio LogShake recorder) $ \GetSyntacticTokens nfp -> handleError recorder $ do
    (parsedModule, _) <- withExceptT LogDependencyError $ useWithStaleE GetParsedModuleWithComments nfp
    pure $ computeRangeHsSyntacticTokenTypeList parsedModule

astTraversalWith :: forall b r. Data b => b -> (forall a. Data a => a -> [r]) -> [r]
astTraversalWith ast f = mconcat $ flip gmapQ ast \y -> f y <> astTraversalWith y f

{-# inline extractTyToTyToTy #-}
extractTyToTyToTy :: forall f a. (Typeable f, Data a) => a -> Maybe (forall r. (forall b c. (Typeable b, Typeable c) => f b c -> r) -> r)
extractTyToTyToTy node
  | App (App conRep argRep1) argRep2 <- typeOf node
  , Just HRefl <- eqTypeRep conRep (typeRep @f)
  = Just $ withTypeable argRep1 $ withTypeable argRep2 \k -> k node
  | otherwise = Nothing

{-# inline extractTyToTy #-}
extractTyToTy :: forall f a. (Typeable f, Data a) => a -> Maybe (forall r. (forall b. Typeable b => f b -> r) -> r)
extractTyToTy node
  | App conRep argRep <- typeOf node
  , Just HRefl <- eqTypeRep conRep (typeRep @f)
  = Just $ withTypeable argRep \k -> k node
  | otherwise = Nothing

{-# inline extractTy #-}
extractTy :: forall b a. (Typeable b, Data a) => a -> Maybe b
extractTy node
  | Just HRefl <- eqTypeRep (typeRep @b) (typeOf node)
  = Just node
  | otherwise = Nothing

computeRangeHsSyntacticTokenTypeList :: ParsedModule -> RangeHsSyntacticTokenTypes
computeRangeHsSyntacticTokenTypeList ParsedModule {pm_parsed_source} =
  let toks = astTraversalWith pm_parsed_source \node -> mconcat
         [
#if MIN_VERSION_ghc(9,9,0)
           maybeToList $ mkFromLocatable TKeyword . (\k -> k \x k' -> k' x) =<< extractTyToTy @EpToken node,
           maybeToList $ mkFromLocatable TKeyword . (\k -> k \x k' -> k' x) =<< extractTyToTyToTy @EpUniToken node,
           do
           AnnContext {ac_darrow, ac_open, ac_close} <- maybeToList $ extractTy node
           let mkFromTok :: (Foldable f, HasSrcSpan a) => f a -> [(Range,HsSyntacticTokenType)]
               mkFromTok = foldMap (\tok -> maybeToList $ mkFromLocatable TKeyword \k -> k tok)
           mconcat
#if MIN_VERSION_ghc(9,11,0)
             [ mkFromTok ac_darrow
#else
             [ foldMap (\(_, loc) -> maybeToList $ mkFromLocatable TKeyword \k -> k loc) ac_darrow
#endif
             , mkFromTok ac_open
             , mkFromTok ac_close
             ],
#endif

#if !MIN_VERSION_ghc(9,11,0)
           maybeToList $ mkFromLocatable TKeyword . (\x k -> k x) =<< extractTy @AddEpAnn node,
           do
           EpAnnImportDecl i p s q pkg a <- maybeToList $ extractTy @EpAnnImportDecl node
           mapMaybe (mkFromLocatable TKeyword . (\x k -> k x)) $ catMaybes $ [Just i, s, q, pkg, a] <> foldMap (\(l, l') -> [Just l, Just l']) p,
#endif
           maybeToList do
             comment <- extractTy @LEpaComment node
#if !MIN_VERSION_ghc(9,7,0)
             -- NOTE: on ghc 9.6 there's an empty comment that is supposed to
             -- located the end of file
             case comment of
               L _ (EpaComment {ac_tok = EpaEofComment}) -> Nothing
               _ -> pure ()
#endif
             mkFromLocatable TComment \k -> k comment,
           do
           L loc expr <- maybeToList $ extractTy @(LHsExpr GhcPs) node
           let fromSimple = maybeToList . flip mkFromLocatable \k -> k loc
           case expr of
             HsOverLabel {} -> fromSimple TStringLit
             HsOverLit _ (OverLit _ lit) -> fromSimple case lit of
               HsIntegral {}   -> TNumberLit
               HsFractional {} -> TNumberLit

               HsIsString {}   -> TStringLit
             HsLit _ lit -> fromSimple case lit of
                 -- NOTE: unfortunately, lsp semantic tokens doesn't have a notion of char literals
                 HsChar {}            -> TStringLit
                 HsCharPrim {}        -> TStringLit

                 HsInt {}             -> TNumberLit
                 HsInteger {}         -> TNumberLit
                 HsIntPrim {}         -> TNumberLit
                 HsWordPrim {}        -> TNumberLit
#if MIN_VERSION_ghc(9,9,0)
                 HsWord8Prim {}       -> TNumberLit
                 HsWord16Prim {}      -> TNumberLit
                 HsWord32Prim {}      -> TNumberLit
#endif
                 HsWord64Prim {}      -> TNumberLit
#if MIN_VERSION_ghc(9,9,0)
                 HsInt8Prim {}        -> TNumberLit
                 HsInt16Prim {}       -> TNumberLit
                 HsInt32Prim {}       -> TNumberLit
#endif
                 HsInt64Prim {}       -> TNumberLit
                 HsFloatPrim {}       -> TNumberLit
                 HsDoublePrim {}      -> TNumberLit
                 HsRat {}             -> TNumberLit

                 HsString {}          -> TStringLit
                 HsStringPrim {}      -> TStringLit
#if MIN_VERSION_ghc(9,11,0)
                 HsMultilineString {} -> TStringLit
#endif
             HsGetField _ _ field -> maybeToList $ mkFromLocatable TRecordSelector \k -> k field
#if MIN_VERSION_ghc(9,11,0)
             HsProjection _ projs -> foldMap (\dotFieldOcc -> maybeToList $ mkFromLocatable TRecordSelector \k -> k dotFieldOcc.dfoLabel) projs
#else
             HsProjection _ projs -> foldMap (\proj -> maybeToList $ mkFromLocatable TRecordSelector \k -> k proj) projs
#endif
             _ -> []
         ]
   in RangeHsSyntacticTokenTypes toks

{-# inline mkFromLocatable #-}
mkFromLocatable
  :: HsSyntacticTokenType
  -> (forall r. (forall a. HasSrcSpan a => a -> r) -> r)
  -> Maybe (Range, HsSyntacticTokenType)
mkFromLocatable tt w = w \tok -> let mrange = srcSpanToRange $ getLoc tok in fmap (, tt) mrange

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

-----------------------
-- helper functions
-----------------------

-- keep track of the semantic tokens response id
-- so that we can compute the delta between two versions
getAndIncreaseSemanticTokensId :: Action SemanticTokenId
getAndIncreaseSemanticTokensId = do
  ShakeExtras{semanticTokensId} <- getShakeExtras
  liftIO $ atomically $ do
    i <- stateTVar semanticTokensId (\val -> (val, val+1))
    return $ T.pack $ show i

getPreviousSemanticTokens :: NormalizedFilePath -> Action (Maybe SemanticTokens)
getPreviousSemanticTokens uri = getShakeExtras >>= liftIO . atomically . STM.lookup uri . semanticTokensCache

setSemanticTokens :: NormalizedFilePath -> SemanticTokens -> Action ()
setSemanticTokens uri tokens = getShakeExtras >>= liftIO . atomically . STM.insert tokens uri . semanticTokensCache
