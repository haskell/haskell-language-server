{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE StrictData               #-}
{-# LANGUAGE TypeFamilies             #-}

module Ide.Plugin.DocumentLink (descriptor, Log(..)) where

import           Control.DeepSeq                  (NFData)
import           Control.Monad.Trans.Maybe        (MaybeT (runMaybeT),
                                                   hoistMaybe)
import           Data.Hashable                    (Hashable)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (maybeToList)
import           Development.IDE                  (DocAndTyThingMap (DKMap, getDocMap),
                                                   GetDocMap (GetDocMap),
                                                   GetHieAst (GetHieAst),
                                                   HieAstResult (HAR, hieAst),
                                                   IdeState (shakeExtras),
                                                   Pretty (pretty), Range,
                                                   Recorder, RuleResult, Rules,
                                                   Uri (Uri), WithPriority,
                                                   cmapWithPrio,
                                                   defineNoDiagnostics,
                                                   fromNormalizedFilePath,
                                                   realSrcSpanToRange)
import           Development.IDE.Core.PluginUtils (runIdeActionE, useMT,
                                                   useWithStaleFastE)
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.GHC.Compat       (lookupNameEnv)
import           Development.IDE.GHC.Compat.Util  (mkFastString)
import           Development.IDE.Spans.Common     (DocMap,
                                                   SpanDoc (SpanDocString, SpanDocText),
                                                   spanDocUriDoc)
import           GHC.Generics                     (Generic)
import           GHC.Iface.Ext.Types              (HieAST (nodeChildren, nodeSpan, sourcedNodeInfo),
                                                   HieASTs (getAsts),
                                                   Identifier,
                                                   NodeInfo (nodeIdentifiers),
                                                   NodeOrigin (SourceInfo),
                                                   SourcedNodeInfo (getSourcedNodeInfo),
                                                   Span, pattern HiePath)
import           Ide.Plugin.Error                 (getNormalizedFilePathE)
import           Ide.Types                        (PluginDescriptor (pluginHandlers),
                                                   PluginId,
                                                   PluginMethodHandler,
                                                   defaultPluginDescriptor,
                                                   mkPluginHandler, pluginRules)
import           Language.LSP.Protocol.Message    (Method (Method_TextDocumentDocumentLink),
                                                   SMethod (SMethod_TextDocumentDocumentLink))
import           Language.LSP.Protocol.Types      (DocumentLink (..),
                                                   DocumentLinkParams (DocumentLinkParams),
                                                   TextDocumentIdentifier (TextDocumentIdentifier),
                                                   type (|?) (InL))

newtype Log = LogShake Shake.Log

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId =
  (defaultPluginDescriptor pluginId "Provide document link of symbols")
    { Ide.Types.pluginHandlers = mkPluginHandler SMethod_TextDocumentDocumentLink documentLinkProvider,
      Ide.Types.pluginRules    = getDocumentLinkRule recorder
    }

documentLinkProvider :: PluginMethodHandler IdeState Method_TextDocumentDocumentLink
documentLinkProvider ideState _pluginId (DocumentLinkParams _ _ (TextDocumentIdentifier uri)) = do
  nfp <- getNormalizedFilePathE uri
  ((DocumentLinks uris), _pm) <- runIdeActionE "DocumentLink" (shakeExtras ideState) $ useWithStaleFastE GetDocumentLinks nfp
  pure $ InL (map mkDocumentLink uris)

mkDocumentLink :: (Range, Uri) -> DocumentLink
mkDocumentLink (range, target) =
  DocumentLink
    { _range = range,
      _target = Just target,
      _tooltip = Nothing,
      _data_ = Nothing
    }

data GetDocumentLinks = GetDocumentLinks
  deriving (Eq, Show, Generic)

instance Hashable GetDocumentLinks

instance NFData GetDocumentLinks

newtype DocumentLinks = DocumentLinks [(Range, Uri)]
  deriving (Show, NFData)

type instance RuleResult GetDocumentLinks = DocumentLinks

getDocumentLinkRule :: Recorder (WithPriority Log) -> Rules ()
getDocumentLinkRule recorder =
  defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \GetDocumentLinks nfp -> runMaybeT $ do
    HAR {hieAst} <- useMT GetHieAst nfp
    DKMap {getDocMap} <- useMT GetDocMap nfp
    ast <- hoistMaybe $ getAsts hieAst Map.!? HiePath (mkFastString $ fromNormalizedFilePath nfp)
    let lookup = lookupDoc getDocMap
    pure $ DocumentLinks (foldAst lookup ast)

-- | Recursively traverses the HieAST in depth-first order to collect information
-- from leaf nodes. For each leaf, it extracts all identifiers and their source
-- spans, applies the lookup function, and aggregates the results using the
-- Monoid instance.
foldAst :: forall a t. Monoid a => ((Identifier, Span) -> a) -> HieAST t -> a
foldAst lookup ast = case nodeChildren ast of
  []   -> visitLeaf ast
  asts -> foldMap (foldAst lookup) asts
  where
    visitLeaf leaf =
      let span = nodeSpan leaf
          mNodeInfo = Map.lookup SourceInfo $ getSourcedNodeInfo (sourcedNodeInfo leaf)
      in flip foldMap mNodeInfo $ \nodeInfo ->
           foldMap (\ident -> lookup (ident, span)) (Map.keys $ nodeIdentifiers nodeInfo)

lookupDoc :: DocMap -> (Identifier, Span) -> [(Range, Uri)]
lookupDoc dm (identifier, span) = do
  Right name <- [identifier]
  doc <- maybeToList $ lookupNameEnv dm name
  uris <- maybeToList $ spanDocUriDoc $ case doc of
        SpanDocString _ uris -> uris
        SpanDocText _ uris   -> uris
  pure (realSrcSpanToRange span, Uri uris)
