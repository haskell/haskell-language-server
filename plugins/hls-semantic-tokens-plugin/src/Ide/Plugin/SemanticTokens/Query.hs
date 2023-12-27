{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ide.Plugin.SemanticTokens.Query where
import           Control.Arrow                       (ArrowChoice ((|||)),
                                                      (&&&))
import           Control.Monad                       (foldM, forM)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Char                           (isAlphaNum)
import           Data.Either                         (rights)
import           Data.Function                       (on)
import           Data.Generics                       (everything)
import qualified Data.HashSet                        as HashSet
import qualified Data.List                           as List
import qualified Data.List.NonEmpty                  as NE
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (catMaybes, fromMaybe,
                                                      listToMaybe, mapMaybe,
                                                      maybeToList)
import           Data.Ord                            (comparing)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Lazy.Builder              as Text
import           Data.Tuple                          (swap)
import           Debug.Trace
import           Development.IDE                     (Action, HieKind,
                                                      filePathToUri',
                                                      rangeToRealSrcSpan,
                                                      realSpan,
                                                      realSrcSpanToRange)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error           (positionToRealSrcLoc)
import           Development.IDE.Spans.AtPoint       (FOIReferences)
import           Development.IDE.Spans.LocalBindings (Bindings)
import           Development.IDE.Types.Exports       (ExportsMap (getModuleExportsMap),
                                                      IdentInfo)
import           Development.IDE.Types.Location      (toNormalizedFilePath')
import           Development.IDE.Types.Shake         (WithHieDb)
import           Generics.SYB                        (mkQ)
-- import HieDb.Types (DefRow (..))
import qualified Data.Map                            as M
import           Data.Monoid                         (First (..))
import qualified Data.Set                            as S
import           HieDb                               (DefRow (..),
                                                      HieDbErr (AmbiguousUnitId, NameNotFound, NameUnhelpfulSpan, NoNameAtPoint, NotIndexed),
                                                      ModuleInfo (modInfoSrcFile),
                                                      RefRow (..), Res,
                                                      findOneDef,
                                                      findReferences,
                                                      type (:.) (..))
import           Ide.Plugin.SemanticTokens.Mappings
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Plugin.SemanticTokens.Utils     (showName)
import           Language.LSP.Protocol.Types


------------------------------------------------
--- | extract semantic tokens from RenamedSource
------------------------------------------------

nameToCollect :: Name -> NameSet
nameToCollect = unitNameSet

nameGetter :: RenamedSource -> NameSet
nameGetter =  everything unionNameSet (emptyNameSet `mkQ` nameToCollect)

--------------------------------------------------
--- | extract semantic map from HieAst for local variables
--------------------------------------------------


mkLocalNameSemanticFromAst :: HieKind a -> HieAST a -> RefMap a -> NameSemanticMap
mkLocalNameSemanticFromAst hieKind ast rm = mkNameEnv (mapMaybe (nameNameSemanticFromHie hieKind ast rm) ns)
    where ns = rights $ M.keys rm
nameNameSemanticFromHie :: forall a. HieKind a -> HieAST a -> RefMap a -> Name -> Maybe (Name,SemanticTokenType)
nameNameSemanticFromHie hieKind hie rm ns = do
    st <- -- traceShow ("to find Name", showName ns) $
        nameSemanticFromRefMap rm ns
    return -- $ traceShow (showName ns, st)
           (ns, st)
    where
        nameSemanticFromRefMap :: RefMap a -> Name -> Maybe SemanticTokenType
        nameSemanticFromRefMap rm name = do
            let nameString = showName name
            spanInfos <- -- traceShow ("getting spans:", nameString) $
                 Map.lookup (Right name) rm
            let infos = S.unions $ map (identInfo . snd) spanInfos
            let typeTokenType = fmap (typeSemantic hieKind) $ listToMaybe $ mapMaybe (identType . snd) spanInfos
            let contextInfoTokenType = contextInfosMaybeTokenType infos
            maximum <$> NE.nonEmpty (catMaybes [typeTokenType, contextInfoTokenType])

        contextInfosMaybeTokenType :: Set.Set ContextInfo -> Maybe SemanticTokenType
        contextInfosMaybeTokenType details = case NE.nonEmpty $ Set.toList details of
            Just infos -> Just $ maximum $ NE.map infoTokenType infos
            Nothing    -> Nothing

--------------------------------------------------
--- | extract location from HieAST a
--------------------------------------------------

hieAstSpanNames :: UniqSet Name -> HieAST a -> [(Range, Name)]
hieAstSpanNames nameSet ast = if null (nodeChildren ast) then
    getIds ast else concatMap (hieAstSpanNames nameSet) (nodeChildren ast)
    where
        -- getIds :: HieAST a -> [(Span, Name)]
        getIds ast = [(realSrcSpanToRange $ nodeSpan ast, c)
                    | (Right c, d) <- Map.toList $ getNodeIds' ast
                    , elemNameSet c nameSet
                    -- at least get one info
                    , not $ any isEvidenceBind $ identInfo d
                    , not $ any isEvidenceUse $ identInfo d
                    , Set.size (identInfo d) > 0
                    -- some derived occName is visible (some fields)
                    -- , not $ isDerivedOccName (occName c)
                    ]
        getNodeIds' :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
        getNodeIds' = Map.foldl' combineNodeIds Map.empty
            .  Map.filterWithKey (\k _ -> k == SourceInfo)
            . getSourcedNodeInfo . sourcedNodeInfo

        combineNodeIds :: Map.Map Identifier (IdentifierDetails a)
                                -> NodeInfo a -> Map.Map Identifier (IdentifierDetails a)
        ad `combineNodeIds` (NodeInfo _ _ bd) = Map.unionWith (<>) ad bd


--------------------------------------------------
--- | extract semantic tokens from NameSemanticMap
--------------------------------------------------

semanticTokenAbsoluteSemanticTokens :: [SemanticTokenAbsolute] -> Either Text SemanticTokens
semanticTokenAbsoluteSemanticTokens = makeSemanticTokens defaultSemanticTokensLegend . List.sort

-- extractSemanticTokensFromNames :: NameSemanticMap -> [(Span, Name)] -> [SemanticTokenAbsolute]
extractSemanticTokensFromNames :: UniqFM Name SemanticTokenType -> [(Range, Name)] -> [SemanticTokenAbsolute]
extractSemanticTokensFromNames nsm =
    mapMaybe (uncurry toAbsSemanticToken) . mergeNameFromSamSpan . mapMaybe (getSemantic nsm)
    where
        -- merge all tokens with same span
        -- mergeNameFromSamSpan :: [(Span, SemanticTokenType)] -> [(Span, SemanticTokenType)]
        mergeNameFromSamSpan xs = Map.toList $ Map.fromListWith (<>) xs

        toAbsSemanticToken :: Range -> SemanticTokenType -> Maybe SemanticTokenAbsolute
        toAbsSemanticToken (Range (Position startLine startColumn) (Position endLine endColumn)) tokenType =
            let len = endColumn - startColumn
            in SemanticTokenAbsolute (fromIntegral startLine) (fromIntegral startColumn)
                (fromIntegral len) <$> toLspTokenType tokenType <*> return []
        getSemantic nameMap (span, name) = do
            let tokenType = lookupNameEnv nameMap name
            fmap (span,) tokenType



