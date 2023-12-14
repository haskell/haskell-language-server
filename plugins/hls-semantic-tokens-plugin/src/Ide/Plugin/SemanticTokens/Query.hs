{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}
module Ide.Plugin.SemanticTokens.Query where
import           Control.Arrow                       (ArrowChoice ((|||)),
                                                      (&&&))
import           Control.Monad                       (forM)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Char                           (isAlphaNum)
import           Data.Function                       (on)
import           Data.Generics                       (everything)
import qualified Data.List                           as List
import qualified Data.List.NonEmpty                  as NE
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (catMaybes, fromMaybe,
                                                      listToMaybe, mapMaybe)
import           Data.Ord                            (comparing)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Lazy.Builder              as Text
import           Development.IDE                     (Action, filePathToUri',
                                                      rangeToRealSrcSpan,
                                                      realSpan)
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint       (FOIReferences)
import           Development.IDE.Types.Shake         (WithHieDb)
import           Generics.SYB                        (mkQ)
import           HieDb                               (DefRow (..),
                                                      HieDbErr (AmbiguousUnitId, NameNotFound, NameUnhelpfulSpan, NoNameAtPoint, NotIndexed),
                                                      ModuleInfo (modInfoSrcFile),
                                                      RefRow (..), Res,
                                                      findOneDef,
                                                      findReferences,
                                                      type (:.) (..))
import           Ide.Plugin.SemanticTokens.Types
import           Language.LSP.Protocol.Types
-- import HieDb.Types (DefRow (..))
import           Data.Either                         (rights)
import qualified Data.HashSet                        as HashSet
import           Data.Tuple                          (swap)
import           Debug.Trace
import           Development.IDE.GHC.Error           (positionToRealSrcLoc)
import           Development.IDE.Spans.LocalBindings (Bindings)
import           Development.IDE.Types.Exports       (ExportsMap (getModuleExportsMap),
                                                      IdentInfo)
import           Development.IDE.Types.Location      (toNormalizedFilePath')





nameToCollect :: Name -> Set.Set Name
nameToCollect name = Set.singleton name

nameGetter :: RenamedSource -> Set.Set Name
nameGetter =  everything Set.union (Set.empty `mkQ` nameToCollect)

-----------------------------------------
---- construct definition map from HieAST a
-----------------------------------------
hieAstSpanNames :: HieAST a -> [(Span, Name)]
hieAstSpanNames ast = if null (nodeChildren ast) then
    getIds ast else concatMap hieAstSpanNames (nodeChildren ast)
    where
        getIds :: HieAST a -> [(Span, Name)]
        getIds ast = [(nodeSpan ast, c)
                    | (Right c, d) <- Map.toList $ getNodeIds' ast
                    -- at least get one info
                    , not $ any isEvidenceBind $ identInfo d
                    , not $ any isEvidenceUse $ identInfo d
                    , Set.size (identInfo d) > 0
                    -- , not $ isDerivedOccName (occName c)
                    ]
        -- getNodeIds'' :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
        -- getNodeIds'' = maybeToList . getSourcedNodeInfo . sourcedNodeInfo
        getNodeIds' :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
        getNodeIds' = Map.foldl' combineNodeIds Map.empty
            .  Map.filterWithKey (\k _ -> k == SourceInfo)
            . getSourcedNodeInfo . sourcedNodeInfo

        combineNodeIds :: Map.Map Identifier (IdentifierDetails a)
                                -> NodeInfo a -> Map.Map Identifier (IdentifierDetails a)
        -- ad `combineNodeIds` (NodeInfo SourceInfo _ bd) = bd
        ad `combineNodeIds` (NodeInfo _ _ bd) = Map.unionWith (<>) ad bd




infoTokenType :: ContextInfo -> SemanticTokenType
infoTokenType x = case x of
    Use                      -> TNothing
    MatchBind                -> TNothing
    IEThing _                -> TNothing
    TyDecl                   -> TNothing -- type signature

    ValBind RegularBind _ _  -> TValBind
    ValBind InstanceBind _ _ -> TClassMethod
    PatternBind _ _ _        -> TPatternBind
    ClassTyDecl _            -> TClassMethod
    TyVarBind _ _            -> TTypeVariable
    RecField _ _             -> TRecField
    -- data constructor, type constructor, type synonym, type family
    Decl ClassDec _          -> TClass
    Decl DataDec  _          -> TTypeCon
    Decl ConDec   _          -> TDataCon
    Decl SynDec   _          -> TTypeSyn
    Decl FamDec   _          -> TTypeFamily
    -- instance dec is class method
    Decl InstDec  _          -> TClassMethod
    Decl PatSynDec _         -> TPatternSyn

    EvidenceVarUse           -> TNothing
    EvidenceVarBind _ _ _    -> TNothing


type NameSemanticMap = Map Name SemanticTokenType
-----------------------------------
-- extract semantic tokens from ast
-----------------------------------

toNameSemanticMap :: RefMap a -> NameSemanticMap
toNameSemanticMap rm = Map.fromListWith (<>)
    [
    --  trace ("toNameSemanticMap" <> ":" <> showSDocUnsafe (ppr name) <> " : " <> printCompactRealSrc span <> ":" <> showIdentifierDetails detail <> " : " <> show tokenType)
     (name, tokenType)
    | (Right name, details) <- Map.toList rm
    , not $ isDerivedOccName (occName name)
    , (span, detail) <- details
    , let tokenType =  detailSemanticMaybeTokenType $ identInfo detail
    , (Just tokenType) <- [tokenType]
    ]
    where
        detailSemanticMaybeTokenType ::  Set.Set ContextInfo -> Maybe SemanticTokenType
        detailSemanticMaybeTokenType details = case NE.nonEmpty $ Set.toList details of
            Just infos -> Just $ maximum $ NE.map infoTokenType infos
            Nothing    -> Nothing

semanticTokenAbsoluteSemanticTokens :: [SemanticTokenAbsolute] -> Either Text SemanticTokens
semanticTokenAbsoluteSemanticTokens xs = makeSemanticTokens defaultSemanticTokensLegend . List.sort $ xs

extractSemanticTokensFromNames :: NameSemanticMap -> [(Span, Name)] -> [SemanticTokenAbsolute]
extractSemanticTokensFromNames nsm =
    map (uncurry toAbsSemanticToken) . mergeNameFromSamSpan . mapMaybe (getSemantic nsm)
    where
        -- merge all tokens with same span
        mergeNameFromSamSpan :: [(Span, SemanticTokenType)] -> [(Span, SemanticTokenType)]
        mergeNameFromSamSpan xs = Map.toList $ Map.fromListWith (<>) xs


toAbsSemanticToken :: Span -> SemanticTokenType -> SemanticTokenAbsolute
toAbsSemanticToken loc tokenType =
    let line = srcSpanStartLine loc - 1
        startChar = srcSpanStartCol loc - 1
        len = srcSpanEndCol loc - 1 - startChar
    in SemanticTokenAbsolute (fromIntegral line) (fromIntegral startChar)
        (fromIntegral len) (toLspTokenType tokenType) [SemanticTokenModifiers_Declaration]

getSemantic :: Map Name SemanticTokenType -> (Span, Name) -> Maybe (Span, SemanticTokenType)
getSemantic nameMap (span, name) = do
    -- let tkt = toTokenType name
    -- let tokenType = maybe tkt (\x -> tkt <> x) $ Map.lookup name nameMap
    let tokenType = Map.lookup name nameMap
    fmap (span,) tokenType

-- this one might contain derived names
hieAstNameSet :: HieAST a -> Set.Set Name
hieAstNameSet ast = Set.fromList importedNames
    where locatedNames = hieAstSpanNames ast
          importedNames = [name | (_, name) <- locatedNames]


