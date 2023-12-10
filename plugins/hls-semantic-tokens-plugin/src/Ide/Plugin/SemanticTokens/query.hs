{-
    The query module is used to query the semantic tokens from the AST
-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Ide.Plugin.SemanticTokens.Query where
import           Control.Arrow                   ((&&&))
import           Data.Char                       (isAlphaNum)
import           Data.Function                   (on)
import           Data.Generics                   (everything)
import qualified Data.List                       as List
import qualified Data.List.NonEmpty              as NE
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, listToMaybe,
                                                  mapMaybe)
import           Data.Ord                        (comparing)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Lazy.Builder          as Text
import           Development.IDE                 (realSpan)
import           Development.IDE.GHC.Compat
import           Generics.SYB                    (mkQ)
import           Ide.Plugin.SemanticTokens.Types
import           Language.LSP.Protocol.Types




-----------------------------------------
---- construct definition map from HieAST a
-----------------------------------------


-- do not use refMap from useAsts because it may contain ghc generated names or derived names
-- which are not useful for semantic tokens (since they are not in source code)
-- only use identifier both None derived and from source code
identifierGetter :: HieAST a -> RefMap a
identifierGetter ast = Map.unionWith (<>) (getIds ast) (Map.unionsWith (<>) $ map identifierGetter (nodeChildren ast))
    where
        getIds :: HieAST a -> RefMap a
        getIds ast = Map.fromList [ (Right c, [(nodeSpan ast, d)])
                    | (Right c, d) <- Map.toList $ getNodeIds' ast
                    -- at least get one info
                    , let (Just infos) = NE.nonEmpty $ Set.toList $ identInfo d
                    -- , SourceInfo == getSourcedNodeInfo (sourcedNodeInfo ast)
                    , not $ isDerivedOccName (occName c)
                    ]
        getNodeIds' :: HieAST a -> Map.Map Identifier (IdentifierDetails a)
        getNodeIds' = Map.foldl' combineNodeIds Map.empty
            .  Map.filterWithKey (\k _ -> k == SourceInfo)
            . getSourcedNodeInfo . sourcedNodeInfo

        combineNodeIds :: Map.Map Identifier (IdentifierDetails a)
                                -> NodeInfo a -> Map.Map Identifier (IdentifierDetails a)
        -- ad `combineNodeIds` (NodeInfo SourceInfo _ bd) = bd
        ad `combineNodeIds` (NodeInfo _ _ bd) = Map.unionWith (<>) ad bd



constructIdentifierMap :: RefMap a -> NameTokenTypeMap
constructIdentifierMap =
        Map.mapWithKey (\k vs ->
            let semanticToken = foldl (<>) (identifierTokenType k) $ mapMaybe (collectToken . snd) vs
            in (map fst vs, semanticToken))
    where
        collectToken :: IdentifierDetails a -> Maybe SemanticTokenType
        collectToken details = case NE.nonEmpty $ Set.toList $ identInfo details of
            Just infos -> Just $ List.maximum $ NE.map infoTokenType infos
            Nothing    -> Nothing

        infoTokenType :: ContextInfo -> SemanticTokenType
        infoTokenType x = case x of
            Use                   -> TNothing
            MatchBind             -> TNothing
            IEThing _             -> TNothing -- todo find a way to get imported name
            TyDecl                -> TNothing -- type signature

            ValBind bt _ _        -> TValBind
            PatternBind _ _ _     -> TPatternBind
            ClassTyDecl _         -> TClassMethod
            TyVarBind _ _         -> TTypeVariable
            RecField _ _          -> TRecField
            -- data constructor, type constructor, type synonym, type family
            Decl ClassDec _       -> TClass
            Decl DataDec  _       -> TTypeCon
            Decl ConDec   _       -> TDataCon
            Decl SynDec   _       -> TTypeSyn
            Decl FamDec   _       -> TTypeFamily
            -- instance dec is class method
            Decl InstDec  _       -> TClassMethod
            Decl PatSynDec _      -> TPatternSyn
            EvidenceVarUse        -> TNothing
            EvidenceVarBind _ _ _ -> TNothing

-----------------------------------------
---- collect all names from RenamedSource
-----------------------------------------


nameToCollect :: LIdP GhcRn -> [Name]
nameToCollect locName = [unLoc locName]

nameGetter :: RenamedSource -> [Name]
nameGetter = everything (++) ([] `mkQ` nameToCollect)


-----------------------------------
-- extract semantic tokens from ast
-----------------------------------


extractSemanticTokens :: forall a. HieAST a -> RenamedSource -> Either Text SemanticTokens
extractSemanticTokens ast rs = makeSemanticTokens defaultSemanticTokensLegend
    $ List.sort $ Map.foldMapWithKey toAbsSemanticToken $ identifierToSemanticMap iMap
    where
          ids = identifierGetter ast
          iMap = constructIdentifierMap ids
          -- refine the semantic token type
          identifierToSemanticMap :: NameTokenTypeMap -> Map.Map Span SemanticTokenType
          identifierToSemanticMap nameMap = Map.fromList
                [ (span, tokenType)
                | (spans, tokenType) <- Map.elems nameMap
                   , span <- spans]

            -- toAbsSemanticToken :: Span -> SemanticTokenType -> SemanticTokenAbsolute
          toAbsSemanticToken loc tokenType =
                let line = srcSpanStartLine loc - 1
                    startChar = srcSpanStartCol loc - 1
                    len = srcSpanEndCol loc - 1 - startChar
                in return $ SemanticTokenAbsolute (fromIntegral line) (fromIntegral startChar)
                    (fromIntegral len) (toLspTokenType tokenType) [SemanticTokenModifiers_Declaration]

