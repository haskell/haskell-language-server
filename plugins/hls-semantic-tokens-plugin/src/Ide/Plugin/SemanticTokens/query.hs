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
import           Control.Arrow                       ((&&&))
import           Control.Monad                       (forM)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Char                           (isAlphaNum)
import           Data.Function                       (on)
import           Data.Generics                       (everything)
import qualified Data.List                           as List
import qualified Data.List.NonEmpty                  as NE
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (catMaybes, listToMaybe,
                                                      mapMaybe)
import           Data.Ord                            (comparing)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import qualified Data.Text.Lazy.Builder              as Text
import           Development.IDE                     (Action,
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
import           Development.IDE                     (filePathToUri')
import           Development.IDE.GHC.Error           (positionToRealSrcLoc)
import           Development.IDE.Spans.LocalBindings (Bindings)
import           Development.IDE.Types.Exports       (ExportsMap (getModuleExportsMap),
                                                      IdentInfo)
import           Development.IDE.Types.Location      (toNormalizedFilePath')




-----------------------------------------
---- construct definition map from HieAST a
-----------------------------------------
-- do not use refMap from useAsts to get identifiers
-- because it may contain ghc generated names or derived names
-- which are not useful for semantic tokens (since they are not in source code)
-- only use identifier both None derived and from source code
identifierGetter :: HieAST a -> [(Name, Span)]
identifierGetter ast = if null (nodeChildren ast) then
    getIds ast else concatMap identifierGetter (nodeChildren ast)
    where
        getIds :: HieAST a -> [(Name, Span)]
        getIds ast = [(c, nodeSpan ast)
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



detailSemanticMaybeTokenType :: IdentifierDetails a -> Maybe SemanticTokenType
detailSemanticMaybeTokenType details = case NE.nonEmpty $ Set.toList $ identInfo details of
    Just infos -> Just $ List.maximum $ NE.map infoTokenType infos
    Nothing    -> Nothing

infoTokenType :: ContextInfo -> SemanticTokenType
infoTokenType x = case x of
    Use                      -> TNothing
    MatchBind                -> TNothing
    IEThing _                -> TNothing -- todo find a way to get imported name
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
    [ (name, tokenType)
    | (Right name, details) <- Map.toList rm
    , (_, detail) <- details
    , let tokenType = detailSemanticMaybeTokenType detail
    , (Just tokenType) <- [tokenType]
    ]
semanticTokenAbsoluteSemanticTokens :: [SemanticTokenAbsolute] -> Either Text SemanticTokens
semanticTokenAbsoluteSemanticTokens xs = makeSemanticTokens defaultSemanticTokensLegend . List.sort $ xs

extractSemanticTokensFromNames :: NameSemanticMap -> [(Name, Span)] -> [SemanticTokenAbsolute]
extractSemanticTokensFromNames nsm =
    map (uncurry toAbsSemanticToken) . removeDup . mapMaybe (getSemantic nsm)
    where
        removeDup :: [(Span, SemanticTokenType)] -> [(Span, SemanticTokenType)]
        removeDup xs = Map.toList $ Map.fromListWith (<>) xs


toAbsSemanticToken :: Span -> SemanticTokenType -> SemanticTokenAbsolute
toAbsSemanticToken loc tokenType =
    let line = srcSpanStartLine loc - 1
        startChar = srcSpanStartCol loc - 1
        len = srcSpanEndCol loc - 1 - startChar
    in SemanticTokenAbsolute (fromIntegral line) (fromIntegral startChar)
        (fromIntegral len) (toLspTokenType tokenType) [SemanticTokenModifiers_Declaration]

getSemantic :: Map Name SemanticTokenType -> (Name, Span) -> Maybe (Span, SemanticTokenType)
getSemantic nameMap locName = do
    let name = fst locName
    let span = snd locName
    let tkt = toTokenType name
    let tokenType = maybe tkt (\x -> tkt <> x) $ Map.lookup name nameMap
    pure (span, tokenType)

importedNameFromModule :: forall a. Module -> HieAST a -> Set.Set Name
importedNameFromModule mod ast = Set.fromList importedNames
    where locatedNames = identifierGetter ast
        --   importedNames = [name | (name, _) <- locatedNames, not $ nameIsLocalOrFrom mod name
        --     , Just mod <- [nameModule_maybe name]]
          importedNames = [name | (name, _) <- locatedNames]


showLocatedNames :: [LIdP GhcRn] -> String
showLocatedNames xs = unlines
    [ showSDocUnsafe (ppr locName) ++ " " ++ show (getLoc locName)
    | locName <- xs]

showName :: Name -> String
showName name = occNameString (occName name) <> ":" <> showSDocUnsafe (ppr name)

getNamesRefs
  :: (MonadIO m)
  => WithHieDb
  -> Set.Set Name
  -> m [NormalizedFilePath]
getNamesRefs withHieDb nameSet = do

--   let nameSpanMap = Map.toList $ Map.fromListWith (<>) $ (fmap . fmap) return nameSet
  refs <- fmap rights $  forM (Set.toList nameSet) $ \name ->
      do ans <- liftIO $ withHieDb (\hieDb -> findOneDef hieDb (nameOccName name) Nothing Nothing)
         return $ ans
--   only those names that are in source code
  let ans = [path | ref <- refs, let (Just path) = rowToLoc ref]
--   liftIO $ putStrLn $ "getNamesRefs: " <> show (List.length ans)
--   liftIO $ mapM_ (putStrLn . showName) nameSet
--   liftIO $ mapM_ print ans
  return ans
  where
    rowToLoc (row:.info) = mfile
        where mfile = toNormalizedFilePath <$> modInfoSrcFile info
