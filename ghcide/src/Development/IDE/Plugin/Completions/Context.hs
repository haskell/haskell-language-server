{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors   #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Plugin.Completions.Context
  ( Context (..)
  , ContextGroup (..)
  , ContextMap
  , GetContextMap (..)
  , contextHasModuleHeader
  , getContext
  , getContextMap
  ) where

import           Control.DeepSeq                      (NFData (..), rwhnf)
import           Data.Generics                        (Data (..), GenericQ,
                                                       extQ, mkQ)
import           Data.Hashable                        (Hashable)
import           Data.List.Extra                      (nubOrd)
import           Data.Maybe                           (fromJust, isJust,
                                                       mapMaybe)
import           Data.List.Extra                      (nubOrd)
import           Data.Maybe                           (isJust, mapMaybe,
                                                       maybeToList)
import           Data.List.Extra                      (nub)
import           Data.Maybe                           (isJust, mapMaybe,
                                                       maybeToList)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat           hiding (getContext)
import           GHC.Generics                         (Generic)

#if MIN_VERSION_ghc(9,9,0)
import           GHC.Hs                               (HasLoc)
#endif


-- | A context of a declaration in the program e.g. is the declaration a
-- type declaration or a value declaration. Used for determining which code
-- completions to show.
data Context
  = TypeContext
  | ValueContext
  | -- | import context with module name.
    ImportContext T.Text
  | -- | import list context with module name.
    ImportListContext T.Text
  | -- | import hiding context with module name.
    ImportHidingContext T.Text
  | -- | Top-level context, with context groups indicating what would be valid
    -- in that top-level context.
    --
    -- NB: An empty list denotes _all_ contexts, this occurs in splices which
    -- overlap with the top-level declaration snippets while typing.
    TopContext [ContextGroup]
  | -- | Unsupported context, a placeholder context where we give up being smart
    -- and show all known symbols.
    DefaultContext
  deriving (Show, Eq)

data ContextGroup
  = HeaderGroup
  | ImportGroup
  | DeclarationGroup
  deriving (Show, Eq, Ord)

data GetContextMap = GetContextMap
  deriving (Eq, Show, Generic)
instance Hashable GetContextMap
instance NFData GetContextMap
type instance RuleResult GetContextMap = ContextMap

-- | Entries within each chunk are from a contiguous group of source items
-- (imports or declarations).
data ContextChunk = Chunk
  { low     :: {-# UNPACK #-} !Position
  , high    :: {-# UNPACK #-} !Position
  , group   :: !ContextGroup
  , context :: Range -> ContextResult
  }

-- | Build lazy 'ContextChunk' by processing @n@ source items at a time.
groupedChunks :: Int -> ContextGroup -> (a -> Maybe Range) -> (a -> Range -> ContextResult) -> [a] -> [ContextChunk]
groupedChunks n group getPos locate xs = go xs
  where
    go [] = []
    go xs =
      let (chunk, rest) = splitAt n xs
          context = foldMap locate chunk
          positions = mapMaybe getPos chunk
      in case positions of
            [] -> go rest
            ps -> Chunk
              { low   = minimum (fmap _start ps)
              , high  = maximum (fmap _end ps)
              , group
              , context
              } : go rest

-- | Build lazy 'ContextChunk' by processing @n@ source items at a time.
singletonChunk :: ContextGroup -> (a -> Maybe Range) -> (a -> Range -> ContextResult) -> a -> ContextChunk
singletonChunk group getPos locate inp = Chunk s e group (locate inp)
  where
    Range s e = fromJust $ getPos inp

-- | Used during context finding, combines into the tightest interval.
-- As an intuition, the primary interface is through
-- @Monoid (Range -> ContextResult)@.
data ContextResult = NoContext | ContextResult !Range !Context
instance Monoid ContextResult where mempty = NoContext
instance Semigroup ContextResult where (<>) = tighten

tighten :: ContextResult -> ContextResult -> ContextResult
tighten NoContext b = b
tighten a NoContext = a
tighten ar@(ContextResult a _) br@(ContextResult b _) =
  if a `dominates` b then br else ar

-- | A context map, built from a parsed module. Stores whether the module
-- already has a @module ... where@ header, so that the header snippet can
-- be suppressed for files that already declare a module.
data ContextMap = ContextMap !Bool [ContextChunk]
instance Semigroup ContextMap where
  ContextMap h1 c1 <> ContextMap h2 c2 = ContextMap (h1 || h2) (c1 <> c2)
instance Monoid ContextMap where
  mempty = ContextMap False []
instance Show ContextMap where show _ = "<context map>"
instance NFData ContextMap where rnf = rwhnf

-- * Building

-- | Build a @ContextMap@ from a parsed module.
--
-- Walks module header, exports, imports, and top-level declarations
-- (one level into class bodies). Built once per file edit and cached
-- as a Shake rule.
getContextMap :: ParsedModule -> ContextMap
getContextMap pm =
  ContextMap (isJust hsmodName) $
    -- These denote the size of the "jumps" of the cursor when traversing the AST.
    -- Reduces the amount of data we have to look at with syb.
  moduleChunk
    <> groupedChunks 10 ImportGroup rangeOf getImportContext hsmodImports
    <> groupedChunks 4 DeclarationGroup rangeOf getDeclContext hsmodDecls
  where
    HsModule {hsmodName, hsmodImports, hsmodDecls} =
      unLoc (pm_parsed_source pm)

#if MIN_VERSION_ghc(9,9,0)
rangeOf :: HasLoc a => a -> Maybe Range
rangeOf = srcSpanToRange . locA
#else
rangeOf :: GenLocated (SrcSpanAnn' a) e -> Maybe Range
rangeOf = srcSpanToRange . getLocA
#endif

getHeaderContext :: Data a => a -> Range -> ContextResult
getHeaderContext decl query =
  gather
    (<>)
    ((mempty, False) `mkQ` modNameQ query)
    decl

getImportContext :: LImportDecl GhcPs -> Range -> ContextResult
getImportContext imports query =
  gather
    (<>)
    ((mempty, False) `mkQ` importQ query)
    imports

getDeclContext :: LHsDecl GhcPs -> Range -> ContextResult
getDeclContext declarations query =
  gather
    (<>)
    ((mempty, False) `mkQ` sigQ query `extQ` bindQ query `extQ` declQ query)
    declarations

-- * Querying

-- | Returns 'True' when the parsed module already has a @module ... where@
-- declaration. Used downstream to suppress the module header snippet.
contextHasModuleHeader :: ContextMap -> Bool
contextHasModuleHeader (ContextMap h _) = h

-- | Look up the completion context at a given position.
-- Returns the innermost (most specific) context that contains the position.
--
-- Only the 'ContextChunks' up to and including the chunk containing the
-- query position are forced; later chunks remain as unevaluated thunks.
getContext :: ContextMap -> PositionResult Position -> Context
getContext (ContextMap _ chunks) query =
  case searchChunks HeaderGroup chunks mempty of
    (groups, NoContext)        -> TopContext $ nub groups
    (_, ContextResult _ found) -> found
  where
    (qLo, qHi) = case query of
      PositionExact p   -> (p, p)
      PositionRange l u -> (l, u)

    searchChunks :: ContextGroup -> [ContextChunk] -> ([ContextGroup], ContextResult) -> ([ContextGroup], ContextResult)
    searchChunks _ [] !acc = acc
    searchChunks lastChunk (Chunk cLo cHi group contextOf : rest) !acc
      | -- query is past this chunk (line-only comparison so cursors
        -- past the last column on the final line still match)
        _line qLo > _line cHi = searchChunks group rest acc
      | -- query is before this chunk
        qHi < cLo = ([lastChunk, group], mempty) <> acc
        -- this chunk is relevant, emit the group and all relevant intervals
      | otherwise = searchChunks group rest (([group], contextOf (Range qLo qHi)) <> acc)

-- * SYB queries types

importQ :: Range -> LImportDecl GhcPs -> (ContextResult, Bool)
importQ query impDecl'@(L _ impDecl) =
  let importModuleName = T.pack $ moduleNameString $ unLoc $ ideclName impDecl
      inlineResults = fst $ importInline query importModuleName (ideclImportList impDecl)
      importResult = fst $ contextual (ImportContext importModuleName) True query impDecl'
      importInline _ _ Nothing = (mempty, False)
      importInline query modName (Just (which, l)) =
        case which of
          EverythingBut -> contextual (ImportHidingContext modName) True query l
          Exactly       -> contextual (ImportListContext modName) True query l
   in (inlineResults <> importResult, False)


declQ :: Range -> LHsDecl GhcPs -> (ContextResult, Bool)
declQ query (L (locA -> ss) decl) = case srcSpanToRange ss of
  Nothing -> (mempty, True)
  Just range | outside query range -> (mempty, True)
  Just range -> case decl of
    SigD {}    -> (ContextResult range TypeContext, True)
    ValD {}    -> (ContextResult range ValueContext, False)
    TyClD {}   -> (ContextResult range TypeContext, False)   -- DataDecl, SynDecl, FamilyDecl
    InstD {}   -> (ContextResult range ValueContext, False)
    DerivD {}  -> (ContextResult range TypeContext, True)
    SpliceD {} -> (ContextResult range (TopContext []), True)
    _          -> (ContextResult range DefaultContext, True)  -- DefD, WarningD, AnnD, RuleD, DocD, KindSigD

modNameQ :: Range -> XRec GhcPs ModuleName -> (ContextResult, Bool)
modNameQ = contextual (TopContext [HeaderGroup]) True

sigQ :: Range -> LSig GhcPs -> (ContextResult, Bool)
sigQ = contextual TypeContext True

bindQ :: Range -> LHsBind GhcPs -> (ContextResult, Bool)
bindQ = contextual ValueContext False


#if MIN_VERSION_ghc(9,9,0)
contextual :: HasLoc a => Context -> Bool -> Range -> a -> (ContextResult, Bool)
#else
contextual :: Context -> Bool -> Range -> GenLocated (SrcSpanAnn' a) e -> (ContextResult, Bool)
#endif
contextual context shouldStop query s =
  let range = rangeOf s
   in case range of
        Nothing -> (mempty, True)
        Just range | outside query range -> (mempty, True)
        Just range -> (ContextResult range context, shouldStop)

-- * Helpers

dominates :: Range -> Range -> Bool
dominates (Range s e) (Range qs qe) = s <= qs && qe <= e

-- | A query range is outside a source range if it ends before the source
-- starts, or it starts on a line after the source ends.
-- We intentionally compare only lines (not columns) for the trailing
-- boundary so that a cursor past the last token on a line still falls
-- inside the node occupying that line.
outside :: Range -> Range -> Bool
outside (Range ps pe) (Range qs qe) = pe < qs || _line ps > _line qe

instance Pretty Context where
  pretty = \case
    TypeContext -> "type context"
    ValueContext -> "value context"
    ImportContext mod -> "import context " <> pretty mod
    ImportListContext mod -> "import explicit context " <> pretty mod
    ImportHidingContext mod -> "import hiding context " <> pretty mod
    TopContext cg -> "top context " <> pretty cg
    DefaultContext -> "unknown context"

instance Pretty ContextGroup where
  pretty = \case
    HeaderGroup -> "header"
    ImportGroup -> "imports"
    DeclarationGroup -> "declarations"

-- | Variation of @Data.Generics.Schemes.everythingBut@, but uses foldl'.
gather :: forall r. (r -> r -> r) -> GenericQ (r, Bool) -> GenericQ r
gather k f = go
  where
    go :: GenericQ r
    go x = let (v, stop) = f x
           in if stop
                then v
                else foldl' k v (gmapQ go x)
