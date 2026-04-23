{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors   #-}
{-# LANGUAGE TypeFamilies       #-}

module Development.IDE.Plugin.Completions.Context
  ( Context (..)
  , ContextGroup (..)
  , ContextMap
  , GetContextMap (..)
  , getContext
  , getContextMap
  ) where

import           Control.DeepSeq                      (NFData (..), rwhnf)
import           Data.Generics                        (extQ, mkQ)
import           Data.Generics.Schemes                (everythingBut)
import           Data.Hashable                        (Hashable)
import           Data.Maybe                           (mapMaybe)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat           hiding (getContext)
import           GHC.Generics                         (Generic)
import           GHC.Hs                               (HasLoc)

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
  , group   :: {-# UNPACK #-} !ContextGroup
  , context :: Range -> ContextResult
  }

-- | Build lazy 'ContextChunk' by processing @n@ source items at a time.
groupedChunks :: Int -> ContextGroup -> (a -> Maybe Range) -> (a -> Range -> ContextResult) -> [a] -> ContextMap
groupedChunks n group getPos locate xs = ContextMap $ go xs
  where
    go [] = []
    go xs =
      let (chunk, rest) = splitAt n xs
          context = foldMap locate chunk
      in case chunk of
            [] -> go rest
            _  -> Chunk
              { low   = minimum (mapMaybe (fmap _start  . getPos) chunk)
              , high  = maximum (mapMaybe (fmap _end  . getPos) chunk)
              , group
              , context
              } : go rest

-- | Used during context finding, combines into the tightest interval.
-- As an intuition, the primary interface is through
-- @Monoid (Position -> ContextResult)@.
data ContextResult = NoContext | ContextResult Range Context
instance Monoid ContextResult where mempty = NoContext
instance Semigroup ContextResult where (<>) = tighten

tighten :: ContextResult -> ContextResult -> ContextResult
tighten NoContext b = b
tighten a NoContext = a
tighten ar@(ContextResult a _) br@(ContextResult b _) =
  if a `dominates` b then br else ar

newtype ContextMap = ContextMap [ContextChunk]
  deriving newtype (Monoid, Semigroup)
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
  groupedChunks 10 ImportGroup rangeOf getImportContext hsmodImports
    <> groupedChunks 5 DeclarationGroup rangeOf getDeclContext hsmodDecls
  where
    HsModule {hsmodImports, hsmodDecls} =
      unLoc (pm_parsed_source pm)

rangeOf :: HasLoc a => a -> Maybe Range
rangeOf = srcSpanToRange . locA

getImportContext :: LImportDecl GhcPs -> Range -> ContextResult
getImportContext imports query =
  everythingBut
    (<>)
    ((mempty, False) `mkQ` importQ query)
    imports

getDeclContext :: LHsDecl GhcPs -> Range -> ContextResult
getDeclContext declarations query =
  everythingBut
    (<>)
    ((mempty, False) `mkQ` sigQ query `extQ` bindQ query `extQ` declQ query)
    declarations

-- * Querying

-- | Look up the completion context at a given position.
-- Returns the innermost (most specific) context that contains the position.
--
-- Only the 'ContextChunks' up to and including the chunk containing the
-- query position are forced; later chunks remain as unevaluated thunks.
getContext :: ContextMap -> PositionResult Position -> Context
getContext (ContextMap chunks) query =
  case searchChunks True chunks of
    (groups, NoContext)        -> TopContext groups
    (_, ContextResult _ found) -> found
  where
    (qLo, qHi) = case query of
      PositionExact p   -> (p, p)
      PositionRange l u -> (l, u)

    searchChunks :: Bool -> [ContextChunk] -> ([ContextGroup], ContextResult)
    searchChunks _ [] = ([], mempty)
    searchChunks firstChunk (Chunk cLo cHi group contextOf : rest)
      | -- query is past this chunk (line-only comparison so cursors
        -- past the last column on the final line still match)
        _line qLo > _line cHi = searchChunks False rest
      | -- query is before this chunk
        qHi < cLo = (if firstChunk then [HeaderGroup] else [], mempty)
        -- this chunk is relevant, emit the group and all relevant intervals
      | otherwise  = ([group], contextOf (Range qLo qHi)) <> searchChunks False rest

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

sigQ :: Range -> LSig GhcPs -> (ContextResult, Bool)
sigQ = contextual TypeContext True

bindQ :: Range -> LHsBind GhcPs -> (ContextResult, Bool)
bindQ = contextual ValueContext False

contextual :: HasLoc a => Context -> Bool -> Range -> a -> (ContextResult, Bool)
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
