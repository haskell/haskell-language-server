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
import           Data.Hashable                        (Hashable)
import           Data.List                            (maximumBy)
import           Data.Maybe                           (catMaybes, mapMaybe)
import           Data.Ord                             (Down (..), comparing)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat           hiding (getContext)
import           Development.IDE.GHC.Compat.Util      (bagToList)
import           GHC.Generics                         (Generic)
import           GHC.Hs                               (HasLoc)

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
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

data GetContextMap = GetContextMap
  deriving (Eq, Show, Generic)
instance Hashable GetContextMap
instance NFData GetContextMap
type instance RuleResult GetContextMap = ContextMap

-- | A lazy chunked interval structure for context lookups.
--
-- Entries within each chunk are from a contiguous group of source items
-- (imports or declarations).
data ContextChunk = Chunk
  { low   :: {-# UNPACK #-} !Position
  , high  :: {-# UNPACK #-} !Position
  , group :: {-# UNPACK #-} !ContextGroup
  , items :: [(Range, Context)]
  }

-- | Build lazy 'ContextChunk' by processing @n@ source items at a time.
groupedChunks :: Int -> ContextGroup -> (a -> Maybe Range) -> (a -> [(Range, Context)]) -> [a] -> ContextMap
groupedChunks n group getPos getRanges xs = ContextMap $ go xs
  where
    go [] = []
    go xs =
      let (chunk, rest) = splitAt n xs
          items = concatMap getRanges chunk
      in case items of
            [] -> go rest
            _  -> Chunk
              { low   = minimum (mapMaybe (fmap _start  . getPos) chunk)
              , high  = maximum (mapMaybe (fmap _end  . getPos) chunk)
              , group
              , items
              } : go rest

newtype ContextMap = ContextMap [ContextChunk]
  deriving newtype (Monoid, Semigroup)
instance Show ContextMap where show _ = "<context map>"
instance NFData ContextMap where rnf = rwhnf

-- | Build a 'ContextMap' from a parsed module.
--
-- Walks module header, exports, imports, and top-level declarations
-- (one level into class bodies). Built once per file edit and cached
-- as a Shake rule.
getContextMap :: ParsedModule -> ContextMap
getContextMap pm =
    groupedChunks 10 ImportGroup rangeOf importEntry hsmodImports
    <> groupedChunks 10 DeclarationGroup rangeOf declEntry hsmodDecls
  where
    HsModule {hsmodImports, hsmodDecls} =
      unLoc (pm_parsed_source pm)

    rangeOf :: HasLoc (Anno a) => XRec GhcPs a -> Maybe Range
    rangeOf (L (locA -> ss) _) = srcSpanToRange ss
    fromSpan context ss = (,context) <$> rangeOf ss

    importEntry :: LImportDecl GhcPs -> [(Range, Context)]
    importEntry decl@(L _ impDecl) =
      let modName = T.pack $ moduleNameString $ unLoc $ ideclName impDecl
          outerCtx = fromSpan (ImportContext modName) decl
          innerCtx = importListEntry modName (fmap (fmap reLoc) $ ideclImportList impDecl)
       in catMaybes [outerCtx, innerCtx]

    importListEntry modName (Just (EverythingBut, imps)) = fromSpan (ImportHidingContext modName) imps
    importListEntry modName (Just (Exactly, imps)) = fromSpan (ImportHidingContext modName) imps
    importListEntry _ _ = Nothing

    declEntry :: LHsDecl GhcPs -> [(Range, Context)]
    declEntry (L (locA -> ss) decl) = case srcSpanToRange ss of
      Nothing -> []
      Just range -> case decl of
        SigD {}                -> [(range, TypeContext)]
        ValD _ bind            -> (range, ValueContext) : bindEntries bind
        TyClD _ cd@ClassDecl{} -> (range, TypeContext) : classEntries cd
        TyClD {}               -> [(range, TypeContext)]   -- DataDecl, SynDecl, FamilyDecl
        InstD _ instDecl       -> (range, ValueContext) : instEntries instDecl
        DerivD {}              -> [(range, TypeContext)]
        ForD {}                -> [(range, ValueContext)]
        SpliceD {}             -> [(range, TopContext [])]
        _                      -> [(range, DefaultContext)]  -- DefD, WarningD, AnnD, RuleD, DocD, KindSigD

    sigsAndBindEntries :: [LSig GhcPs] -> LHsBinds GhcPs -> [(Range, Context)]
    sigsAndBindEntries sigs binds =
      [ (r, TypeContext)
      | L (locA -> ss) _ <- sigs
      , Just r <- [srcSpanToRange ss]
      ] ++
      [ entry
      | L (locA -> ss) bind <- bagToList binds
      , Just r <- [srcSpanToRange ss]
      , entry <- (r, ValueContext) : bindEntries bind
      ]

    classEntries :: TyClDecl GhcPs -> [(Range, Context)]
    classEntries ClassDecl { tcdSigs, tcdMeths } = sigsAndBindEntries tcdSigs tcdMeths
    classEntries _ = []

    instEntries :: InstDecl GhcPs -> [(Range, Context)]
    instEntries ClsInstD { cid_inst = ClsInstDecl { cid_sigs, cid_binds } } =
      sigsAndBindEntries cid_sigs cid_binds
    instEntries _ = []

    bindEntries :: HsBind GhcPs -> [(Range, Context)]
    bindEntries FunBind { fun_matches = MG { mg_alts = L _ alts } } =
      concatMap matchLocalEntries alts
    bindEntries PatBind { pat_rhs = GRHSs { grhssLocalBinds } } =
      localBindEntries grhssLocalBinds
    bindEntries _ = []

    matchLocalEntries :: LMatch GhcPs (LHsExpr GhcPs) -> [(Range, Context)]
    matchLocalEntries (L _ Match { m_grhss = GRHSs { grhssLocalBinds } }) =
      localBindEntries grhssLocalBinds

    localBindEntries :: HsLocalBinds GhcPs -> [(Range, Context)]
    localBindEntries (HsValBinds _ (ValBinds _ binds sigs)) =
      sigsAndBindEntries sigs binds
    localBindEntries _ = []

-- | Look up the completion context at a given position.
-- Returns the innermost (most specific) context that contains the position.
--
-- Only the 'ContextChunks' up to and including the chunk containing the
-- query position are forced; later chunks remain as unevaluated thunks.
getContext :: ContextMap -> PositionResult Position -> Context
getContext (ContextMap chunks) pos =
  case searchChunks True chunks of
    ([], []) -> TopContext []
    (groups, []) -> TopContext groups
    (_, xs) -> snd $ maximumBy (comparing (\(Range s e, _) -> (s, Down e))) xs
  where
    (qLo, qHi) = case pos of
      PositionExact p   -> (p, p)
      PositionRange l u -> (l, u)

    dominates :: (Range, Context) -> Bool
    dominates (Range s e, _) = s <= qLo && qHi <= e

    searchChunks :: Bool -> [ContextChunk] -> ([ContextGroup], [(Range, Context)])
    searchChunks _ [] = ([], [])
    searchChunks firstChunk (Chunk cLo cHi group items : rest)
      | -- query is past this chunk
        qLo > cHi = searchChunks False rest
      | -- query is before this chunk
        qHi < cLo = (if firstChunk then [HeaderGroup] else [], [])
        -- this chunk is relevant, emit the group and all relevant intervals
      | otherwise  = ([group], filter dominates items) <> searchChunks False rest
