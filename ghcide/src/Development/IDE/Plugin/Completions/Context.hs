{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Plugin.Completions.Context where

import           Control.DeepSeq                      (NFData (..), rwhnf)
import           Data.Hashable                        (Hashable)
import qualified Data.IntervalMap.FingerTree          as IM
import           Data.List                            (maximumBy)
import           Data.Maybe                           (maybeToList)
import           Data.Ord                             (Down (..), comparing)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util      (bagToList)
import           GHC.Generics                         (Generic)
import           Ide.Plugin.RangeMap                  (RangeMap (..), fromList')

-- | A context of a declaration in the program
-- e.g. is the declaration a type declaration or a value declaration
-- Used for determining which code completions to show
data Context
  = TypeContext
  | ValueContext
  | -- | module context with module name
    ModuleContext T.Text
  | -- | import context with module name
    ImportContext T.Text
  | -- | import list context with module name
    ImportListContext T.Text
  | -- | import hiding context with module name
    ImportHidingContext T.Text
  | -- | List of exported identifiers from the current module
    ExportContext
  | -- | Top-level context
    TopContext
  | -- | Unsupported context, a placeholder context where we give up being smart
    -- and show all known symbols.
    DefaultContext
  deriving (Show, Eq)

instance Pretty Context where
  pretty = \case
    TypeContext -> "type context"
    ValueContext -> "value context"
    ModuleContext mod -> "module context " <> pretty mod
    ImportContext mod -> "import context " <> pretty mod
    ImportListContext mod -> "import explicit context " <> pretty mod
    ImportHidingContext mod -> "import hiding context " <> pretty mod
    ExportContext -> "export context"
    TopContext -> "top context"
    DefaultContext -> "unknown context"

data GetContextTree = GetContextTree
  deriving (Eq, Show, Generic)
instance Hashable GetContextTree
instance NFData GetContextTree
type instance RuleResult GetContextTree = ContextTree

newtype ContextTree = ContextTree {contextTree :: RangeMap Context}

instance Show ContextTree where show _ = "<context tree>"
instance NFData ContextTree where rnf = rwhnf

-- | Build a 'ContextTree' from a parsed module.
--
-- Walks module header, exports, imports, and top-level declarations
-- (one level into class bodies). Built once per file edit and cached
-- as a Shake rule.
--
-- TODO: Would be nice if this would be updated incrementally. Most of the time
-- edits occur in unrelated parts of the module, meaning the largest proportion of
-- this tree doesn't require changing.
--
-- Could be done by tracking the 'dirtied' parts of a file using didChange and
-- 'refreshing' and doing a lighter weight traversal across the parsed module.
getContextTree :: ParsedModule -> ContextTree
getContextTree pm = ContextTree $ fromList' entries
  where
    HsModule{hsmodName, hsmodExports, hsmodImports, hsmodDecls} =
      unLoc (pm_parsed_source pm)

    entries :: [(Range, Context)]
    entries = moduleEntry ++ exportEntry ++ importEntries ++ declEntries

    -- Module name keyword span -> ModuleContext
    moduleEntry = case hsmodName of
      Just (L (locA -> ss) modName) ->
        maybeToList $ (, ModuleContext (T.pack $ moduleNameString modName)) <$> srcSpanToRange ss
      Nothing -> []

    -- Export list -> ExportContext
    exportEntry = case hsmodExports of
      Just (L (locA -> ss) _) ->
        maybeToList $ (, ExportContext) <$> srcSpanToRange ss
      Nothing -> []

    -- Each import declaration
    importEntries = foldMap importEntry hsmodImports

    importEntry :: LImportDecl GhcPs -> [(Range, Context)]
    importEntry (L (locA -> ss) impDecl) =
      let modName = T.pack $ moduleNameString $ unLoc $ ideclName impDecl
          outerCtx = (, ImportContext modName) <$> srcSpanToRange ss
          innerCtx = importListEntry modName (fmap (fmap reLoc) $ ideclImportList impDecl)
      in maybeToList outerCtx ++ innerCtx

    importListEntry :: T.Text -> Maybe (ImportListInterpretation, Located [LIE GhcPs]) -> [(Range, Context)]
    importListEntry modName (Just (EverythingBut, L ss _)) =
      maybeToList $ (, ImportHidingContext modName) <$> srcSpanToRange ss
    importListEntry modName (Just (Exactly, L ss _)) =
      maybeToList $ (, ImportListContext modName) <$> srcSpanToRange ss
    importListEntry _ _ = []

    -- Top-level declarations
    declEntries = concatMap declEntry hsmodDecls

    declEntry :: LHsDecl GhcPs -> [(Range, Context)]
    declEntry (L (locA -> ss) decl) = case srcSpanToRange ss of
      Nothing -> []
      Just range -> case decl of
        SigD {}                -> [(range, TypeContext)]
        ValD {}                -> [(range, ValueContext)]
        TyClD _ cd@ClassDecl{} -> (range, TypeContext) : classEntries cd
        TyClD {}               -> [(range, TypeContext)]   -- DataDecl, SynDecl, FamilyDecl
        InstD {}               -> [(range, ValueContext)]
        DerivD {}              -> [(range, TypeContext)]
        ForD {}                -> [(range, ValueContext)]
        SpliceD {}             -> [(range, TopContext)]
        _                      -> [(range, DefaultContext)]  -- DefD, WarningD, AnnD, RuleD, DocD, KindSigD

    -- One level into class bodies: method sigs and default implementations
    classEntries :: TyClDecl GhcPs -> [(Range, Context)]
    classEntries ClassDecl{tcdSigs, tcdMeths} =
      [ (r, TypeContext)
      | L (locA -> ss) _ <- tcdSigs
      , Just r <- [srcSpanToRange ss]
      ] ++
      [ (r, ValueContext)
      | L (locA -> ss) _ <- bagToList tcdMeths
      , Just r <- [srcSpanToRange ss]
      ]
    classEntries _ = []

-- | Look up the completion context at a given position.
-- Returns the innermost (most specific) context that contains the position.
getContext :: ContextTree -> PositionResult Position -> Context
getContext (ContextTree (RangeMap im)) pos =
    case IM.dominators pointInterval im of
      [] -> TopContext
      xs -> snd $ maximumBy (comparing (\(iv, _) -> (IM.low iv, Down (IM.high iv)))) xs
  where
    pointInterval = case pos of
      PositionExact p   -> IM.Interval p p
      PositionRange l u -> IM.Interval l u
