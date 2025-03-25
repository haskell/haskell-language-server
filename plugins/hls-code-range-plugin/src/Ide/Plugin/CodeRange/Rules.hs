{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.CodeRange.Rules
    ( CodeRange (..)
    , codeRange_range
    , codeRange_children
    , codeRange_kind
    , CodeRangeKind(..)
    , GetCodeRange(..)
    , codeRangeRule
    , Log(..)

    -- * Internal
    , removeInterleaving
    , simplify
    , crkToFrk
    ) where

import           Control.DeepSeq                    (NFData)
import qualified Control.Lens                       as Lens
import           Control.Monad                      (foldM)
import           Control.Monad.Except               (ExceptT (..), runExceptT)
import           Control.Monad.Reader               (runReader)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Maybe          (MaybeT (MaybeT),
                                                     maybeToExceptT)
import           Control.Monad.Trans.Writer.CPS
import           Data.Coerce                        (coerce)
import           Data.Foldable                      (traverse_)
import           Data.Function                      (on, (&))
import           Data.Hashable
import           Data.List                          (sort)
import qualified Data.Map.Strict                    as Map
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Development.IDE
import           Development.IDE.Core.Rules         (toIdeResult)
import qualified Development.IDE.Core.Shake         as Shake
import           Development.IDE.GHC.Compat         (HieAST (..),
                                                     HieASTs (getAsts), RefMap)
import           Development.IDE.GHC.Compat.Util
import           GHC.Generics                       (Generic)
import           Ide.Plugin.CodeRange.ASTPreProcess (CustomNodeType (..),
                                                     PreProcessEnv (..),
                                                     isCustomNode,
                                                     preProcessAST)
import           Language.LSP.Protocol.Types        (FoldingRangeKind (FoldingRangeKind_Comment, FoldingRangeKind_Imports, FoldingRangeKind_Region))

import           Language.LSP.Protocol.Lens         (HasEnd (end),
                                                     HasStart (start))
import           Prelude                            hiding (log)

data Log = LogShake Shake.Log
    | LogNoAST
    | LogFoundInterleaving CodeRange CodeRange
      deriving Show

instance Pretty Log where
    pretty log = case log of
        LogShake shakeLog -> pretty shakeLog
        LogNoAST          -> "no HieAst exist for file"
        LogFoundInterleaving r1 r2 ->
            let prettyRange = pretty . show . _codeRange_range
             in "CodeRange interleave: " <> prettyRange r1 <> " & " <> prettyRange r2

-- | A tree representing code ranges in a file. This can be useful for features like selection range and folding range
data CodeRange = CodeRange {
    -- | Range for current level
        _codeRange_range    :: !Range,
    -- | A vector of children, sorted by their ranges in ascending order.
    -- Children are guaranteed not to interleave, but some gaps may exist among them.
        _codeRange_children :: !(Vector CodeRange),
    -- The kind of current code range
        _codeRange_kind     :: !CodeRangeKind
    }
    deriving (Show, Generic, NFData)

-- | 'CodeKind' represents the kind of a code range
data CodeRangeKind =
    -- | ordinary code
    CodeKindRegion
    -- | the group of imports
  | CodeKindImports
  -- | a comment
  | CodeKindComment
    deriving (Show, Eq, Generic, NFData)

Lens.makeLenses ''CodeRange

instance Eq CodeRange where
    (==) = (==) `on` _codeRange_range

instance Ord CodeRange where
    compare :: CodeRange -> CodeRange -> Ordering
    compare = compare `on` _codeRange_range

-- | Construct a 'CodeRange'. A valid CodeRange will be returned in any case. If anything go wrong,
-- a list of warnings will be returned as 'Log'
buildCodeRange :: HieAST a -> RefMap a -> Writer [Log] CodeRange
buildCodeRange ast refMap = do
    -- We work on 'HieAST', then convert it to 'CodeRange', so that applications such as selection range and folding
    -- range don't need to care about 'HieAST'
    -- TODO @sloorush actually use 'Annotated ParsedSource' to handle structures not in 'HieAST' properly (for example comments)
    let ast' = runReader (preProcessAST ast) (PreProcessEnv refMap)
    codeRange <- astToCodeRange ast'
    pure $ simplify codeRange

astToCodeRange :: HieAST a -> Writer [Log] CodeRange
astToCodeRange (Node _ sp []) = pure $ CodeRange (realSrcSpanToRange sp) mempty CodeKindRegion
astToCodeRange node@(Node _ sp children) = do
    children' <- removeInterleaving . sort =<< traverse astToCodeRange children
    let codeKind = if Just CustomNodeImportsGroup == isCustomNode node then CodeKindImports else CodeKindRegion
    pure $ CodeRange (realSrcSpanToRange sp) (V.fromList children') codeKind

-- | Remove interleaving of the list of 'CodeRange's.
removeInterleaving :: [CodeRange] -> Writer [Log] [CodeRange]
removeInterleaving = fmap reverse . foldM go []
  where
    -- we want to traverse from left to right (to make the logs easier to read)
    go :: [CodeRange] -> CodeRange -> Writer [Log] [CodeRange]
    go [] x = pure [x]
    go (x1:acc) x2 = do
        -- Given that the CodeRange is already sorted on it's Range, and the Ord instance of Range
        -- compares it's start position first, the start position must be already in an ascending order.
        -- Then, if the end position of a node is larger than it's next neighbour's start position, an interleaving
        -- must exist.
        -- (Note: LSP Range's end position is exclusive)
        x1' <- if x1 Lens.^. codeRange_range . end > x2 Lens.^. codeRange_range . start
            then do
                -- set x1.end to x2.start
                let x1' :: CodeRange = x1 & codeRange_range . end Lens..~ (x2 Lens.^. codeRange_range . start)
                tell [LogFoundInterleaving x1 x2]
                pure x1'
            else pure x1
        pure $ x2:x1':acc

-- | Remove redundant nodes in 'CodeRange' tree
simplify :: CodeRange -> CodeRange
simplify r =
    case onlyChild of
        -- If a node has the exact same range as it's parent, and it has no sibling, then it can be removed.
        Just onlyChild' ->
            if _codeRange_range onlyChild' == curRange
            then simplify (r { _codeRange_children = _codeRange_children onlyChild' })
            else withChildrenSimplified
        Nothing -> withChildrenSimplified
  where
    curRange = _codeRange_range r

    onlyChild :: Maybe CodeRange =
        let children = _codeRange_children r
         in if V.length children == 1 then V.headM children else Nothing

    withChildrenSimplified = r { _codeRange_children = simplify <$> _codeRange_children r }

data GetCodeRange = GetCodeRange
    deriving (Eq, Show, Generic)

instance Hashable GetCodeRange
instance NFData   GetCodeRange

type instance RuleResult GetCodeRange = CodeRange

codeRangeRule :: Recorder (WithPriority Log) -> Rules ()
codeRangeRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetCodeRange file -> handleError recorder $ do
        -- We need both 'HieAST' (for basic AST) and api annotations (for comments and some keywords).
        -- See https://gitlab.haskell.org/ghc/ghc/-/wikis/api-annotations
        HAR{hieAst, refMap} <- lift $ use_ GetHieAst file
        ast <- maybeToExceptT LogNoAST . MaybeT . pure $
            getAsts hieAst Map.!? (coerce . mkFastString . fromNormalizedFilePath) file
        let (codeRange, warnings) = runWriter (buildCodeRange ast refMap)
        traverse_ (logWith recorder Warning) warnings

        pure codeRange

-- | Handle error in 'Action'. Returns an 'IdeResult' with no value and no diagnostics on error. (but writes log)
handleError :: Recorder (WithPriority msg) -> ExceptT msg Action a -> Action (IdeResult a)
handleError recorder action' = do
    valueEither <- runExceptT action'
    case valueEither of
        Left msg -> do
            logWith recorder Warning msg
            pure $ toIdeResult (Left [])
        Right value -> pure $ toIdeResult (Right value)

-- | Maps type CodeRangeKind to FoldingRangeKind
crkToFrk :: CodeRangeKind -> FoldingRangeKind
crkToFrk crk = case crk of
        CodeKindComment -> FoldingRangeKind_Comment
        CodeKindImports -> FoldingRangeKind_Imports
        CodeKindRegion  -> FoldingRangeKind_Region
