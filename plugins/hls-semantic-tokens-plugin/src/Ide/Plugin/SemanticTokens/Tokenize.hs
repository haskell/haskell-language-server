{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module Ide.Plugin.SemanticTokens.Tokenize (hieAstSpanIdentifiers) where

import           Data.Foldable                        (Foldable (foldl'))
import qualified Data.Map                             as M
import Control.Monad (guard)
import qualified Data.Map                             as Map
import           Data.Maybe                           (fromMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import qualified Data.Text                             as T
import           Data.Text                            (Text)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error            (realSrcSpanToCodePointRange)
import           Language.LSP.Protocol.Types          (Position (Position),
                                                       Range (Range), UInt)
import           Language.LSP.VFS hiding (line)
import           Prelude                              hiding (length, span)
import           Data.Text.Utf16.Rope.Mixed            (Rope)
import qualified Data.Text.Utf16.Rope.Mixed            as Rope
import qualified Data.Text.Rope            as Char
import Data.Text.Utf16.Rope (toText)
import Control.Monad.State (State, modify, MonadState (get), gets, MonadTrans (lift), runStateT, put)
import Control.Monad.Trans.State (StateT)
import Control.Monad.State (evalStateT)
import Control.Monad.State (execStateT)
import Control.Monad (forM_)
import Control.Monad.State (execState)
import Debug.Trace (traceShow, traceShowM, trace)
import Ide.Plugin.SemanticTokens.Utils (showIdentifier, rangeShortStr)
import qualified Data.Text.Utf16.Rope as Utf16
import Development.IDE (pretty)



type RangeIdSetMap = Map.Map Range (Set Identifier)
type TokenState = (RangeIdSetMap, Rope, Char.Position)
data PTokenState t = PTokenState {
    rangeIdSetMap :: RangeIdSetMap
    , rope :: Rope
    , cursor :: Char.Position
    , currentAst :: HieAST t
    , columnsInUtf16 :: UInt
    , currentRange :: Range
    , currentRangeContext :: RangeSplitContext
    }
data RangeSplitContext = RangeSplitContext {
    fullText :: Text,
    fullRange :: Range,
    splitResult :: SplitResult
} deriving (Show)
emptyRangeSplitContext :: RangeSplitContext
emptyRangeSplitContext = RangeSplitContext "" (Range (Position 0 0) (Position 0 0)) (NoSplit ("", Range (Position 0 0) (Position 0 0)))


data SplitResult =
    NoSplit (Text, Range) |
    -- token text, prefix range(module range), token range
    Split (Text, Range, Range) deriving (Show)

startRange :: Range
startRange = Range (Position 0 0) (Position 0 0)

mkPTokenState :: VirtualFile -> HieAST a -> PTokenState a
mkPTokenState vf ast = PTokenState mempty (Rope.fromText $ toText vf._file_text) (Char.Position 0 0) ast 0 startRange emptyRangeSplitContext

type Parser m a = forall t . StateT (PTokenState t) m a

updateCursor :: Monad m => Char.Position -> Parser m ()
updateCursor pos = modify $ \s -> s {cursor = pos}
updateRope :: Monad m => Rope -> Parser m ()
updateRope r = modify $ \s -> s {rope = r}
insertRangeIdSetMap :: Monad m => Range -> Set Identifier -> Parser m ()
insertRangeIdSetMap r si = modify $ \s -> s {rangeIdSetMap = Map.insertWith (<>) r si $ rangeIdSetMap s}
addRangeIdSetMap :: Monad m => Range -> Identifier -> Parser m ()
addRangeIdSetMap r i = insertRangeIdSetMap r $ S.singleton i
updateColumnsInUtf16 :: Monad m => UInt -> Parser m ()
updateColumnsInUtf16 n = modify $ \s -> s {columnsInUtf16 = n}

maybeM:: Monad m => Parser Maybe () -> Parser m ()
maybeM p = do
    st <- get
    forM_ (execStateT p st) put

foldAst :: Monad m => Parser m ()
foldAst = do
    ast <- gets currentAst
    if null (nodeChildren ast)
    then maybeM visitLeafIds
    else do
        let children = nodeChildren ast
        mapM_ (\x -> (modify $ \s -> s {currentAst = x}) >> foldAst) children

foldAndGetRangeIdSetMap :: VirtualFile -> HieAST a -> RangeIdSetMap
foldAndGetRangeIdSetMap vf ast = rangeIdSetMap $ execState foldAst (mkPTokenState vf ast)

codePointRangeToRangeWith :: UInt -> UInt -> CodePointRange -> Range
codePointRangeToRangeWith newStartCol newEndCol (CodePointRange (CodePointPosition startLine _) (CodePointPosition endLine _)) =
    Range (Position startLine newStartCol) (Position endLine newEndCol)

-- >>> T.breakOnEnd "::;" "a::b::c"
-- ("","a::b::c")

newColumn :: UInt -> Text -> UInt
newColumn n rp = case T.breakOnEnd "\n" rp of
    ("", nEnd) -> n + fromIntegral (Rope.utf16Length $ Rope.fromText nEnd)
    (_, nEnd) -> fromIntegral (Rope.utf16Length $ Rope.fromText nEnd)


visitLeafIds :: Parser Maybe ()
visitLeafIds = maybeM $ do
    leaf <- gets currentAst
    pos <- gets cursor
    rp <- gets rope
    let span = nodeSpan leaf
    (gap, token, remains) <- lift $ splitTokenAt pos rp span
    cs <- gets columnsInUtf16
    let ncs = newColumn cs gap
    let nce = newColumn ncs token
    let ran = codePointRangeToRangeWith ncs nce $ realSrcSpanToCodePointRange span
    -- ran <- lift $ codePointRangeToRange vf $ realSrcSpanToCodePointRange span
    -- _ <- lift $ traceShowM (cs, ncs, nce, rangeShortStr ran)
    -- set the new column to nce
    updateColumnsInUtf16 nce
    ranges <- lift $ splitRangeByText token ran
    modify $ \s -> s {currentRange = ran, currentRangeContext = ranges}
    mapM_ combineNodeIds $ Map.filterWithKey (\k _ -> k == SourceInfo) $ getSourcedNodeInfo $ sourcedNodeInfo leaf
    updateCursor $ srcSpanEndCharPosition span
    updateRope remains
    where
        combineNodeIds :: Monad m => NodeInfo a -> Parser m ()
        combineNodeIds (NodeInfo _ _ bd) = mapM_ getIdentifier (M.keys bd)
        getIdentifier :: Monad m => Identifier -> Parser m ()
        getIdentifier idt = maybeM $ do
            ran <- gets currentRange
            case idt of
                Left _moduleName -> addRangeIdSetMap ran idt
                Right name -> do
                    ranCtx <- gets currentRangeContext
                    occStr <- lift $ case nameString name of
                                -- the generated selector name with {-# LANGUAGE DuplicateRecordFields #-}
                                '$':'s':'e':'l':':':xs -> Just $ takeWhile (/= ':') xs
                                ['$'] -> Just "$"
                                -- other generated names that should not be visible
                                '$':_ -> Nothing
                                ns -> Just ns
                    case splitResult ranCtx of
                        (NoSplit (tk, r)) -> do
                            guard $ T.unpack tk == occStr
                            addRangeIdSetMap r idt
                        (Split (tk, r1, r2)) -> do
                            guard $ T.unpack tk == occStr
                            addRangeIdSetMap r1 (Left $ mkModuleName "")
                            addRangeIdSetMap r2 idt



splitTokenAt :: Char.Position -> Rope -> Span -> Maybe (Text, Text, Rope)
splitTokenAt pos rp span = do
    (startPos, length) <- srcSpanMaybePositionLength span
    let s = startPos `sub` pos
    -- discard the gap between the end of the last identifier and the start of the current identifier
    let (_gap, startRope) =  Rope.charSplitAtPosition s rp
    (token, remains) <- charSplitAtMaybe length startRope
    return (Rope.toText _gap, Rope.toText token, remains)

charSplitAtMaybe :: Word -> Rope -> Maybe (Rope, Rope)
charSplitAtMaybe len rp = do
    let (prefix, suffix) = Rope.charSplitAt len rp
    guard $ Rope.charLength prefix == len
    return (prefix, suffix)

srcSpanMaybePositionLength :: (Integral l) => RealSrcSpan -> Maybe (Char.Position, l)
srcSpanMaybePositionLength real = return (realSrcLocRopePosition $ realSrcSpanStart real,
    fromIntegral $ (srcLocCol $ realSrcSpanEnd real) - (srcLocCol $ realSrcSpanStart real))
realSrcLocRopePosition :: RealSrcLoc -> Char.Position
realSrcLocRopePosition real = Char.Position (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real - 1)

sub :: Char.Position -> Char.Position -> Char.Position
sub (Char.Position l1 c1) (Char.Position l2 c2) =
    if l1 == l2 then Char.Position 0 (c1 - c2) else Char.Position (l1 - l2) c1

srcSpanEndCharPosition :: RealSrcSpan -> Char.Position
srcSpanEndCharPosition real = realSrcLocRopePosition $ realSrcSpanEnd real

hieAstSpanIdentifiers :: VirtualFile -> HieAST a -> RangeIdSetMap
hieAstSpanIdentifiers =
    traceShow ("hello") foldAndGetRangeIdSetMap
-- hieAstSpanIdentifiers :: VirtualFile -> HieAST a -> RangeIdSetMap
-- hieAstSpanIdentifiers vf ast = case go (mempty, Rope.fromText $ toText vf._file_text, Char.Position 0 0) ast of
--         (m, _, _) -> m
--     where
--         go :: TokenState -> HieAST a -> TokenState
--         go ts astTree =
--             if null (nodeChildren astTree)
--             then getId ts astTree
--             else foldl' go ts (nodeChildren astTree)
--         getId :: TokenState -> HieAST a -> TokenState
--         getId ts@(rm, rp, pos) leaf = fromMaybe ts $ do
--             let span = nodeSpan leaf
--             -- todo fix the range
--             ran <- codePointRangeToRange vf $ realSrcSpanToCodePointRange span
--             (_, token, remains) <- splitTokenAt pos rp span
--             -- todo fix module name
--             -- let idt = case splitRangeByText token ran of
--             --             Just (Left r) -> getNodeIds' (r, token) leaf
--             --             Just (Right (token', r1, r2)) -> M.insertWith (<>) r1 (S.singleton $ Left $ mkModuleName "")
--             --                                                 $ getNodeIds' (r2, token') leaf
--             --             _ -> mempty
--             return (Map.unionWith (<>) rm (getNodeIds' (ran, Rope.toText token) leaf), remains, srcSpanEndCharPosition span)
--             where
--                 getNodeIds' :: (Range, Text) -> HieAST a -> RangeIdSetMap
--                 getNodeIds' rt =
--                     Map.foldl' (combineNodeIds rt) mempty
--                         . Map.filterWithKey (\k _ -> k == SourceInfo)
--                         . getSourcedNodeInfo
--                         . sourcedNodeInfo

--                 combineNodeIds :: (Range, Text) -> RangeIdSetMap -> NodeInfo a -> RangeIdSetMap
--                 combineNodeIds rt ad (NodeInfo _ _ bd) = M.unionWith (<>) ad xs
--                     where xs = M.unionsWith (<>) $ map (M.fromList . getIdentifier rt) $ M.keys bd

--                 getIdentifier :: (Range, Text) -> Identifier -> [(Range, Set Identifier)]
--                 getIdentifier (ran, token) idt = case idt of
--                     Left _moduleName -> [(ran, S.singleton $ Left _moduleName)]
--                     Right name ->
--                         let occStrMaybe = case nameString name of
--                                     -- the generated selector name with {-# LANGUAGE DuplicateRecordFields #-}
--                                     '$':'s':'e':'l':':':xs -> Just $ takeWhile (/= ':') xs
--                                     ['$'] -> Just "$"
--                                     -- other generated names that should not be visible
--                                     '$':_ -> Nothing
--                                     ns -> Just ns
--                         in case occStrMaybe of
--                                 (Just occStr) | token == T.pack occStr -> [(ran, S.singleton $ Right name)]
--                                 _ -> []


splitRangeByText :: Text -> Range -> Maybe RangeSplitContext
splitRangeByText tk ran = do
    let (ran', tk') = case T.uncons tk of
                Just ('(', xs) -> (subOneRange ran, T.takeWhile (/= ')') xs)
                Just ('`', xs) -> (subOneRange ran, T.takeWhile (/= '`') xs)
                _ -> (ran, tk)
    let (prefix, tk'') = T.breakOnEnd "." tk'
    spr <- splitRange tk'' (fromIntegral $ Rope.utf16Length $ Rope.fromText prefix) ran'
    return $ RangeSplitContext tk ran spr

splitRange :: Text -> UInt -> Range -> Maybe SplitResult
splitRange tx n ran@(Range (Position l1 c1) (Position l2 c2))
    | l1 == l2, n <= 0 = Just $ NoSplit (tx, ran)
    | l1 == l2, n < fromIntegral (c2 - c1) = Just $ Split (tx, Range (Position l1 c1) (Position l1 (c1+n)), Range (Position l1 (c1+n)) (Position l1 c2))
    | otherwise = Nothing

subOneRange :: Range -> Range
subOneRange (Range (Position l1 c1) (Position l2 c2)) = Range (Position l1 (c1 + 1)) (Position l2 (c2 - 1))

nameString :: Name -> String
nameString = occNameString . nameOccName

