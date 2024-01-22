{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Ide.Plugin.SemanticTokens.Tokenize (hieAstSpanIdentifiers) where

import Control.Monad (forM_, guard)
import Control.Monad.State (MonadState (get), MonadTrans (lift), State, evalStateT, execState, execStateT, gets, modify, put, runStateT)
import Control.Monad.Trans.State (StateT)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Rope as Char
import Data.Text.Utf16.Rope (toText)
import qualified Data.Text.Utf16.Rope as Utf16
import Data.Text.Utf16.Rope.Mixed (Rope)
import qualified Data.Text.Utf16.Rope.Mixed as Rope
import Debug.Trace (trace, traceShow, traceShowM)
import Development.IDE (pretty)
import Development.IDE.GHC.Compat
import Development.IDE.GHC.Error (realSrcSpanToCodePointRange)
import Ide.Plugin.SemanticTokens.Utils (rangeShortStr, showIdentifier)
import Language.LSP.Protocol.Types
  ( Position (Position),
    Range (Range),
    UInt,
  )
import Language.LSP.VFS hiding (line)
import Prelude hiding (length, span)

type RangeIdSetMap = Map.Map Range (Set Identifier)

type TokenState = (RangeIdSetMap, Rope, Char.Position)

data PTokenState t = PTokenState
  { rangeIdSetMap :: RangeIdSetMap,
    rope :: Rope,
    cursor :: Char.Position,
    currentAst :: HieAST t,
    columnsInUtf16 :: UInt,
    currentRange :: Range,
    currentRangeContext :: SplitResult
  }

data SplitResult
  = NoSplit (Text, Range)
  | -- token text, prefix range(module range), token range
    Split (Text, Range, Range)
  deriving (Show)

startRange :: Range
startRange = Range (Position 0 0) (Position 0 0)

mkPTokenState :: VirtualFile -> HieAST a -> PTokenState a
mkPTokenState vf ast =
  PTokenState
    { rangeIdSetMap = mempty,
      rope = Rope.fromText $ toText vf._file_text,
      cursor = Char.Position 0 0,
      currentAst = ast,
      columnsInUtf16 = 0,
      currentRange = startRange,
      currentRangeContext = NoSplit ("", startRange)
    }

type Parser m a = forall t. StateT (PTokenState t) m a

updateCursor :: (Monad m) => Char.Position -> Parser m ()
updateCursor pos = modify $ \s -> s {cursor = pos}

updateRope :: (Monad m) => Rope -> Parser m ()
updateRope r = modify $ \s -> s {rope = r}

insertRangeIdSetMap :: (Monad m) => Range -> Set Identifier -> Parser m ()
insertRangeIdSetMap r si = modify $ \s -> s {rangeIdSetMap = Map.insertWith (<>) r si $ rangeIdSetMap s}

addRangeIdSetMap :: (Monad m) => Range -> Identifier -> Parser m ()
addRangeIdSetMap r i = insertRangeIdSetMap r $ S.singleton i

updateColumnsInUtf16 :: (Monad m) => UInt -> Parser m ()
updateColumnsInUtf16 n = modify $ \s -> s {columnsInUtf16 = n}

maybeM :: (Monad m) => Parser Maybe () -> Parser m ()
maybeM p = do
  st <- get
  forM_ (execStateT p st) put

foldAst :: (Monad m) => Parser m ()
foldAst = do
  ast <- gets currentAst
  if null (nodeChildren ast)
    then maybeM visitLeafIds
    else do
      let children = nodeChildren ast
      mapM_ (\x -> (modify $ \s -> s {currentAst = x}) >> foldAst) children

hieAstSpanIdentifiers :: VirtualFile -> HieAST a -> RangeIdSetMap
hieAstSpanIdentifiers vf ast = rangeIdSetMap $ execState foldAst (mkPTokenState vf ast)

codePointRangeToRangeWith :: UInt -> UInt -> CodePointRange -> Range
codePointRangeToRangeWith newStartCol newEndCol (CodePointRange (CodePointPosition startLine _) (CodePointPosition endLine _)) =
  Range (Position startLine newStartCol) (Position endLine newEndCol)

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
    combineNodeIds :: (Monad m) => NodeInfo a -> Parser m ()
    combineNodeIds (NodeInfo _ _ bd) = mapM_ getIdentifier (M.keys bd)
    getIdentifier :: (Monad m) => Identifier -> Parser m ()
    getIdentifier idt = maybeM $ do
      ran <- gets currentRange
      case idt of
        Left _moduleName -> addRangeIdSetMap ran idt
        Right name -> do
          ranSplit <- gets currentRangeContext
          occStr <- lift $ case nameString name of
            -- the generated selector name with {-# LANGUAGE DuplicateRecordFields #-}
            '$' : 's' : 'e' : 'l' : ':' : xs -> Just $ takeWhile (/= ':') xs
            ['$'] -> Just "$"
            -- other generated names that should not be visible
            '$' : _ -> Nothing
            ns -> Just ns
          case ranSplit of
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
  let (_gap, startRope) = Rope.charSplitAtPosition s rp
  (token, remains) <- charSplitAtMaybe length startRope
  return (Rope.toText _gap, Rope.toText token, remains)

charSplitAtMaybe :: Word -> Rope -> Maybe (Rope, Rope)
charSplitAtMaybe len rp = do
  let (prefix, suffix) = Rope.charSplitAt len rp
  guard $ Rope.charLength prefix == len
  return (prefix, suffix)

srcSpanMaybePositionLength :: (Integral l) => RealSrcSpan -> Maybe (Char.Position, l)
srcSpanMaybePositionLength real =
  return
    ( realSrcLocRopePosition $ realSrcSpanStart real,
      fromIntegral $ (srcLocCol $ realSrcSpanEnd real) - (srcLocCol $ realSrcSpanStart real)
    )

realSrcLocRopePosition :: RealSrcLoc -> Char.Position
realSrcLocRopePosition real = Char.Position (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real - 1)

sub :: Char.Position -> Char.Position -> Char.Position
sub (Char.Position l1 c1) (Char.Position l2 c2) =
  if l1 == l2 then Char.Position 0 (c1 - c2) else Char.Position (l1 - l2) c1

srcSpanEndCharPosition :: RealSrcSpan -> Char.Position
srcSpanEndCharPosition real = realSrcLocRopePosition $ realSrcSpanEnd real

splitRangeByText :: Text -> Range -> Maybe SplitResult
splitRangeByText tk ran = do
  let (ran', tk') = case T.uncons tk of
        Just ('(', xs) -> (subOneRange ran, T.takeWhile (/= ')') xs)
        Just ('`', xs) -> (subOneRange ran, T.takeWhile (/= '`') xs)
        _ -> (ran, tk)
  let (prefix, tk'') = T.breakOnEnd "." tk'
  spr <- splitRange tk'' (fromIntegral $ Rope.utf16Length $ Rope.fromText prefix) ran'
  return spr

splitRange :: Text -> UInt -> Range -> Maybe SplitResult
splitRange tx n ran@(Range (Position l1 c1) (Position l2 c2))
  | l1 == l2, n <= 0 = Just $ NoSplit (tx, ran)
  | l1 == l2, n < fromIntegral (c2 - c1) = Just $ Split (tx, Range (Position l1 c1) (Position l1 (c1 + n)), Range (Position l1 (c1 + n)) (Position l1 c2))
  | otherwise = Nothing

subOneRange :: Range -> Range
subOneRange (Range (Position l1 c1) (Position l2 c2)) = Range (Position l1 (c1 + 1)) (Position l2 (c2 - 1))

nameString :: Name -> String
nameString = occNameString . nameOccName
