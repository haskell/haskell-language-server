{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}

module Ide.Plugin.SemanticTokens.Tokenize (hieAstSpanIdentifiers) where

import           Control.Lens                (Identity (runIdentity))
import           Control.Monad               (forM_, guard)
import           Control.Monad.State         (MonadState (get),
                                              MonadTrans (lift), execStateT,
                                              gets, modify, put)
import           Control.Monad.Trans.State   (StateT)
import qualified Data.Map                    as M
import qualified Data.Map                    as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Rope              as Char
import           Data.Text.Utf16.Rope        (toText)
import           Data.Text.Utf16.Rope.Mixed  (Rope)
import qualified Data.Text.Utf16.Rope.Mixed  as Rope
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error   (realSrcSpanToCodePointRange)
import           Language.LSP.Protocol.Types (Position (Position),
                                              Range (Range), UInt)
import           Language.LSP.VFS            hiding (line)
import           Prelude                     hiding (length, span)

type Tokenizer m a = forall t. StateT (PTokenState t) m a

type RangeIdSetMap = Map.Map Range (Set Identifier)

data PTokenState t = PTokenState
  { rangeIdSetMap       :: RangeIdSetMap,
    rope                :: Rope,
    cursor              :: Char.Position,
    currentAst          :: HieAST t,
    columnsInUtf16      :: UInt,
    currentRange        :: Range,
    currentRangeContext :: SplitResult
  }

runTokenizer :: (Monad m) => Tokenizer m a -> PTokenState t -> m RangeIdSetMap
runTokenizer p st = rangeIdSetMap <$> execStateT p st

data SplitResult
  = NoSplit (Text, Range) -- does not need to split, token text, token range
  | Split (Text, Range, Range) -- token text, prefix range(module range), token range
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

updateCursor :: (Monad m) => Char.Position -> Tokenizer m ()
updateCursor pos = modify $ \s -> s {cursor = pos}

updateRope :: (Monad m) => Rope -> Tokenizer m ()
updateRope r = modify $ \s -> s {rope = r}

addRangeIdSetMap :: (Monad m) => Range -> Identifier -> Tokenizer m ()
addRangeIdSetMap r i = modify $ \s -> s {rangeIdSetMap = Map.insertWith (<>) r (S.singleton i) $ rangeIdSetMap s}

updateColumnsInUtf16 :: (Monad m) => UInt -> Tokenizer m ()
updateColumnsInUtf16 n = modify $ \s -> s {columnsInUtf16 = n}

liftMaybeM :: (Monad m) => Tokenizer Maybe () -> Tokenizer m ()
liftMaybeM p = do
  st <- get
  forM_ (execStateT p st) put

hieAstSpanIdentifiers :: VirtualFile -> HieAST a -> RangeIdSetMap
hieAstSpanIdentifiers vf ast = runIdentity $ runTokenizer foldAst (mkPTokenState vf ast)

-- | foldAst
-- visit every leaf node in the ast in depth first order
foldAst :: (Monad m) => Tokenizer m ()
foldAst = do
  ast <- gets currentAst
  if null (nodeChildren ast)
    then liftMaybeM visitLeafIds
    else do
      let children = nodeChildren ast
      mapM_ (\x -> (modify $ \s -> s {currentAst = x}) >> foldAst) children

visitLeafIds :: Tokenizer Maybe ()
visitLeafIds = liftMaybeM $ do
  leaf <- gets currentAst
  (ran, token) <- focusTokenAt leaf
  splitResult <- lift $ splitRangeByText token ran
  modify $ \s -> s {currentRange = ran, currentRangeContext = splitResult}
  mapM_ combineNodeIds $ Map.filterWithKey (\k _ -> k == SourceInfo) $ getSourcedNodeInfo $ sourcedNodeInfo leaf
  where
    combineNodeIds :: (Monad m) => NodeInfo a -> Tokenizer m ()
    combineNodeIds (NodeInfo _ _ bd) = mapM_ getIdentifier (M.keys bd)
    getIdentifier :: (Monad m) => Identifier -> Tokenizer m ()
    getIdentifier idt = liftMaybeM $ do
      ran <- gets currentRange
      case idt of
        Left _moduleName -> addRangeIdSetMap ran idt
        Right name -> do
          ranSplit <- gets currentRangeContext
          occStr <- lift $ case (occNameString . nameOccName) name of
            -- the generated selector name with {-# LANGUAGE DuplicateRecordFields #-}
            '$' : 's' : 'e' : 'l' : ':' : xs -> Just $ takeWhile (/= ':') xs
            ['$']                            -> Just "$"
            -- other generated names that should not be visible
            '$' : _                          -> Nothing
            ns                               -> Just ns
          case ranSplit of
            (NoSplit (tk, r)) -> do
              guard $ T.unpack tk == occStr
              addRangeIdSetMap r idt
            (Split (tk, r1, r2)) -> do
              guard $ T.unpack tk == occStr
              addRangeIdSetMap r1 (Left $ mkModuleName "")
              addRangeIdSetMap r2 idt

focusTokenAt ::
  -- | leaf node we want to focus on
  HieAST a ->
  -- | (token, remains)
  Tokenizer Maybe (Range, Text)
focusTokenAt leaf = do
  rp <- gets rope
  cur <- gets cursor
  cs <- gets columnsInUtf16
  let span = nodeSpan leaf
  (startPos, length) <- lift $ srcSpanMaybePositionLength span
  let (gap, startRope) = Rope.charSplitAtPosition (startPos `sub` cur) rp
  (token, remains) <- lift $ charSplitAtMaybe length startRope
  let tokenText = Rope.toText token
  let ncs = newColumn cs $ Rope.toText gap
  let nce = newColumn ncs tokenText
  -- compute the new range for utf16
  let ran = codePointRangeToRangeWith ncs nce $ realSrcSpanToCodePointRange span
  updateColumnsInUtf16 nce
  updateRope remains
  updateCursor $ srcSpanEndCharPosition span
  return (ran, tokenText)
  where
    srcSpanMaybePositionLength :: (Integral l) => RealSrcSpan -> Maybe (Char.Position, l)
    srcSpanMaybePositionLength real =
      return
        ( realSrcLocRopePosition $ realSrcSpanStart real,
          fromIntegral $ (srcLocCol $ realSrcSpanEnd real) - (srcLocCol $ realSrcSpanStart real)
        )
    charSplitAtMaybe :: Word -> Rope -> Maybe (Rope, Rope)
    charSplitAtMaybe len rpe = do
      let (prefix, suffix) = Rope.charSplitAt len rpe
      guard $ Rope.charLength prefix == len
      return (prefix, suffix)
    sub :: Char.Position -> Char.Position -> Char.Position
    sub (Char.Position l1 c1) (Char.Position l2 c2) =
      if l1 == l2 then Char.Position 0 (c1 - c2) else Char.Position (l1 - l2) c1
    realSrcLocRopePosition :: RealSrcLoc -> Char.Position
    realSrcLocRopePosition real = Char.Position (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real - 1)
    srcSpanEndCharPosition :: RealSrcSpan -> Char.Position
    srcSpanEndCharPosition real = realSrcLocRopePosition $ realSrcSpanEnd real
    newColumn :: UInt -> Text -> UInt
    newColumn n rp = case T.breakOnEnd "\n" rp of
      ("", nEnd) -> n + fromIntegral (Rope.utf16Length $ Rope.fromText nEnd)
      (_, nEnd)  -> fromIntegral (Rope.utf16Length $ Rope.fromText nEnd)
    codePointRangeToRangeWith :: UInt -> UInt -> CodePointRange -> Range
    codePointRangeToRangeWith newStartCol newEndCol (CodePointRange (CodePointPosition startLine _) (CodePointPosition endLine _)) =
      Range (Position startLine newStartCol) (Position endLine newEndCol)

-- | splitRangeByText
-- split a qualified identifier into module name and identifier and/or strip the (), ``
-- for `ModuleA.b`, break it into `ModuleA.` and `b`
-- for `(b)`, strip `()`, and get `b`
-- for `(ModuleA.b)`, strip `()` and break it into `ModuleA.` and `b`
-- nameLength get the length of the `b` in code points unit
-- while Range might not be in code points unit.
-- but the comparison is still valid since we only want to know if it is potentially a qualified identifier
-- or an identifier that is wrapped in () or ``
splitRangeByText :: Text -> Range -> Maybe SplitResult
splitRangeByText tk ran = do
  let (ran', tk') = case T.uncons tk of
        Just ('(', xs) -> (subOneRange ran, T.takeWhile (/= ')') xs)
        Just ('`', xs) -> (subOneRange ran, T.takeWhile (/= '`') xs)
        _              -> (ran, tk)
  let (prefix, tk'') = T.breakOnEnd "." tk'
  spr <- splitRange tk'' (fromIntegral $ Rope.utf16Length $ Rope.fromText prefix) ran'
  return spr
  where
    splitRange :: Text -> UInt -> Range -> Maybe SplitResult
    splitRange tx n r@(Range (Position l1 c1) (Position l2 c2))
      | l1 == l2, n <= 0 = Just $ NoSplit (tx, r)
      | l1 == l2, n < fromIntegral (c2 - c1) = Just $ Split (tx, Range (Position l1 c1) (Position l1 (c1 + n)), Range (Position l1 (c1 + n)) (Position l1 c2))
      | otherwise = Nothing
    subOneRange :: Range -> Range
    subOneRange (Range (Position l1 c1) (Position l2 c2)) = Range (Position l1 (c1 + 1)) (Position l2 (c2 - 1))
