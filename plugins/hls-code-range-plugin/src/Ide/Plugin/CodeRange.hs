{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
module Ide.Plugin.CodeRange (
    descriptor
    , Log

    -- * Internal
    , findPosition
    , findFoldingRanges
    , createFoldingRange
    ) where

import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       maybeToExceptT)
import           Data.List.Extra                      (drop1)
import           Data.Maybe                           (fromMaybe)
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V
import           Development.IDE                      (Action, IdeAction,
                                                       IdeState (shakeExtras),
                                                       Range (Range), Recorder,
                                                       WithPriority,
                                                       cmapWithPrio)
import qualified Development.IDE.Core.PluginUtils     as PluginUtils
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       fromCurrentPosition,
                                                       toCurrentRange)
import           Development.IDE.Types.Logger         (Pretty (..),
                                                       Priority (Warning),
                                                       logWith)
import           Ide.Plugin.CodeRange.Rules           (CodeRange (..),
                                                       GetCodeRange (..),
                                                       codeRangeRule, crkToFrk)
import qualified Ide.Plugin.CodeRange.Rules           as Rules (Log)
import           Ide.Plugin.Error
import           Ide.PluginUtils                      (getNormalizedFilePath',
                                                       positionInRange)
import           Ide.Types                            (PluginDescriptor (pluginHandlers, pluginRules),
                                                       PluginId,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Protocol.Message        (ResponseError,
                                                       SMethod (SMethod_TextDocumentFoldingRange, SMethod_TextDocumentSelectionRange))
import           Language.LSP.Protocol.Types          (FoldingRange (..),
                                                       FoldingRangeParams (..),
                                                       NormalizedFilePath, Null,
                                                       Position (..),
                                                       Range (_start),
                                                       SelectionRange (..),
                                                       SelectionRangeParams (..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       Uri, type (|?) (InL))
import           Language.LSP.Server                  (LspM, LspT)
import           Prelude                              hiding (log, span)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentSelectionRange (selectionRangeHandler recorder)
    <> mkPluginHandler SMethod_TextDocumentFoldingRange (foldingRangeHandler recorder)
    , pluginRules = codeRangeRule (cmapWithPrio LogRules recorder)
    }

data Log = LogRules Rules.Log
         | forall rule. Show rule => LogBadDependency rule

instance Pretty Log where
    pretty log = case log of
        LogRules codeRangeLog -> pretty codeRangeLog
        LogBadDependency rule -> pretty $ "bad dependency: " <> show rule

foldingRangeHandler :: Recorder (WithPriority Log) -> IdeState -> PluginId -> FoldingRangeParams -> LspM c (Either ResponseError ([FoldingRange] |? Null))
foldingRangeHandler recorder ide _ FoldingRangeParams{..} =
    pluginResponseM handleErrors $ do
        filePath <- getNormalizedFilePath' uri
        foldingRanges <- PluginUtils.runAction "FoldingRange" ide $ getFoldingRanges filePath
        pure . InL $ foldingRanges
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    handleErrors = \case
        RuleFailed rule -> do
            logWith recorder Warning $ LogBadDependency rule
            pure $ Right $ InL []
        errs -> pure $ Left $ handlePluginError errs

getFoldingRanges :: NormalizedFilePath -> ExceptT PluginError Action [FoldingRange]
getFoldingRanges file = do
    codeRange <- PluginUtils.use GetCodeRange file
    pure $ findFoldingRanges codeRange

selectionRangeHandler :: Recorder (WithPriority Log) -> IdeState -> PluginId -> SelectionRangeParams -> LspM c (Either ResponseError ([SelectionRange] |? Null))
selectionRangeHandler recorder ide _ SelectionRangeParams{..} = do
    pluginResponseM handleErrors $ do
        filePath <- withError GhcidePluginErrors $ getNormalizedFilePath' uri
        fmap  id . runIdeAction' $ getSelectionRanges filePath positions
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    positions :: [Position]
    positions = _positions

    runIdeAction' :: MonadIO m => ExceptT SelectionRangeError IdeAction ([SelectionRange] |? Null) -> ExceptT SelectionRangeError m ([SelectionRange] |? Null)
    runIdeAction' action = PluginUtils.runIdeAction "SelectionRange" (shakeExtras ide) action

    handleErrors ::
        MonadIO m =>
        SelectionRangeError ->
        m (Either ResponseError ([a] |? Null))
    handleErrors err = case err of
        SelectionRangeBadDependency rule -> do
            logWith recorder Warning $ LogBadDependency rule
            -- This might happen if the HieAst is not ready,
            -- so we give it a default value instead of throwing an error
            pure $ Right $ InL []
        SelectionRangeInputPositionMappingFailure ->
            pure $ Left $ mkSimpleResponseError "failed to apply position mapping to input positions"
        SelectionRangeOutputPositionMappingFailure ->
            pure $ Left $ mkSimpleResponseError "failed to apply position mapping to output positions"
        GhcidePluginErrors ghcidePluginError ->
            pure $ Left $ handlePluginError ghcidePluginError


data SelectionRangeError = forall rule. Show rule => SelectionRangeBadDependency rule
                         | SelectionRangeInputPositionMappingFailure
                         | SelectionRangeOutputPositionMappingFailure
                         | GhcidePluginErrors PluginError

getSelectionRanges :: NormalizedFilePath -> [Position] -> ExceptT SelectionRangeError IdeAction ([SelectionRange] |? Null)
getSelectionRanges file positions = do
    (codeRange, positionMapping) <- withError (\_ -> SelectionRangeBadDependency GetCodeRange) $
        PluginUtils.useWithStaleFast GetCodeRange file
    -- 'positionMapping' should be applied to the input before using them
    positions' <- maybeToExceptT SelectionRangeInputPositionMappingFailure . MaybeT . pure $
        traverse (fromCurrentPosition positionMapping) positions

    let selectionRanges = flip fmap positions' $ \pos ->
            -- We need a default selection range if the lookup fails,
            -- so that other positions can still have valid results.
            let defaultSelectionRange = SelectionRange (Range pos pos) Nothing
             in fromMaybe defaultSelectionRange . findPosition pos $ codeRange

    -- 'positionMapping' should be applied to the output ranges before returning them
    maybeToExceptT SelectionRangeOutputPositionMappingFailure . MaybeT . pure $
        InL <$> traverse (toCurrentSelectionRange positionMapping) selectionRanges

-- | Find 'Position' in 'CodeRange'. This can fail, if the given position is not covered by the 'CodeRange'.
findPosition :: Position -> CodeRange -> Maybe SelectionRange
findPosition pos root = go Nothing root
  where
    -- Helper function for recursion. The range list is built top-down
    go :: Maybe SelectionRange -> CodeRange -> Maybe SelectionRange
    go acc node =
        if positionInRange pos range
        then maybe acc' (go acc') (binarySearchPos children)
        -- If all children doesn't contain pos, acc' will be returned.
        -- acc' will be Nothing only if we are in the root level.
        else Nothing
      where
        range = _codeRange_range node
        children = _codeRange_children node
        acc' = Just $ maybe (SelectionRange range Nothing) (SelectionRange range . Just) acc

    binarySearchPos :: Vector CodeRange -> Maybe CodeRange
    binarySearchPos v
        | V.null v = Nothing
        | V.length v == 1,
            Just r <- V.headM v = if positionInRange pos (_codeRange_range r) then Just r else Nothing
        | otherwise = do
            let (left, right) = V.splitAt (V.length v `div` 2) v
            startOfRight <- _start . _codeRange_range <$> V.headM right
            if pos < startOfRight then binarySearchPos left else binarySearchPos right

-- | Traverses through the code range and it children to a folding ranges.
--
-- It starts with the root node, converts that into a folding range then moves towards the children.
-- It converts each child of each root node and parses it to folding range and moves to its children.
--
-- Two cases to that are assumed to be taken care on the client side are:
--
--      1. When a folding range starts and ends on the same line, it is upto the client if it wants to
--      fold a single line folding or not.
--
--      2. As we are converting nodes of the ast into folding ranges, there are multiple nodes starting from a single line.
--      A single line of code doesn't mean a single node in AST, so this function removes all the nodes that have a duplicate
--      start line, ie. they start from the same line.
--      Eg. A multi-line function that also has a multi-line if statement starting from the same line should have the folding
--      according to the function.
--
-- We think the client can handle this, if not we could change to remove these in future
--
-- Discussion reference: https://github.com/haskell/haskell-language-server/pull/3058#discussion_r973737211
findFoldingRanges :: CodeRange -> [FoldingRange]
findFoldingRanges codeRange =
    -- removing the first node because it folds the entire file
    drop1 $ findFoldingRangesRec codeRange

findFoldingRangesRec :: CodeRange -> [FoldingRange]
findFoldingRangesRec r@(CodeRange _ children _) =
    let frChildren :: [FoldingRange] = concat $ V.toList $ fmap findFoldingRangesRec children
    in case createFoldingRange r of
        Just x  -> x:frChildren
        Nothing -> frChildren

-- | Parses code range to folding range
createFoldingRange :: CodeRange -> Maybe FoldingRange
createFoldingRange (CodeRange (Range (Position lineStart charStart) (Position lineEnd charEnd)) _ ck) = do
    -- Type conversion of codeRangeKind to FoldingRangeKind
    let frk = crkToFrk ck
    Just (FoldingRange lineStart (Just charStart) lineEnd (Just charEnd) (Just frk) Nothing)

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }
