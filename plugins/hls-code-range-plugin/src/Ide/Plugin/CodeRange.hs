{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.CodeRange (
    descriptor
    , Log

    -- * Internal
    , findPosition
    , findFoldingRanges
    , createFoldingRange
    ) where

import           Control.Monad.Except                 (ExceptT (ExceptT),
                                                       mapExceptT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       maybeToExceptT)
import           Data.Either.Extra                    (maybeToEither)
import           Data.Maybe                           (fromMaybe)
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V
import           Development.IDE                      (Action, IdeAction,
                                                       IdeState (shakeExtras),
                                                       Range (Range), Recorder,
                                                       WithPriority,
                                                       cmapWithPrio, runAction,
                                                       runIdeAction,
                                                       toNormalizedFilePath',
                                                       uriToFilePath', use,
                                                       useWithStaleFast)
import           Development.IDE.Core.PositionMapping (PositionMapping,
                                                       fromCurrentPosition,
                                                       toCurrentRange)
import           Development.IDE.Types.Logger         (Pretty (..))
import           Ide.Plugin.CodeRange.Rules           (CodeRange (..),
                                                       GetCodeRange (..),
                                                       codeRangeRule, crkToFrk)
import qualified Ide.Plugin.CodeRange.Rules           as Rules (Log)
import           Ide.PluginUtils                      (pluginResponse,
                                                       positionInRange)
import           Ide.Types                            (PluginDescriptor (pluginHandlers, pluginRules),
                                                       PluginId,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Server                  (LspM, LspT)
import           Language.LSP.Types                   (FoldingRange (..),
                                                       FoldingRangeParams (..),
                                                       List (List),
                                                       NormalizedFilePath,
                                                       Position (..),
                                                       Range (_start),
                                                       ResponseError,
                                                       SMethod (STextDocumentFoldingRange, STextDocumentSelectionRange),
                                                       SelectionRange (..),
                                                       SelectionRangeParams (..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       Uri)
import           Prelude                              hiding (log, span)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentSelectionRange selectionRangeHandler
    <> mkPluginHandler STextDocumentFoldingRange foldingRangeHandler
    , pluginRules = codeRangeRule (cmapWithPrio LogRules recorder)
    }

data Log = LogRules Rules.Log

instance Pretty Log where
    pretty log = case log of
        LogRules codeRangeLog -> pretty codeRangeLog

foldingRangeHandler :: IdeState -> PluginId -> FoldingRangeParams -> LspM c (Either ResponseError (List FoldingRange))
foldingRangeHandler ide _ FoldingRangeParams{..} = do
    pluginResponse $ do
        filePath <- ExceptT . pure . maybeToEither "fail to convert uri to file path" $
                toNormalizedFilePath' <$> uriToFilePath' uri
        foldingRanges <- liftIO . runAction "FoldingRange" ide $
            getFoldingRanges filePath
        pure . List $ foldingRanges
    where
        uri :: Uri
        TextDocumentIdentifier uri = _textDocument

getFoldingRanges :: NormalizedFilePath -> Action [FoldingRange]
getFoldingRanges file = fmap (maybe [] findFoldingRanges) $ use GetCodeRange file

selectionRangeHandler :: IdeState -> PluginId -> SelectionRangeParams -> LspM c (Either ResponseError (List SelectionRange))
selectionRangeHandler ide _ SelectionRangeParams{..} = do
    pluginResponse $ do
        filePath <- ExceptT . pure . maybeToEither "fail to convert uri to file path" $
                toNormalizedFilePath' <$> uriToFilePath' uri
        fmap List . mapExceptT runIdeAction' . getSelectionRanges filePath $ positions
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    positions :: [Position]
    List positions = _positions

    runIdeAction' :: IdeAction (Either SelectionRangeError [SelectionRange]) -> LspT c IO (Either String [SelectionRange])
    runIdeAction' action = do
        result <- liftIO $ runIdeAction "SelectionRange" (shakeExtras ide) action
        pure $ case result of
            Left err   -> maybe (Right []) Left (showError err)
            Right list -> Right list

    showError :: SelectionRangeError -> Maybe String
    -- This might happen if the HieAst is not ready, so we give it a default value instead of throwing an error
    showError SelectionRangeBadDependency = Nothing
    showError SelectionRangeInputPositionMappingFailure = Just "failed to apply position mapping to input positions"
    showError SelectionRangeOutputPositionMappingFailure = Just "failed to apply position mapping to output positions"

data SelectionRangeError = SelectionRangeBadDependency
                         | SelectionRangeInputPositionMappingFailure
                         | SelectionRangeOutputPositionMappingFailure

getSelectionRanges :: NormalizedFilePath -> [Position] -> ExceptT SelectionRangeError IdeAction [SelectionRange]
getSelectionRanges file positions = do
    (codeRange, positionMapping) <- maybeToExceptT SelectionRangeBadDependency . MaybeT $
        useWithStaleFast GetCodeRange file
    -- 'positionMapping' should be appied to the input before using them
    positions' <- maybeToExceptT SelectionRangeInputPositionMappingFailure . MaybeT . pure $
        traverse (fromCurrentPosition positionMapping) positions

    let selectionRanges = flip fmap positions' $ \pos ->
            -- We need a default selection range if the lookup fails,
            -- so that other positions can still have valid results.
            let defaultSelectionRange = SelectionRange (Range pos pos) Nothing
             in fromMaybe defaultSelectionRange . findPosition pos $ codeRange

    -- 'positionMapping' should be applied to the output ranges before returning them
    maybeToExceptT SelectionRangeOutputPositionMappingFailure . MaybeT . pure $
         traverse (toCurrentSelectionRange positionMapping) selectionRanges

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
    drop 1 $ findFoldingRangesRec codeRange

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
    Just (FoldingRange lineStart (Just charStart) lineEnd (Just charEnd) (Just frk))

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }
