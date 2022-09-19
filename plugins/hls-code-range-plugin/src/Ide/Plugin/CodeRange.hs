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
                                                       runExceptT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       maybeToExceptT)
import           Data.Either.Extra                    (maybeToEither)
import           Data.Maybe                           (fromMaybe)
import           Data.Vector                          (Vector)
import qualified Data.Vector                          as V
import           Development.IDE                      (IdeAction,
                                                       IdeState (shakeExtras),
                                                       Range (Range), Recorder,
                                                       WithPriority,
                                                       cmapWithPrio,
                                                       runIdeAction,
                                                       toNormalizedFilePath',
                                                       uriToFilePath')
import           Development.IDE.Core.Actions         (useE)
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
import           Language.LSP.Server                  (LspM)
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
        foldingRanges <- ExceptT . liftIO . runIdeAction "FoldingRange" (shakeExtras ide) . runExceptT $
            getFoldingRanges filePath
        pure . List $ removeDupStartLineFoldings foldingRanges
    where
        uri :: Uri
        TextDocumentIdentifier uri = _textDocument

getFoldingRanges :: NormalizedFilePath -> ExceptT String IdeAction [FoldingRange]
getFoldingRanges file = do
    (codeRange, _) <- maybeToExceptT "fail to get code range" $ useE GetCodeRange file

    pure $ findFoldingRanges codeRange

selectionRangeHandler :: IdeState -> PluginId -> SelectionRangeParams -> LspM c (Either ResponseError (List SelectionRange))
selectionRangeHandler ide _ SelectionRangeParams{..} = do
    pluginResponse $ do
        filePath <- ExceptT . pure . maybeToEither "fail to convert uri to file path" $
                toNormalizedFilePath' <$> uriToFilePath' uri
        selectionRanges <- ExceptT . liftIO . runIdeAction "SelectionRange" (shakeExtras ide) . runExceptT $
            getSelectionRanges filePath positions
        pure . List $ selectionRanges
  where
    uri :: Uri
    TextDocumentIdentifier uri = _textDocument

    positions :: [Position]
    List positions = _positions

getSelectionRanges :: NormalizedFilePath -> [Position] -> ExceptT String IdeAction [SelectionRange]
getSelectionRanges file positions = do
    (codeRange, positionMapping) <- maybeToExceptT "fail to get code range" $ useE GetCodeRange file
    -- 'positionMapping' should be appied to the input before using them
    positions' <- maybeToExceptT "fail to apply position mapping to input positions" . MaybeT . pure $
        traverse (fromCurrentPosition positionMapping) positions

    let selectionRanges = flip fmap positions' $ \pos ->
            -- We need a default selection range if the lookup fails, so that other positions can still have valid results.
            let defaultSelectionRange = SelectionRange (Range pos pos) Nothing
             in fromMaybe defaultSelectionRange . findPosition pos $ codeRange

    -- 'positionMapping' should be applied to the output ranges before returning them
    maybeToExceptT "fail to apply position mapping to output positions" . MaybeT . pure $
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
findFoldingRanges :: CodeRange -> [FoldingRange]
findFoldingRanges r@(CodeRange _ children _) =
    let frChildren :: [FoldingRange] = concat $ V.toList $ fmap findFoldingRanges children
    in case createFoldingRange r of
        Just x  -> x:frChildren
        Nothing -> frChildren

-- | Parses code range to folding range
createFoldingRange :: CodeRange -> Maybe FoldingRange
createFoldingRange (CodeRange (Range (Position lineStart charStart) (Position lineEnd charEnd)) _ ck) = do
    -- Type conversion of codeRangeKind to FoldingRangeKind
    let frk = crkToFrk ck

    -- Filtering code ranges that start and end on the same line as need/can not be folded.
    --
    -- Eg. A single line function will also generate a Folding Range but it cannot be folded
    -- because it is already single line, so omiting it.
    if lineStart == lineEnd
        then Nothing
    else Just (FoldingRange lineStart (Just charStart) lineEnd (Just charEnd) (Just frk))

-- | Removes all small foldings that start from the same line.
--
-- As we are converting nodes of the ast into folding ranges, there are multiple nodes starting from a single line.
-- A single line of code doesn't mean a single node in AST, so this function removes all the nodes that have a duplicate
-- start line, ie. they start from the same line.
--
-- This function preserves the largest folding range from the ranges that start from the same line.
--
-- Eg. A multi-line function that also has a multi-line if statement starting from the same line should have the folding
-- according to the function.
--
-- This is done by breaking the [FoldingRange] into parts -->
--      frx: Head
--      xs: rest of the array
--      fry(not shown as it is not used): head of xs
--      xs2: rest of the array other than the first two elements
--      slx and sly: start line of frx and fry
--
-- We compare the start line of the first two elements in the array and if the start line is the same we remove the
-- second one as it is the smaller one amoung the two.
-- otherwise frx is returned and the function runs recursively on xs.
removeDupStartLineFoldings :: [FoldingRange] -> [FoldingRange]
removeDupStartLineFoldings [] = []
removeDupStartLineFoldings [x] = [x]
removeDupStartLineFoldings (frx@(FoldingRange slx _ _ _ _):xs@((FoldingRange sly _ _ _ _):xs2))
    | slx == sly = removeDupStartLineFoldings (frx:xs2)
    | otherwise = frx : removeDupStartLineFoldings xs

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }
