{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.CodeRange (
    descriptor
    , Log

    -- * Internal
    , findPosition
    ) where

import           Control.Monad.Except                 (ExceptT (ExceptT),
                                                       runExceptT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       maybeToExceptT)
import           Data.Either.Extra                    (maybeToEither)
import Data.Maybe ( fromMaybe, catMaybes )
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
import           Language.LSP.Types                   (List (List),
                                                       NormalizedFilePath,
                                                       Position (..),
                                                       Range (_start),
                                                       ResponseError,
                                                       SMethod (STextDocumentSelectionRange, STextDocumentFoldingRange),
                                                       SelectionRange (..),
                                                       SelectionRangeParams (..),
                                                       FoldingRange (..),
                                                       FoldingRangeParams(..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       Uri
                                                       )
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
        pure . List $ foldingRanges
    where
        uri :: Uri
        TextDocumentIdentifier uri = _textDocument

getFoldingRanges :: NormalizedFilePath -> ExceptT String IdeAction [FoldingRange]
getFoldingRanges file = do
    (codeRange, _) <- maybeToExceptT "fail to get code range" $ useE GetCodeRange file

    let foldingRanges = catMaybes $ findFoldingRanges codeRange

    maybeToExceptT "Fail to generate folding range" (MaybeT . pure $ Just foldingRanges)

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

findFoldingRanges :: CodeRange -> [Maybe FoldingRange]
findFoldingRanges root = do
    let acc = []
    let node = _codeRange_children root
    binarySearchFoldingRange node acc

binarySearchFoldingRange :: Vector CodeRange -> [Maybe FoldingRange] -> [Maybe FoldingRange]
binarySearchFoldingRange v acc
    | V.null v = []
    | V.length v == 1 = do
        headOfCodeRange <- V.headM v
        let foldingRangesOfRange = createFoldingRange headOfCodeRange
        let acc' = acc ++ [foldingRangesOfRange]
        acc'
    | otherwise = do
        let (left, right) = V.splitAt (V.length v `div` 2) v
        _ <- binarySearchFoldingRange left acc
        binarySearchFoldingRange right acc

createFoldingRange :: CodeRange -> Maybe FoldingRange
createFoldingRange node1 = do
    let range = _codeRange_range node1
    let Range startPos endPos = range
    let Position lineStart _= startPos
    let Position lineEnd _ = endPos
    let codeRangeKind = _codeRange_kind node1

    let frk = crkToFrk codeRangeKind

    case frk of
        Just _ -> Just (FoldingRange lineStart Nothing lineEnd Nothing frk)
        Nothing -> Nothing

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }
