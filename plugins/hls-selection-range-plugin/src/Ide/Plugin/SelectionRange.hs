{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.SelectionRange (descriptor, Log) where

import           Control.Monad.Except                 (ExceptT (ExceptT),
                                                       runExceptT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       maybeToExceptT)
import           Data.Either.Extra                    (maybeToEither)
import           Data.List.NonEmpty                   (NonEmpty)
import qualified Data.List.NonEmpty                   as NonEmpty
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
import           Ide.Plugin.SelectionRange.CodeRange  (CodeRange (..),
                                                       GetCodeRange (..),
                                                       codeRangeRule)
import qualified Ide.Plugin.SelectionRange.CodeRange  as CodeRange
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
                                                       SMethod (STextDocumentSelectionRange),
                                                       SelectionRange (..),
                                                       SelectionRangeParams (..),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       Uri)
import           Prelude                              hiding (log, span)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentSelectionRange selectionRangeHandler
    -- TODO @sloorush add folding range
    -- <> mkPluginHandler STextDocumentFoldingRange foldingRangeHandler
    , pluginRules = codeRangeRule (cmapWithPrio LogCodeRange recorder)
    }

data Log = LogCodeRange CodeRange.Log

instance Pretty Log where
    pretty log = case log of
        LogCodeRange codeRangeLog -> pretty codeRangeLog

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
            -- 'codeRange' may not cover all portions of text in the file, we need a default value to make sure
            -- other positions can still work.
            let defaultSelectionRange = SelectionRange (Range pos pos) Nothing
             in fromMaybe defaultSelectionRange . findPosition pos $ codeRange

    -- 'positionMapping' should be applied to the output ranges before returning them
    maybeToExceptT "fail to apply position mapping to output positions" . MaybeT . pure $
         traverse (toCurrentSelectionRange positionMapping) selectionRanges

-- | Find 'Position' in 'CodeRange'.
findPosition :: Position -> CodeRange -> Maybe SelectionRange
findPosition pos root =
    selectionRangeFromNonEmpty . NonEmpty.reverse -- SelectionRange requires a bottom-up order, so we need to reverse
    <$> go root
  where
    -- Helper function for recursion. The range list is built top-down
    go :: CodeRange -> Maybe (NonEmpty Range)
    go node =
        if positionInRange pos range
        then case binarySearchPos children of
            Just childContainingPos -> fmap (range NonEmpty.<|) (go childContainingPos)
            Nothing -> Just $ range NonEmpty.:| [] -- NonEmpty.singleton doesn't exist in GHC 8.8.4
        else Nothing
      where
        range = _codeRange_range node
        children = _codeRange_children node

    binarySearchPos :: Vector CodeRange -> Maybe CodeRange
    binarySearchPos v
        | V.null v = Nothing
        | V.length v == 1,
            Just r <- V.headM v = if positionInRange pos (_codeRange_range r) then Just r else Nothing
        | otherwise = do
            let (left, right) = V.splitAt (V.length v `div` 2) v
            startOfRight <- _start . _codeRange_range <$> V.headM right
            if pos < startOfRight then binarySearchPos left else binarySearchPos right

-- | Construct 'SelectionRange' from 'NonEmpty' 'Range'
selectionRangeFromNonEmpty :: NonEmpty Range -> SelectionRange
selectionRangeFromNonEmpty ranges
    | (r, remaining) <- NonEmpty.uncons ranges =
        SelectionRange r (fmap selectionRangeFromNonEmpty remaining)

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }
