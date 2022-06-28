{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.SelectionRange (descriptor, Log) where

import           Control.Monad.Except                 (ExceptT (ExceptT),
                                                       runExceptT)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Maybe            (MaybeT (MaybeT),
                                                       maybeToExceptT)
import           Data.Either.Extra                    (maybeToEither)
import           Data.Maybe                           (fromMaybe, mapMaybe)
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
            -- codeRange doesn't cover all portions of text in the file, so we need a default value
            let defaultSelectionRange = SelectionRange (Range pos pos) Nothing
             in reverseSelectionRange . fromMaybe defaultSelectionRange . findPosition' pos $ codeRange

    -- 'positionMapping' should be applied to the output ranges before returning them
    maybeToExceptT "fail to apply position mapping to output positions" . MaybeT . pure $
         traverse (toCurrentSelectionRange positionMapping) selectionRanges

-- Find 'Position' in 'CodeRange'. Producing an inverse 'SelectionRange'
findPosition' :: Position -> CodeRange -> Maybe SelectionRange
findPosition' pos (CodeRange range children) =
    if positionInRange pos range
    then Just $ case mapMaybe (findPosition' pos) children of
            [childSelectionRange] -> SelectionRange range (Just childSelectionRange)
            _                     -> SelectionRange range Nothing
    else Nothing

-- Reverse 'SelectionRange'. Just like 'reverse' for list.
reverseSelectionRange :: SelectionRange -> SelectionRange
reverseSelectionRange = go (SelectionRange invalidRange Nothing)
  where
    go :: SelectionRange -> SelectionRange -> SelectionRange
    go acc (SelectionRange r Nothing) = SelectionRange r (checkRange acc)
    go acc (SelectionRange r (Just parent)) = go (SelectionRange r (checkRange acc)) parent

    checkRange :: SelectionRange -> Maybe SelectionRange
    checkRange r@(SelectionRange range _) = if range == invalidRange then Nothing else Just r

    invalidRange :: Range
    invalidRange = Range (Position (-1) (-1)) (Position (-1) (-1))

-- | Likes 'toCurrentPosition', but works on 'SelectionRange'
toCurrentSelectionRange :: PositionMapping -> SelectionRange -> Maybe SelectionRange
toCurrentSelectionRange positionMapping SelectionRange{..} = do
    newRange <- toCurrentRange positionMapping _range
    pure $ SelectionRange {
        _range = newRange,
        _parent = _parent >>= toCurrentSelectionRange positionMapping
    }
