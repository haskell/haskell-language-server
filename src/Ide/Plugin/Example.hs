{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Example
  (
    plugin
  ) where
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Service
import           Development.IDE.LSP.Server
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import           Development.Shake
import qualified Language.Haskell.LSP.Core       as LSP

import Development.IDE.Plugin
import Development.IDE.Core.Service
import Development.IDE.Types.Location
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Util
import Development.IDE.LSP.Server
import Development.IDE.Import.DependencyInformation

import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types

import qualified Data.Text as T

-- ---------------------------------------------------------------------

plugin :: Plugin
plugin = Plugin mempty handlersExample

hover          :: IdeState -> TextDocumentPositionParams -> IO (Maybe Hover)
hover          = request "Hover"      blah     Nothing      foundHover

blah :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
blah _ (Position line col)
  = return $ Just (Just (Range (Position line col) (Position (line+1) 0)), ["example hover"])

handlersExample :: PartialHandlers
handlersExample = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler = withResponse RspHover $ const hover}


-- ---------------------------------------------------------------------
foundHover :: (Maybe Range, [T.Text]) -> Maybe Hover
foundHover (mbRange, contents) =
  Just $ Hover (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents) mbRange


-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> Action (Maybe a))
  -> b
  -> (a -> b)
  -> IdeState
  -> TextDocumentPositionParams
  -> IO b
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos _) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b) -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runAction ide $ getResults filePath pos
