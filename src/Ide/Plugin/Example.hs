{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.Example
  (
    plugin
  ) where

import           Control.Concurrent.Extra
import Control.DeepSeq
import Control.Exception
import Data.Binary
import qualified Data.ByteString.UTF8 as BS
import Data.Functor
import Data.Hashable
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import qualified Data.Text as T
import Data.Tuple.Extra
import Data.Typeable
import Development.IDE.Core.OfInterest
import Development.IDE.Core.PositionMapping
import Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Service
import Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.GHC.Util
import Development.IDE.Import.DependencyInformation
import           Development.IDE.LSP.Server
import Development.IDE.LSP.Server
import Development.IDE.Plugin
import           Development.IDE.Types.Location
import Development.IDE.Types.Location
import           Development.IDE.Types.Logger
import Development.IDE.Types.Logger
import           Development.Shake
import GHC.Generics
import qualified Language.Haskell.LSP.Core       as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import  SrcLoc

-- ---------------------------------------------------------------------

plugin :: Plugin
plugin = Plugin exampleRules handlersExample

hover          :: IdeState -> TextDocumentPositionParams -> IO (Maybe Hover)
hover          = request "Hover"      blah     Nothing      foundHover

blah :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
blah _ (Position line col)
  = return $ Just (Just (Range (Position line col) (Position (line+1) 0)), ["example hover"])

handlersExample :: PartialHandlers
handlersExample = PartialHandlers $ \WithMessage{..} x ->
  return x{LSP.hoverHandler = withResponse RspHover $ const hover}


-- ---------------------------------------------------------------------

data Example = Example
    deriving (Eq, Show, Typeable, Generic)
instance Hashable Example
instance NFData   Example
instance Binary   Example

type instance RuleResult Example = ()

exampleRules = do
  define $ \Example file -> do
    getParsedModule file
    let diag = diagFromString "example" DsError (noSpan "<Internal>") "example diagnostic, hello world"
    return (diag, Just ())

  action $ do
    files <- getFilesOfInterest
    void $ uses Example $ Set.toList files
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
