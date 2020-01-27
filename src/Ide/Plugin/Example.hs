{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.Example
  (
    plugin
  ) where

import Control.DeepSeq ( NFData )
import Data.Binary
import Data.Functor
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import qualified Data.Set                     as Set
import qualified Data.Text as T
import Data.Typeable
import Development.IDE.Core.OfInterest
import Development.IDE.Core.Rules
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.LSP.Server
import Development.IDE.Plugin
import Development.IDE.Types.Diagnostics as D
import Development.IDE.Types.Location
import Development.IDE.Types.Logger
import Development.Shake hiding ( Diagnostic )
import GHC.Generics
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

plugin :: Plugin
plugin = Plugin exampleRules handlersExample <> codeActionPlugin codeAction

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

exampleRules :: Rules ()
exampleRules = do
  define $ \Example file -> do
    _pm <- getParsedModule file
    let diag = mkDiag file "example" DsError (Range (Position 0 0) (Position 1 0)) "example diagnostic, hello world"
    return ([diag], Just ())

  action $ do
    files <- getFilesOfInterest
    void $ uses Example $ Set.toList files

mkDiag :: NormalizedFilePath
       -> DiagnosticSource
       -> DiagnosticSeverity
       -> Range
       -> T.Text
       -> FileDiagnostic
mkDiag file diagSource sev loc msg = (file, D.ShowDiag,)
    Diagnostic
    { _range    = loc
    , _severity = Just sev
    , _source   = Just diagSource
    , _message  = msg
    , _code     = Nothing
    , _relatedInformation = Nothing
    }

-- ---------------------------------------------------------------------

-- | Generate code actions.
codeAction
    :: LSP.LspFuncs ()
    -> IdeState
    -> TextDocumentIdentifier
    -> Range
    -> CodeActionContext
    -> IO [CAResult]
codeAction _lsp _state (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List _xs} = do
    let
      title = "Add TODO Item"
      tedit = [TextEdit (Range (Position 0 0) (Position 1 0)) "-- TODO added by Example Plugin directly\n"]
      edit  = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
    pure
        [ CACodeAction $ CodeAction title (Just CodeActionQuickFix) (Just $ List []) (Just edit) Nothing ]


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
