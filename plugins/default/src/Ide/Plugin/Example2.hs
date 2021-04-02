{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.Example2
  (
    descriptor
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Binary
import Data.Functor
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import qualified Data.Text as T
import Data.Typeable
import Development.IDE as D
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import GHC.Generics
import Ide.PluginUtils
import Ide.Types
import Language.LSP.Types
import Language.LSP.Server
import Text.Regex.TDFA.Text()
import Control.Monad.IO.Class

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = exampleRules
  , pluginCommands = [PluginCommand "codelens.todo" "example adding" addTodoCmd]
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction     codeAction
                  <> mkPluginHandler STextDocumentCodeLens       codeLens
                  <> mkPluginHandler STextDocumentHover          hover
                  <> mkPluginHandler STextDocumentDocumentSymbol symbols
                  <> mkPluginHandler STextDocumentCompletion     completion
  }

-- ---------------------------------------------------------------------

hover :: PluginMethodHandler IdeState TextDocumentHover
hover ide _ HoverParams{..} = liftIO $ request "Hover" blah (Right Nothing) foundHover ide TextDocumentPositionParams{..}

blah :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
blah _ (Position line col)
  = return $ Just (Just (Range (Position line col) (Position (line+1) 0)), ["example hover 2\n"])

-- ---------------------------------------------------------------------
-- Generating Diagnostics via rules
-- ---------------------------------------------------------------------

data Example2 = Example2
    deriving (Eq, Show, Typeable, Generic)
instance Hashable Example2
instance NFData   Example2
instance Binary   Example2

type instance RuleResult Example2 = ()

exampleRules :: Rules ()
exampleRules = do
  define $ \Example2 file -> do
    _pm <- getParsedModule file
    let diag = mkDiag file "example2" DsError (Range (Position 0 0) (Position 1 0)) "example2 diagnostic, hello world"
    return ([diag], Just ())

  action $ do
    files <- getFilesOfInterest
    void $ uses Example2 $ Map.keys files

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
    , _tags     = Nothing
    , _relatedInformation = Nothing
    }

-- ---------------------------------------------------------------------
-- code actions
-- ---------------------------------------------------------------------

-- | Generate code actions.
codeAction :: PluginMethodHandler IdeState TextDocumentCodeAction
codeAction _state _pid (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List _xs}) = do
    let
      title = "Add TODO2 Item"
      tedit = [TextEdit (Range (Position 3 0) (Position 3 0))
               "-- TODO2 added by Example2 Plugin directly\n"]
      edit  = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
    pure $ Right $ List
        [ InR $ CodeAction title (Just CodeActionQuickFix) (Just $ List []) Nothing Nothing (Just edit) Nothing Nothing]

-- ---------------------------------------------------------------------

codeLens :: PluginMethodHandler IdeState TextDocumentCodeLens
codeLens ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = liftIO $ do
    logInfo (ideLogger ideState) "Example2.codeLens entered (ideLogger)" -- AZ
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> filePath) -> do
        _ <- runIdeAction (fromNormalizedFilePath filePath) (shakeExtras ideState) $ runMaybeT $ useE TypeCheck filePath
        _diag <- getDiagnostics ideState
        _hDiag <- getHiddenDiagnostics ideState
        let
          title = "Add TODO2 Item via Code Lens"
          range = Range (Position 3 0) (Position 4 0)
        let cmdParams = AddTodoParams uri "do abc"
            cmd = mkLspCommand plId "codelens.todo" title (Just [toJSON cmdParams])
        pure $ Right $ List [ CodeLens range (Just cmd) Nothing ]
      Nothing -> pure $ Right $ List []

-- ---------------------------------------------------------------------
-- | Parameters for the addTodo PluginCommand.
data AddTodoParams = AddTodoParams
  { file   :: Uri  -- ^ Uri of the file to add the pragma to
  , todoText :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

addTodoCmd :: CommandFunction IdeState AddTodoParams
addTodoCmd _ide (AddTodoParams uri todoText) = do
  let
    pos = Position 5 0
    textEdits = List
      [TextEdit (Range pos pos)
                  ("-- TODO2:" <> todoText <> "\n")
      ]
    res = WorkspaceEdit
      (Just $ Map.singleton uri textEdits)
      Nothing
      Nothing
  _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing res) (\_ -> pure ())
  return $ Right Null

-- ---------------------------------------------------------------------

foundHover :: (Maybe Range, [T.Text]) -> Either ResponseError (Maybe Hover)
foundHover (mbRange, contents) =
  Right $ Just $ Hover (HoverContents $ MarkupContent MkMarkdown
                        $ T.intercalate sectionSeparator contents) mbRange


-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> Action (Maybe a))
  -> Either ResponseError b
  -> (a -> Either ResponseError b)
  -> IdeState
  -> TextDocumentPositionParams
  -> IO (Either ResponseError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b)
                  -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runAction "Example2" ide $ getResults filePath pos

-- ---------------------------------------------------------------------

symbols :: PluginMethodHandler IdeState TextDocumentDocumentSymbol
symbols _ide _ (DocumentSymbolParams _ _ _doc)
    = pure $ Right $ InL $ List [r]
    where
        r = DocumentSymbol name detail kind Nothing deprecation range selR chList
        name = "Example2_symbol_name"
        detail = Nothing
        kind = SkVariable
        deprecation = Nothing
        range = Range (Position 4 1) (Position 4 7)
        selR = range
        chList = Nothing

-- ---------------------------------------------------------------------

completion :: PluginMethodHandler IdeState TextDocumentCompletion
completion _ide _pid (CompletionParams _doc _pos _ _ _mctxt)
    = pure $ Right $ InL $ List [r]
    where
        r = CompletionItem label kind tags detail documentation deprecated preselect
                           sortText filterText insertText insertTextFormat insertTextMode
                           textEdit additionalTextEdits commitCharacters
                           command xd
        label = "Example2 completion"
        kind = Nothing
        tags = Nothing
        detail = Nothing
        documentation = Nothing
        deprecated = Nothing
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = Nothing
        insertTextMode = Nothing
        insertTextFormat = Nothing
        textEdit = Nothing
        additionalTextEdits = Nothing
        commitCharacters = Nothing
        command = Nothing
        xd = Nothing

-- ---------------------------------------------------------------------
