{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.ExampleCabal where

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text as T
import           Development.IDE            as D hiding (pluginHandlers)
import           GHC.Generics
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types

newtype Log = LogText T.Text deriving Show

instance Pretty Log where
  pretty = \case
    LogText log -> pretty log

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginCommands = [PluginCommand "codelens.todo" "example adding" addTodoCmd]
  , pluginHandlers = mkPluginHandler STextDocumentCodeLens       (codeLens recorder)
  }

-- ---------------------------------------------------------------------

codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState TextDocumentCodeLens
codeLens recorder _ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = liftIO $ do
    log Debug $ LogText "ExampleCabal.codeLens entered (ideLogger)"
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> _filePath) -> do
        let
          title = "Add TODO Item via Code Lens"
          range = Range (Position 3 0) (Position 4 0)
        let cmdParams = AddTodoParams uri "do abc"
            cmd = mkLspCommand plId "codelens.todo" title (Just [toJSON cmdParams])
        pure $ Right $ List [ CodeLens range (Just cmd) Nothing ]
      Nothing -> pure $ Right $ List []
  where
    log = logWith recorder

-- ---------------------------------------------------------------------
-- | Parameters for the addTodo PluginCommand.
data AddTodoParams = AddTodoParams
  { file     :: Uri  -- ^ Uri of the file to add the pragma to
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
