{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A plugin that uses tactics to synthesize code

module Ide.Plugin.Tactic
  ( descriptor
  ) where

import           Data.Aeson
import qualified Data.HashMap.Strict             as H
import qualified GHC.Generics                    as Generics
import           Ide.Types
import qualified Language.Haskell.LSP.Types      as J
import           Language.Haskell.LSP.Types

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands = [PluginCommand "fillHole" "fill the hole using a tactic" tacticCmd]
    , pluginCodeActionProvider = Just codeActionProvider
    }

codeActionProvider :: CodeActionProvider
codeActionProvider _conf _ide _plid (TextDocumentIdentifier uri) range _ctx = _hole

-- codeAction :: CodeActionProvider
-- codeAction _lf _state _pid _tid _range 

data TacticParams = TacticParams
    { file :: J.Uri -- ^ Uri of the file to fill the hole in
    , range :: J.Range -- ^ The range of the hole
    }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

tacticCmd :: CommandFunction TacticParams
tacticCmd _lf _ide (TacticParams uri range) = do
    let
      textEdits = J.List
        [J.TextEdit range _solution
        ]
      res = J.WorkspaceEdit
        (Just $ H.singleton uri textEdits)
        Nothing
    pure (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams res))
