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

import           Development.IDE.Core.RuleTypes (GhcSessionDeps (GhcSessionDeps),
                                                 TcModuleResult (tmrModule),
                                                 TypeCheck (TypeCheck))
import           Development.IDE.Core.Shake     (use, IdeState (..))
import           Development.IDE.Core.Service (runAction)
import           Development.Shake (Action)

import           Ide.Types

import qualified Language.Haskell.LSP.Types      as J
import           Language.Haskell.LSP.Types

-- import Refinery.Tactic

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands = [PluginCommand "fillHole" "fill the hole using a tactic" tacticCmd]
    , pluginCodeActionProvider = Just codeActionProvider
    }

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state

codeActionProvider :: CodeActionProvider
codeActionProvider _conf state _plid (TextDocumentIdentifier uri) range _ctx
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
          tmr <- runIde state $ use TypeCheck nfp
          hsc <- runIde state $ use GhcSessionDeps nfp
          -- TODO Check if we have a hole inside of the range.
          -- Go from range -> SrcSpan, then look inside the module contents
          _hole


data TacticParams = TacticParams
    { file :: J.Uri -- ^ Uri of the file to fill the hole in
    , range :: J.Range -- ^ The range of the hole
    }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

tacticCmd :: CommandFunction TacticParams
tacticCmd _lf _ide (TacticParams uri range) = do
    let
      textEdits = J.List
        [J.TextEdit range ("Howdy Partner!")
        ]
      res = J.WorkspaceEdit
        (Just $ H.singleton uri textEdits)
        Nothing
    pure (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams res))
