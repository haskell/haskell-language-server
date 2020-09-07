{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A plugin that uses tactics to synthesize code

module Ide.Plugin.Tactic
  ( descriptor
  ) where

import           Data.Aeson
import           Data.Coerce
import           Data.Maybe
import qualified Data.HashMap.Strict             as H
import qualified Data.Text                       as T
import qualified GHC.Generics                    as Generics

import           Development.IDE.Core.RuleTypes (GhcSessionDeps (GhcSessionDeps),
                                                 TcModuleResult (tmrModule),
                                                 TypeCheck (TypeCheck))
import           Development.IDE.Core.Shake     (use, IdeState (..))
import           Development.IDE.Core.Service (runAction)
import           Development.Shake (Action)
import           Development.IDE.GHC.Util

import           Ide.Types
import           Ide.TacticMachinery
import           Ide.Plugin
import           Ide.LocalBindings

import qualified Language.Haskell.LSP.Types      as J
import           Language.Haskell.LSP.Types

import           FastString
import           HsBinds
import           HsExpr
import           GHC
import           SrcLoc
import           DynFlags


import System.IO
-- import Refinery.Tactic

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands = [PluginCommand (coerce tacticCommandName) "fill the hole using a tactic" tacticCmd]
    , pluginCodeActionProvider = Just codeActionProvider
    }

tacticCommandName :: T.Text
tacticCommandName = "tacticCommand"

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state

rangeToSrcSpan :: String -> Range -> SrcSpan
rangeToSrcSpan file (Range (Position startLn startCh) (Position endLn endCh)) =
    mkSrcSpan (mkSrcLoc (fsLit file) (startLn + 1) (startCh + 1)) (mkSrcLoc (fsLit file) (endLn + 1) (endCh + 1))

codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
          Just tmr <- runIde state $ use TypeCheck nfp
          -- FIXME Get the dynflags from this?
          hsc <- runIde state $ use GhcSessionDeps nfp
          let title = "Fill Hole"
          let span = rangeToSrcSpan (fromNormalizedFilePath nfp) range
          let mod = tmrModule tmr
          cmd <- mkLspCommand plId (coerce tacticCommandName) title Nothing
          case mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod) of
            Just hole -> pure (Right (List [ CACodeAction $ CodeAction (title <> " " <> (T.pack $ render unsafeGlobalDynFlags hole)) (Just CodeActionQuickFix) Nothing Nothing (Just cmd) ]))
            Nothing -> pure (Right (List [ CACodeAction $ CodeAction ("Broken: " <> " ") (Just CodeActionQuickFix) Nothing Nothing (Just cmd) ]))


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
