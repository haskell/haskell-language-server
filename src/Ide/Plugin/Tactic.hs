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
import           Data.Traversable
import qualified Data.Map                        as Map
import qualified Data.HashMap.Strict             as H
import qualified Data.Text                       as T
import qualified GHC.Generics                    as Generics

import           Development.IDE.Core.RuleTypes (GhcSessionDeps (GhcSessionDeps),
                                                 TcModuleResult (tmrModule),
                                                 TypeCheck (TypeCheck))
import           Development.IDE.Core.Shake     (useWithStale, IdeState (..))
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Service (runAction)
import           Development.Shake (Action)
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util

import           Ide.Types
import           Ide.TacticMachinery
import           Ide.Tactics
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
import           Type


import System.IO
-- import Refinery.Tactic

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands = fmap (\(name, tac) -> PluginCommand (coerce $ name <> "Command") (tacticDesc name) (tacticCmd tac)) $ Map.toList tacticCommands
    , pluginCodeActionProvider = Just codeActionProvider
    }
tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

tacticCommands :: Map.Map T.Text (TacticsM ())
tacticCommands = Map.fromList
    [ ("auto", auto)
    , ("split", split)
    , ("intro", intro)
    ]

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state

rangeToSrcSpan :: String -> Range -> SrcSpan
rangeToSrcSpan file (Range (Position startLn startCh) (Position endLn endCh)) =
    mkSrcSpan (mkSrcLoc (fsLit file) (startLn + 1) (startCh + 1)) (mkSrcLoc (fsLit file) (endLn + 1) (endCh + 1))

codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
          Just (tmr, pos) <- runIde state $ useWithStale TypeCheck nfp
          let span = rangeToSrcSpan (fromNormalizedFilePath nfp) $ fromMaybe (error "bummer") $ fromCurrentRange pos range
          let mod = tmrModule tmr
          case mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod) of
            -- FIXME For some reason we get an HsVar instead of an HsUnboundVar. We should
            -- check if this is a hole somehow??
            Just (L span' (HsVar _ _)) -> do
                let params = TacticParams { file = uri, range = fromMaybe (error "that is not great") $ toCurrentRange pos =<< srcSpanToRange span' }
                let names = Map.keys tacticCommands
                actions <- for names $ \name -> do
                  cmd <- mkLspCommand plId (coerce $ name <> "Command") name (Just [toJSON params])
                  pure $ CACodeAction $ CodeAction name (Just CodeActionQuickFix) Nothing Nothing (Just cmd)
                pure $ Right $ List actions
            Just e -> pure (Right (List [ CACodeAction $ CodeAction (T.pack (render unsafeGlobalDynFlags e)) (Just CodeActionQuickFix) Nothing Nothing Nothing ]))
            Nothing -> pure (Right (List []))


data TacticParams = TacticParams
    { file :: J.Uri -- ^ Uri of the file to fill the hole in
    , range :: J.Range -- ^ The range of the hole
    }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

tacticCmd :: TacticsM () -> CommandFunction TacticParams
tacticCmd tac _lf state (TacticParams uri range)
    | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
        Just (tmr, pos) <- runIde state $ useWithStale TypeCheck nfp
        let
          span = rangeToSrcSpan (fromNormalizedFilePath nfp) $ fromMaybe (error "Oh shucks") $ fromCurrentRange pos range
          mod = tmrModule tmr
          hyps = hypothesisFromBindings span $ bindings mod
          Just (L _ (HsVar _ (L _ v))) = mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod)
          goal = varType v
        case runTactic unsafeGlobalDynFlags goal hyps tac of
            Left err -> pure (Left (ResponseError InvalidRequest (T.pack $ show err) Nothing), Nothing)
            Right res ->
                let edit = J.List [ J.TextEdit (fromMaybe (error "Fiddlesticks" ) $ toCurrentRange pos range) (T.pack res) ]
                    response = J.WorkspaceEdit (Just $ H.singleton uri edit) Nothing
                in pure (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams response))
