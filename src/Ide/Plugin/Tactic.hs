-- | A plugin that uses tactics to synthesize code
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

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

import           Development.IDE.Core.RuleTypes (TcModuleResult (tmrModule),
                                                 TypeCheck (TypeCheck))
import           Development.IDE.Core.Shake     (useWithStale, IdeState (..))
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Service (runAction)
import           Development.Shake (Action)
import           Development.IDE.GHC.Error

import           Ide.Types
import           Ide.TacticMachinery
import           Ide.Tactics
import           Ide.Plugin
import           Ide.LocalBindings

import qualified Language.Haskell.LSP.Types      as J
import           Language.Haskell.LSP.Types

import OccName
import           HsExpr
import           GHC
import           DynFlags
import           Type


descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
    { pluginCommands
        = fmap (\(name, tac) ->
            PluginCommand
              (coerce $ name <> "Command")
              (tacticDesc name)
              (tacticCmd tac)) $
                Map.toList tacticCommands
    , pluginCodeActionProvider = Just codeActionProvider
    }
tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

tacticCommands :: Map.Map T.Text (OccName -> TacticsM ())
tacticCommands = Map.fromList
    [ ("auto",     const auto)
    , ("split",    const split)
    , ("intro",    const intro)
    , ("destruct", destruct)
    , ("homo",     homo)
    ]

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state

codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      Just (tmr, pos) <- runIde state $ useWithStale TypeCheck nfp
      let span = rangeToSrcSpan (fromNormalizedFilePath nfp)
               $ fromMaybe (error "bummer")
               $ fromCurrentRange pos range
      let mod = tmrModule tmr
      case mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod) of
        -- FIXME For some reason we get an HsVar instead of an
        -- HsUnboundVar. We should check if this is a hole somehow??
        Just (L span' (HsVar _ (L _ var))) -> do
          let params = TacticParams
                { file = uri
                , range
                    = fromMaybe (error "that is not great")
                    $ toCurrentRange pos =<< srcSpanToRange span'
                , var_name = T.pack $ occNameString $ occName var
                }
          let names = Map.keys tacticCommands
          actions <- for names $ \name -> do
            cmd <-
              mkLspCommand
                plId
                (coerce $ name <> "Command")
                name
                (Just [toJSON params])
            pure
              $ CACodeAction
              $ CodeAction
                  name
                  (Just CodeActionQuickFix)
                  Nothing
                  Nothing
              $ Just cmd
          pure $ Right $ List actions
        _ -> pure $ Right $ codeActions []
codeActionProvider _ _ _ _ _ _ = pure $ Right $ codeActions []

codeActions :: [CodeAction] -> List CAResult
codeActions = List . fmap CACodeAction


data TacticParams = TacticParams
    { file :: J.Uri -- ^ Uri of the file to fill the hole in
    , range :: J.Range -- ^ The range of the hole
    , var_name :: T.Text
    }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

tacticCmd :: (OccName -> TacticsM ()) -> CommandFunction TacticParams
tacticCmd tac _lf state (TacticParams uri range var_name)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      Just (tmr, pos) <- runIde state $ useWithStale TypeCheck nfp
      let
        span
          = rangeToSrcSpan (fromNormalizedFilePath nfp)
          $ fromMaybe (error "Oh shucks")
          $ fromCurrentRange pos range
        mod = tmrModule tmr
        hyps = hypothesisFromBindings span $ bindings mod
        Just (L _ (HsVar _ (L _ v)))
          = mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod)
        goal = varType v

      pure $
        case runTactic
                unsafeGlobalDynFlags
                goal
                hyps
              $ tac
              $ mkVarOcc
              $ T.unpack var_name of
          Left err ->
            (, Nothing)
              $ Left
              $ ResponseError InvalidRequest (T.pack $ show err) Nothing
          Right res ->
            let edit
                  = J.List
                  $ pure
                  $ J.TextEdit
                      ( fromMaybe (error "Fiddlesticks")
                      $ toCurrentRange pos range
                      )
                  $ T.pack res

                response = J.WorkspaceEdit (Just $ H.singleton uri edit) Nothing
            in ( Right Null
               , Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams response)
               )
tacticCmd _ _ _ _ =
  pure (Left $ ResponseError InvalidRequest (T.pack "nah") Nothing, Nothing)

