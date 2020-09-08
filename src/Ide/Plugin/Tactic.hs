-- | A plugin that uses tactics to synthesize code
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Ide.Plugin.Tactic
  ( descriptor
  ) where

import Control.Monad
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
              (tacticCmd tac))
              (Map.toList registeredCommands)
    , pluginCodeActionProvider = Just codeActionProvider
    }

tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

registeredCommands :: Map.Map T.Text (OccName -> TacticsM ())
registeredCommands = Map.fromList
    [ ("auto",     const auto)
    , ("split",    const split)
    , ("intro",    const intro)
    , ("destruct", destruct)
    , ("homo",     homo)
    ]

alwaysCommands :: [T.Text]
alwaysCommands =
    [ "auto"
    , "split"
    , "intro"
    ]

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state

codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      (pos, mss, jdg) <- judgmentForHole state nfp range
      case mss of
        -- FIXME For some reason we get an HsVar instead of an
        -- HsUnboundVar. We should check if this is a hole somehow??
        L span' (HsVar _ (L _ var)) -> do
          let resulting_range
                = fromMaybe (error "that is not great")
                $ toCurrentRange pos =<< srcSpanToRange span'
          let params = TacticParams
                { file = uri
                , range = resulting_range
                , var_name = T.pack $ occNameString $ occName var
                }
          let names = alwaysCommands
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
          split_actions <- mkSplits plId uri resulting_range jdg
          homo_actions  <- mkHomos plId uri resulting_range jdg
          pure $ Right $ List $ mconcat
            [ actions
            , split_actions
            , homo_actions
            ]
        _ -> pure $ Right $ codeActions []
codeActionProvider _ _ _ _ _ _ = pure $ Right $ codeActions []


codeActions :: [CodeAction] -> List CAResult
codeActions = List . fmap CACodeAction


mkSplits :: PluginId -> Uri -> Range -> Judgement -> IO [CAResult]
mkSplits plId uri range (Judgement hys _) =
  fmap join $ for (Map.toList hys) $ \(occ, CType ty) ->
    case algebraicTyCon ty of
      Nothing -> pure []
      Just _ -> do
        let name = T.pack $ occNameString occ
        let params = TacticParams
              { file = uri
              , range = range
              , var_name = name
              }
        cmd <-
          mkLspCommand
            plId
            ("destructCommand")
            name
            (Just [toJSON params])
        pure
          $ pure
          $ CACodeAction
          $ CodeAction
              ("destruct " <> name)
              (Just CodeActionQuickFix)
              Nothing
              Nothing
          $ Just cmd


mkHomos :: PluginId -> Uri -> Range -> Judgement -> IO [CAResult]
mkHomos plId uri range (Judgement hys (CType g)) =
  case algebraicTyCon g of
    Nothing -> pure []
    Just tycon ->
      fmap join $ for (Map.toList hys) $ \(occ, CType ty) ->
        case algebraicTyCon ty of
          Just tycon2 | tycon == tycon2 -> do
            let name = T.pack $ occNameString occ
            let params = TacticParams
                  { file = uri
                  , range = range
                  , var_name = name
                  }
            cmd <-
              mkLspCommand
                plId
                ("homoCommand")
                name
                (Just [toJSON params])
            pure
              $ pure
              $ CACodeAction
              $ CodeAction
                  ("homo " <> name)
                  (Just CodeActionQuickFix)
                  Nothing
                  Nothing
              $ Just cmd
          _ -> pure []





data TacticParams = TacticParams
    { file :: J.Uri -- ^ Uri of the file to fill the hole in
    , range :: J.Range -- ^ The range of the hole
    , var_name :: T.Text
    }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)


judgmentForHole
    :: IdeState
    -> NormalizedFilePath
    -> Range
    -> IO (PositionMapping, LHsExpr GhcTc, Judgement)
judgmentForHole state nfp range = do
  Just (tmr, pos) <- runIde state $ useWithStale TypeCheck nfp
  let span = rangeToSrcSpan (fromNormalizedFilePath nfp)
           $ fromMaybe (error "Oh shucks")
           $ fromCurrentRange pos range
      mod = tmrModule tmr
      Just (mss@(L span' (HsVar _ (L _ v))))
        = mostSpecificSpan @_ @GhcTc span (tm_typechecked_source mod)
      goal = varType v
      hyps = hypothesisFromBindings span' $ bindings mod
  pure (pos, mss, Judgement hyps $ CType goal)


tacticCmd :: (OccName -> TacticsM ()) -> CommandFunction TacticParams
tacticCmd tac _lf state (TacticParams uri range var_name)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      (pos, _, jdg) <- judgmentForHole state nfp range
      pure $
        case runTactic
                unsafeGlobalDynFlags
                jdg
              $ tac
              $ mkVarOcc
              $ T.unpack var_name of
          Left err ->
            (, Nothing)
              $ Left
              $ ResponseError InvalidRequest (T.pack $ show err) Nothing
          Right res ->
            let edit =
                  J.List
                    $ pure
                    $ J.TextEdit
                        ( fromMaybe (error "Fiddlesticks")
                        $ toCurrentRange pos range
                        )
                    $ T.pack res

                response =
                  J.WorkspaceEdit (Just $ H.singleton uri edit) Nothing
            in ( Right Null
               , Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams response)
               )
tacticCmd _ _ _ _ =
  pure (Left $ ResponseError InvalidRequest (T.pack "nah") Nothing, Nothing)

