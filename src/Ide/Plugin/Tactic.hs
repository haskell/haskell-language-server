{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

-- | A plugin that uses tactics to synthesize code
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
        = fmap (\tc ->
            PluginCommand
              (tcCommandId tc)
              (tacticDesc $ tcCommandName tc)
              (tacticCmd $ commandTactic tc))
              [minBound .. maxBound]
    , pluginCodeActionProvider = Just codeActionProvider
    }

tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"

data TacticCommand
  = Auto
  | Split
  | Intro
  | Destruct
  | Homo
  deriving (Eq, Ord, Show, Enum, Bounded)

data TacticVariety
  = PerHole
  | PerBinding
  deriving (Eq, Ord, Show, Enum, Bounded)

tcCommandId :: TacticCommand -> CommandId
tcCommandId c = coerce $ T.pack $ "tactics" <> show c <> "Command"

tcCommandName :: TacticCommand -> T.Text
tcCommandName = T.pack . show

tcCommandTitle :: TacticCommand -> OccName -> T.Text
tcCommandTitle tc occ = T.pack $ show tc <> " " <> occNameString occ

commandProvider :: TacticCommand -> TacticProvider
commandProvider Auto  = provide Auto "Auto" ""
commandProvider Split = provide Split "Split" ""
commandProvider Intro =
  filterGoalType isFunction $
    provide Intro "Intro" ""
commandProvider Destruct =
  filterBindingType destructFilter $ \occ _ ->
    provide Destruct (tcCommandTitle Destruct occ) $ T.pack $ occNameString occ
commandProvider Homo =
  filterBindingType homoFilter $ \occ _ ->
    provide Homo (tcCommandTitle Homo occ) $ T.pack $ occNameString occ

commandTactic :: TacticCommand -> OccName -> TacticsM ()
commandTactic Auto     = const auto
commandTactic Split    = const split
commandTactic Intro    = const intro
commandTactic Destruct = destruct
commandTactic Homo     = homo

homoFilter :: Type -> Type -> Bool
homoFilter (algebraicTyCon -> Just t1) (algebraicTyCon -> Just t2) = t1 == t2
homoFilter _ _ = False

destructFilter :: Type -> Type -> Bool
destructFilter _ (algebraicTyCon -> Just _) = True
destructFilter _ _ = False

runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state

codeActionProvider :: CodeActionProvider
codeActionProvider _conf state plId (TextDocumentIdentifier uri) range _ctx
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      (pos, mss, jdg) <- judgmentForHole state nfp range
      case mss of
        -- FIXME For some reason we get an HsVar instead of an
        -- HsUnboundVar. We should check if this is a hole somehow??
        L span' (HsVar _ (L _ _)) -> do
          let resulting_range
                = fromMaybe (error "that is not great")
                $ toCurrentRange pos =<< srcSpanToRange span'
          actions <- (foldMap commandProvider [minBound .. maxBound]) plId uri resulting_range jdg
          pure $ Right $ List actions
        _ -> pure $ Right $ codeActions []
codeActionProvider _ _ _ _ _ _ = pure $ Right $ codeActions []


codeActions :: [CodeAction] -> List CAResult
codeActions = List . fmap CACodeAction


type TacticProvider = PluginId -> Uri -> Range -> Judgement -> IO [CAResult]

provide :: TacticCommand -> T.Text -> T.Text -> TacticProvider
provide tc title name plId uri range _ = do
  let params = TacticParams { file = uri , range = range , var_name = name }
  cmd <- mkLspCommand plId (tcCommandId tc) title (Just [toJSON params])
  pure
    $ pure
    $ CACodeAction
    $ CodeAction title (Just CodeActionQuickFix) Nothing Nothing
    $ Just cmd

filterGoalType :: (Type -> Bool) -> TacticProvider -> TacticProvider
filterGoalType p tp plId uri range jdg@(Judgement _ (CType g)) =
  case p g of
    True  -> tp plId uri range jdg
    False -> pure []

filterBindingType
    :: (Type -> Type -> Bool)
    -> (OccName -> Type -> TacticProvider)
    -> TacticProvider
filterBindingType p tp plId uri range jdg@(Judgement hys (CType g)) =
  fmap join $ for (Map.toList hys) $ \(occ, CType ty) ->
    case p g ty of
      True  -> tp occ ty plId uri range jdg
      False -> pure []


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

