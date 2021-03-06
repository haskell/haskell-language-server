{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Ide.Plugin.TypeSynonyms
  ( descriptor,
  )
where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Debug.Trace
import Development.IDE
import Development.IDE.Spans.Common
import Development.IDE.Spans.LocalBindings
import GhcPlugins (typeEnvTyCons)
import Ide.PluginUtils
import Ide.Types
import Language.Haskell.TH.Syntax as TH
import Language.LSP.Types
import TcRnTypes
import Development.IDE.GHC.Compat
import Data.Maybe (mapMaybe)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
    }

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ (TextDocumentIdentifier docUri) (Range start end) _context)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri docUri = do

    -- Gather data
    tcModuleResult <- liftIO $ runAction "typeSynonymPlugin" ideState $ use_ TypeCheck nfp
    let tEnv = tcg_type_env $ tmrTypechecked tcModuleResult
    let tyCons = typeEnvTyCons tEnv
    let tySynonyms = mapMaybe synTyConRhs_maybe tyCons
    liftIO $ logInfo (ideLogger ideState) ("tySynonyms: " <> showGhc tySynonyms)

    -- Make the code action
    let _title = "Replace with type synonym"
    let _kind = Just CodeActionQuickFix
    let _command = Nothing
    let _edit = Nothing
    let _changes = Nothing
    let _documentChanges = Nothing
    let _diagnostics = Nothing
    let _isPreferred = Nothing
    let _disabled = Nothing
    let caReplaceWithSynonym = CodeAction {..}
    return $ Right $ List [InR caReplaceWithSynonym]
