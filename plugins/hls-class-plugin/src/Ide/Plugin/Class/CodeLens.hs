{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Ide.Plugin.Class.CodeLens where

import           Control.Lens                         ((&), (?~), (^.))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Data.Aeson                           hiding (Null)
import qualified Data.IntMap.Strict                   as IntMap
import           Data.Maybe                           (mapMaybe)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server                  (sendRequest)

codeLens :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLens state plId clp = do
    nfp <-  getNormalizedFilePathE $ clp ^. L.textDocument . L.uri
    (InstanceBindLensResult (InstanceBindLens{lensRange}), pm) <- runActionE "classplugin.GetInstanceBindLens" state
        -- Using stale results means that we can almost always return a value. In practice
        -- this means the lenses don't 'flicker'
        $ useWithStaleE GetInstanceBindLens nfp
    pure $ InL $ mapMaybe (toCodeLens pm) lensRange
    where toCodeLens pm (range, int) =
            let newRange = toCurrentRange pm range
            in (\r -> CodeLens r Nothing (Just $ toJSON int)) <$> newRange

codeLensResolve:: ResolveFunction IdeState Int Method_CodeLensResolve
codeLensResolve state plId cl uri uniqueID = do
    nfp <-  getNormalizedFilePathE uri
    pragmaInsertion <- insertPragmaIfNotPresent state nfp InstanceSigs
    (InstanceBindLensResult (InstanceBindLens{lensRendered}), pm) <- runActionE "classplugin.GetInstanceBindLens" state
        -- Using stale results means that we can almost always return a value. In practice
        -- this means the lenses don't 'flicker'
        $ useWithStaleE GetInstanceBindLens nfp
    resolveData <- handleMaybe PluginStaleResolve $ IntMap.lookup uniqueID lensRendered
    let makeCommand (TextEdit range title) =
            case makeEdit range title pm of
                Just edit -> Just
                    $ mkLspCommand plId typeLensCommandId title (Just [toJSON (workspaceEdit pragmaInsertion [edit])])
                Nothing -> Nothing
    codeLensCommand <- handleMaybe PluginStaleResolve $ makeCommand resolveData
    pure $ cl & L.command ?~ codeLensCommand
    where
        workspaceEdit pragmaInsertion edits =
            WorkspaceEdit
                (pure [(uri, edits ++ pragmaInsertion)])
                Nothing
                Nothing

        makeEdit :: Range -> T.Text -> PositionMapping -> Maybe TextEdit
        makeEdit range bind mp =
            let startPos = range ^. L.start
                insertChar = startPos ^. L.character
                insertRange = Range startPos startPos
            in case toCurrentRange mp insertRange of
                Just rg -> Just $ TextEdit rg (bind <> "\n" <> T.replicate (fromIntegral insertChar) " ")
                Nothing -> Nothing

codeLensCommandHandler :: CommandFunction IdeState WorkspaceEdit
codeLensCommandHandler _ wedit = do
  _ <- lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  pure $ InR Null
