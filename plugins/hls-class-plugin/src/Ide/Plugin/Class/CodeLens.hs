{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns    #-}
module Ide.Plugin.Class.CodeLens where

import           Control.Lens                         ((&), (?~), (^.))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Data.Aeson                           hiding (Null)
import qualified Data.IntMap.Strict                   as IntMap
import           Data.Maybe                           (mapMaybe, maybeToList)
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.Pragmas        (getFirstPragma,
                                                       insertNewPragma)
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

-- The code lens method is only responsible for providing the ranges of the code
-- lenses matched to a unique id
codeLens :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLens state _plId clp = do
    nfp <-  getNormalizedFilePathE $ clp ^. L.textDocument . L.uri
    (InstanceBindLensResult (InstanceBindLens{lensRange}), pm)
        <- runActionE "classplugin.GetInstanceBindLens" state
            -- Using stale results means that we can almost always return a
            -- value. In practice this means the lenses don't 'flicker'
            $ useWithStaleE GetInstanceBindLens nfp
    pure $ InL $ mapMaybe (toCodeLens pm) lensRange
    where toCodeLens pm (range, int) =
            let newRange = toCurrentRange pm range
            in (\r -> CodeLens r Nothing (Just $ toJSON int)) <$> newRange

-- The code lens resolve method matches a title to each unique id
codeLensResolve:: ResolveFunction IdeState Int Method_CodeLensResolve
codeLensResolve state plId cl uri uniqueID = do
    nfp <-  getNormalizedFilePathE uri
    (InstanceBindLensResult (InstanceBindLens{lensDetails}), pm)
        <- runActionE "classplugin.GetInstanceBindLens" state
            $ useWithStaleE GetInstanceBindLens nfp
    (tmrTypechecked -> gblEnv, _) <- runActionE "classplugin.codeAction.TypeCheck" state $ useWithStaleE TypeCheck nfp
    (hscEnv -> hsc, _) <- runActionE "classplugin.codeAction.GhcSession" state $ useWithStaleE GhcSession nfp
    (range, name, typ) <- handleMaybe PluginStaleResolve
                    $ IntMap.lookup uniqueID lensDetails
    let title = prettyBindingNameString (printOutputable name) <> " :: " <> T.pack (showDoc hsc gblEnv typ)
    edit <- handleMaybe (PluginInvalidUserState "toCurrentRange") $ makeEdit range title pm
    let command = mkLspCommand plId typeLensCommandId title (Just [toJSON $ InstanceBindLensCommand uri edit])
    pure $ cl & L.command ?~ command
    where
        makeEdit :: Range -> T.Text -> PositionMapping -> Maybe TextEdit
        makeEdit range bind mp =
            let startPos = range ^. L.start
                insertChar = startPos ^. L.character
                insertRange = Range startPos startPos
            in case toCurrentRange mp insertRange of
                Just rg -> Just $ TextEdit rg (bind <> "\n" <> T.replicate (fromIntegral insertChar) " ")
                Nothing -> Nothing

-- Finally the command actually generates and applies the workspace edit for the
-- specified unique id.
codeLensCommandHandler :: PluginId -> CommandFunction IdeState InstanceBindLensCommand
codeLensCommandHandler plId state _ InstanceBindLensCommand{commandUri, commandEdit} = do
    nfp <-  getNormalizedFilePathE commandUri
    (InstanceBindLensResult (InstanceBindLens{lensEnabledExtensions}), _)
        <- runActionE "classplugin.GetInstanceBindLens" state
            $ useWithStaleE GetInstanceBindLens nfp
    -- We are only interested in the pragma information if the user does not
    -- have the InstanceSigs extension enabled
    mbPragma <- if InstanceSigs `elem` lensEnabledExtensions
                then pure Nothing
                else Just <$> getFirstPragma plId state nfp
    let -- By mapping over our Maybe NextPragmaInfo value, we only compute this
        -- edit if we actually need to.
        pragmaInsertion =
            maybeToList $ flip insertNewPragma InstanceSigs <$> mbPragma
        wEdit = workspaceEdit pragmaInsertion
    _ <- lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wEdit) (\_ -> pure ())
    pure $ InR Null
    where
        workspaceEdit pragmaInsertion=
            WorkspaceEdit
                (pure [(commandUri, commandEdit : pragmaInsertion)])
                Nothing
                Nothing




