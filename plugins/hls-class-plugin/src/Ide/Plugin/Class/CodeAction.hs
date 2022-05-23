{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.CodeAction where

import           Control.Lens                         hiding (List, use)
import           Control.Monad.Extra
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (throwE)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.List
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (fromJust, isNothing)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.PositionMapping (fromCurrentRange)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Spans.AtPoint        (pointCommand)
import           GHC.LanguageExtensions.Type
import           Ide.Plugin.Class.ExactPrint
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens              as J

addMethodPlaceholders :: PluginId -> CommandFunction IdeState AddMinimalMethodsParams
addMethodPlaceholders plId state param@AddMinimalMethodsParams{..} = do
    caps <- getClientCapabilities
    response $ do
        nfp <- getNormalizedFilePath plId uri
        pm <- handleMaybeM "Unable to GetParsedModule"
            $ liftIO
            $ runAction "classplugin.addMethodPlaceholders.GetParsedModule" state
            $ use GetParsedModule nfp
        (hsc_dflags . hscEnv -> df) <- handleMaybeM "Unable to GhcSessionDeps"
            $ liftIO
            $ runAction "classplugin.addMethodPlaceholders.GhcSessionDeps" state
            $ use GhcSessionDeps nfp
        (old, new) <- handleMaybeM "Unable to makeEditText"
            $ liftIO $ runMaybeT
            $ makeEditText pm df param
        pragmaInsertion <- insertPragmaIfNotPresent state nfp InstanceSigs
        let edit =
                if withSig
                then mergeEdit (workspaceEdit caps old new) pragmaInsertion
                else workspaceEdit caps old new

        void $ lift $ sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())

        pure Null
    where
        toTextDocunemtEdit edit =
            TextDocumentEdit (VersionedTextDocumentIdentifier uri (Just 0)) (List [InL edit])

        mergeEdit :: WorkspaceEdit -> [TextEdit] -> WorkspaceEdit
        mergeEdit WorkspaceEdit{..} edits = WorkspaceEdit
            { _documentChanges =
                (\(List x) -> List $ x ++ map (InL . toTextDocunemtEdit) edits)
                    <$> _documentChanges
            , ..
            }

        workspaceEdit caps old new
            = diffText caps (uri, old) new IncludeDeletions

-- |
-- This implementation is ad-hoc in a sense that the diagnostic detection mechanism is
-- sensitive to the format of diagnostic messages from GHC.
codeAction :: PluginMethodHandler IdeState TextDocumentCodeAction
codeAction state plId (CodeActionParams _ _ docId _ context) = response $ do
    nfp <- getNormalizedFilePath plId uri
    actions <- join <$> mapM (mkActions nfp) methodDiags
    pure $ List actions
    where
        uri = docId ^. J.uri
        List diags = context ^. J.diagnostics

        ghcDiags = filter (\d -> d ^. J.source == Just "typecheck") diags
        methodDiags = filter (\d -> isClassMethodWarning (d ^. J.message)) ghcDiags

        mkActions docPath diag = do
            ident <- findClassIdentifier docPath range
            cls <- findClassFromIdentifier docPath ident
            InstanceBindTypeSigsResult sigs <- handleMaybeM "Unable to GetInstanceBindTypeSigs"
                $ liftIO
                $ runAction "classplugin.codeAction.GetInstanceBindTypeSigs" state
                $ use GetInstanceBindTypeSigs docPath
            pure $ concatMap mkAction $ minDefToMethodGroups range sigs . classMinimalDef $ cls
            where
                range = diag ^. J.range

                mkAction :: [(T.Text, T.Text)] -> [Command |? CodeAction]
                mkAction methodGroup
                    = [ mkCodeAction title
                            $ mkLspCommand plId codeActionCommandId title
                                (Just $ mkCmdParams methodGroup False)
                    , mkCodeAction titleWithSig
                            $ mkLspCommand plId codeActionCommandId titleWithSig
                                (Just $ mkCmdParams methodGroup True)
                    ]
                    where
                        title = mkTitle $ fst <$> methodGroup
                        titleWithSig = mkTitleWithSig $ fst <$> methodGroup

                mkTitle methodGroup
                    = "Add placeholders for "
                        <> mconcat (intersperse ", " (fmap (\m -> "'" <> m <> "'") methodGroup))

                mkTitleWithSig methodGroup = mkTitle methodGroup <> " with signature(s)"

                mkCmdParams methodGroup withSig =
                    [toJSON (AddMinimalMethodsParams uri range (List methodGroup) withSig)]

                mkCodeAction title cmd
                    = InR
                    $ CodeAction
                        title
                        (Just CodeActionQuickFix)
                        (Just (List []))
                        Nothing
                        Nothing
                        Nothing
                        (Just cmd)
                        Nothing

        findClassIdentifier docPath range = do
            (hieAstResult, pmap) <- handleMaybeM "Unable to GetHieAst"
                . liftIO
                . runAction "classplugin.findClassIdentifier.GetHieAst" state
                $ useWithStale GetHieAst docPath
            case hieAstResult of
                HAR {hieAst = hf} ->
                    pure
                        $ head . head
                        $ pointCommand hf (fromJust (fromCurrentRange pmap range) ^. J.start & J.character -~ 1)
                        ( (Map.keys . Map.filter isClassNodeIdentifier . getNodeIds)
                            <=< nodeChildren
                        )

        findClassFromIdentifier docPath (Right name) = do
            (hscEnv -> hscenv, _) <- handleMaybeM "Unable to GhcSessionDeps"
                . liftIO
                . runAction "classplugin.findClassFromIdentifier.GhcSessionDeps" state
                $ useWithStale GhcSessionDeps docPath
            (tmrTypechecked -> thisMod, _) <- handleMaybeM "Unable to TypeCheck"
                . liftIO
                . runAction "classplugin.findClassFromIdentifier.TypeCheck" state
                $ useWithStale TypeCheck docPath
            handleMaybeM "Error in TcEnv"
                . liftIO
                . fmap snd
                . initTcWithGbl hscenv thisMod ghostSpan $ do
                    tcthing <- tcLookup name
                    case tcthing of
                        AGlobal (AConLike (RealDataCon con))
                            | Just cls <- tyConClass_maybe (dataConOrigTyCon con) -> pure cls
                        _ -> panic "Ide.Plugin.Class.findClassFromIdentifier"
        findClassFromIdentifier _ (Left _) = throwE "Ide.Plugin.Class.findClassIdentifier"

isClassNodeIdentifier :: IdentifierDetails a -> Bool
isClassNodeIdentifier ident = (isNothing . identType) ident && Use `Set.member` identInfo ident

isClassMethodWarning :: T.Text -> Bool
isClassMethodWarning = T.isPrefixOf "• No explicit implementation for"

-- Return (name text, signature text)
minDefToMethodGroups :: Range -> [InstanceBindTypeSig] -> BooleanFormula Name -> [[(T.Text, T.Text)]]
minDefToMethodGroups range sigs = go
    where
        go (Var mn)   = [[ (T.pack . occNameString . occName $ mn, bindRendered sig)
                        | sig <- sigs
                        , inRange range (getSrcSpan (bindName sig))
                        , printOutputable mn == T.drop (T.length bindingPrefix) (printOutputable (bindName sig))
                        ]]
        go (Or ms)    = concatMap (go . unLoc) ms
        go (And ms)   = foldr (liftA2 (<>)) [[]] (fmap (go . unLoc) ms)
        go (Parens m) = go (unLoc m)
