{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.CodeAction (
    addMethodPlaceholders,
    codeAction,
) where

import           Control.Arrow                    ((>>>))
import           Control.Lens                     hiding (List, use)
import           Control.Monad.Extra
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                       hiding (Null)
import           Data.List
import           Data.List.Extra                  (nubOrdOn)
import           Data.Maybe                       (listToMaybe, mapMaybe)
import qualified Data.Text                        as T
import           Development.IDE
import           Development.IDE.Core.FileStore   (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error (TcRnMessage (..),
                                                   _TcRnMessage,
                                                   msgEnvelopeErrorL,
                                                   stripTcRnMessageContext)
import           Development.IDE.GHC.Compat.Util
import           Ide.Plugin.Class.ExactPrint
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import qualified Ide.Plugin.Config
import           Ide.Plugin.Error
import qualified Ide.Plugin.RangeMap              as RangeMap
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

addMethodPlaceholders :: PluginId -> CommandFunction IdeState AddMinimalMethodsParams
addMethodPlaceholders _ state _ param@AddMinimalMethodsParams{..} = do
    caps <- lift pluginGetClientCapabilities
    nfp <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
    pm <- runActionE "classplugin.addMethodPlaceholders.GetParsedModule" state
        $ useE GetParsedModule nfp
    (hsc_dflags . hscEnv -> df) <- runActionE "classplugin.addMethodPlaceholders.GhcSessionDeps" state
        $ useE GhcSessionDeps nfp
    (old, new) <- handleMaybeM (PluginInternalError "Unable to makeEditText")
        $ liftIO $ runMaybeT
        $ makeEditText pm df param
    pragmaInsertion <- insertPragmaIfNotPresent state nfp InstanceSigs
    let edit =
            if withSig
            then mergeEdit (workspaceEdit caps old new) pragmaInsertion
            else workspaceEdit caps old new

    void $ lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())

    pure $ InR Null
    where
        toTextDocumentEdit edit =
            TextDocumentEdit (verTxtDocId ^.re _versionedTextDocumentIdentifier) [InL edit]

        mergeEdit :: WorkspaceEdit -> [TextEdit] -> WorkspaceEdit
        mergeEdit WorkspaceEdit{..} edits = WorkspaceEdit
            { _documentChanges =
                (\x -> x ++ map (InL . toTextDocumentEdit) edits)
                    <$> _documentChanges
            , ..
            }

        workspaceEdit caps old new
            = diffText caps (verTxtDocId, old) new IncludeDeletions

-- |
-- This implementation is ad-hoc in a sense that the diagnostic detection mechanism is
-- sensitive to the format of diagnostic messages from GHC.
codeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeAction recorder state plId (CodeActionParams _ _ docId caRange _) = do
    verTxtDocId <- liftIO $ runAction "classplugin.codeAction.getVersionedTextDoc" state $ getVersionedTextDoc docId
    nfp <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
    activeDiagnosticsInRange (shakeExtras state) nfp caRange
        >>= \case
        Nothing -> pure $ InL []
        Just fileDiags -> do
            actions <- join <$> mapM (mkActions nfp verTxtDocId) (methodDiags fileDiags)
            pure $ InL actions
    where
        methodDiags fileDiags =
            mapMaybe (\d -> (d,) <$> isClassMethodWarning (d ^. fdStructuredMessageL)) fileDiags

        mkActions
            :: NormalizedFilePath
            -> VersionedTextDocumentIdentifier
            -> (FileDiagnostic, ClassMinimalDef)
            -> ExceptT PluginError (HandlerM Ide.Plugin.Config.Config) [Command |? CodeAction]
        mkActions docPath verTxtDocId (diag, classMinDef) = do
            ClassInstancesResult instMap <- runActionE "classplugin.codeAction.GetClassInstances" state
                $ useE GetClassInstances docPath
            inst <- handleMaybe (PluginInvalidUserState "no instance at diagnostic range")
                $ listToMaybe (RangeMap.filterByRange range instMap)
            (tmrTypechecked -> gblEnv) <- runActionE "classplugin.codeAction.TypeCheck" state $ useE TypeCheck docPath
            (hscEnv -> hsc) <- runActionE "classplugin.codeAction.GhcSession" state $ useE GhcSession docPath
            logWith recorder Debug (LogImplementedMethods (hsc_dflags hsc) (instClass inst) classMinDef)
            pure
                $ concatMap mkAction
                $ nubOrdOn snd
                $ filter ((/=) mempty . snd)
                $ mkMethodGroups hsc gblEnv inst classMinDef
            where
                range = diag ^. fdLspDiagnosticL . L.range

                mkMethodGroups :: HscEnv -> TcGblEnv -> InstanceInfo -> ClassMinimalDef -> [MethodGroup]
                mkMethodGroups hsc gblEnv inst classMinDef = minimalDef <> [allClassMethods]
                    where
                        methods    = instMethods inst
                        minimalDef = minDefToMethodGroups hsc gblEnv methods classMinDef
                        allClassMethods =
                            ("all missing methods", map (makeMethodDefinition hsc gblEnv) methods)

                mkAction :: MethodGroup -> [Command |? CodeAction]
                mkAction (name, methods)
                    = [ mkCodeAction title
                            $ mkLspCommand plId codeActionCommandId title
                                (Just $ mkCmdParams methods False)
                      , mkCodeAction titleWithSig
                            $ mkLspCommand plId codeActionCommandId titleWithSig
                                (Just $ mkCmdParams methods True)
                      ]
                    where
                        title = "Add placeholders for " <> name
                        titleWithSig = title <> " with signature(s)"

                mkCmdParams :: [(T.Text, T.Text)] -> Bool -> [Value]
                mkCmdParams methodGroup withSig =
                    [toJSON (AddMinimalMethodsParams verTxtDocId range methodGroup withSig)]

                mkCodeAction title cmd
                    = InR
                    $ CodeAction
                        title
                        (Just CodeActionKind_QuickFix)
                        (Just [])
                        Nothing
                        Nothing
                        Nothing
                        (Just cmd)
                        Nothing

isClassMethodWarning :: StructuredMessage -> Maybe ClassMinimalDef
isClassMethodWarning message = case message ^? _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnMessage of
    Nothing          -> Nothing
    Just tcRnMessage -> isUnsatisfiedMinimalDefWarning tcRnMessage

isUnsatisfiedMinimalDefWarning :: TcRnMessage -> Maybe ClassMinimalDef
isUnsatisfiedMinimalDefWarning = stripTcRnMessageContext >>> \case
    TcRnUnsatisfiedMinimalDef classMinDef -> Just classMinDef
    _ -> Nothing

type MethodSignature = T.Text
type MethodName = T.Text
type MethodDefinition = (MethodName, MethodSignature)
type MethodGroup = (T.Text, [MethodDefinition])

makeMethodDefinition :: HscEnv -> TcGblEnv -> (Name, Type) -> MethodDefinition
makeMethodDefinition hsc gblEnv (name, ty) = (nameTxt, signature)
    where
        -- nameTxt is bare (no parens); ExactPrint.makeMethodDecl applies
        -- toMethodName to wrap operators when emitting the placeholder.
        nameTxt   = printOutputable name
        signature = toMethodName nameTxt <> " :: " <> T.pack (showDoc hsc gblEnv ty)

minDefToMethodGroups :: HscEnv -> TcGblEnv -> [(Name, Type)] -> ClassMinimalDef -> [MethodGroup]
minDefToMethodGroups hsc gblEnv methods minDef = makeMethodGroup <$> go minDef
    where
        makeMethodGroup methodDefinitions =
            let name = mconcat $ intersperse "," $ (\x -> "'" <> x <> "'") . fst <$> methodDefinitions
            in  (name, methodDefinitions)

        matchMethod n =
            map (makeMethodDefinition hsc gblEnv)
                $ filter ((== n) . fst) methods
#if __GLASGOW_HASKELL__ >= 913
        go (Var lmn)  = pure $ matchMethod (unLoc lmn)
#else
        go (Var mn)   = pure $ matchMethod mn
#endif
        go (Or ms)    = concatMap (go . unLoc) ms
        go (And ms)   = foldr (liftA2 (<>) . go . unLoc) [[]] ms
        go (Parens m) = go (unLoc m)
