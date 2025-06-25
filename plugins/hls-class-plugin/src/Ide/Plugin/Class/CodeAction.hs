{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.CodeAction (
    addMethodPlaceholders,
    codeAction,
) where

import           Control.Arrow                        ((>>>))
import           Control.Lens                         hiding (List, use)
import           Control.Monad.Error.Class            (MonadError (throwError))
import           Control.Monad.Extra
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           hiding (Null)
import           Data.List
import           Data.List.Extra                      (nubOrdOn)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isNothing, listToMaybe,
                                                       mapMaybe)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.FileStore       (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (fromCurrentRange)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error     (TcRnMessage (..),
                                                       _TcRnMessage,
                                                       msgEnvelopeErrorL,
                                                       stripTcRnMessageContext)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Spans.AtPoint        (pointCommand)
import           GHC.Iface.Ext.Types                  (ContextInfo (..),
                                                       HieAST (..), Identifier,
                                                       IdentifierDetails (..))
import           Ide.Plugin.Class.ExactPrint
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import qualified Ide.Plugin.Config
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
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
            (HAR {hieAst = ast}, pmap) <- runActionE "classplugin.findClassIdentifier.GetHieAst" state
                $ useWithStaleE GetHieAst docPath
            instancePosition <- handleMaybe (PluginInvalidUserState "fromCurrentRange") $
                              fromCurrentRange pmap range ^? _Just . L.start
                              & fmap (L.character -~ 1)
            ident <- findClassIdentifier ast instancePosition
            cls <- findClassFromIdentifier docPath ident
            InstanceBindTypeSigsResult sigs <- runActionE "classplugin.codeAction.GetInstanceBindTypeSigs" state
                $ useE GetInstanceBindTypeSigs docPath
            (tmrTypechecked -> gblEnv ) <- runActionE "classplugin.codeAction.TypeCheck" state $ useE TypeCheck docPath
            (hscEnv -> hsc) <- runActionE "classplugin.codeAction.GhcSession" state $ useE GhcSession docPath
            logWith recorder Debug (LogImplementedMethods (hsc_dflags hsc) cls classMinDef)
            pure
                $ concatMap mkAction
                $ nubOrdOn snd
                $ filter ((/=) mempty . snd)
                $ mkMethodGroups hsc gblEnv range sigs classMinDef
            where
                range = diag ^. fdLspDiagnosticL . L.range

                mkMethodGroups :: HscEnv -> TcGblEnv -> Range -> [InstanceBindTypeSig] -> ClassMinimalDef -> [MethodGroup]
                mkMethodGroups hsc gblEnv range sigs classMinDef = minimalDef <> [allClassMethods]
                    where
                        minimalDef = minDefToMethodGroups hsc gblEnv range sigs classMinDef
                        allClassMethods = ("all missing methods", makeMethodDefinitions hsc gblEnv range sigs)

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

        findClassIdentifier hf instancePosition =
            handleMaybe (PluginInternalError "No Identifier found")
                $ listToMaybe
                $ mapMaybe listToMaybe
                $ pointCommand hf instancePosition
                    ( (Map.keys . Map.filterWithKey isClassNodeIdentifier . getNodeIds)
                        <=< nodeChildren
                    )

        findClassFromIdentifier docPath (Right name) = do
            (hscEnv -> hscenv, _) <- runActionE "classplugin.findClassFromIdentifier.GhcSessionDeps" state
                $ useWithStaleE GhcSessionDeps docPath
            (tmrTypechecked -> thisMod, _) <- runActionE "classplugin.findClassFromIdentifier.TypeCheck" state
                $ useWithStaleE TypeCheck docPath
            handleMaybeM (PluginInternalError "initTcWithGbl failed")
                . liftIO
                . fmap snd
                . initTcWithGbl hscenv thisMod ghostSpan $ do
                    tcthing <- tcLookup name
                    case tcthing of
                        AGlobal (AConLike (RealDataCon con))
                            | Just cls <- tyConClass_maybe (dataConOrigTyCon con) -> pure cls
                        _ -> fail "Ide.Plugin.Class.findClassFromIdentifier"
        findClassFromIdentifier _ (Left _) = throwError (PluginInternalError "Ide.Plugin.Class.findClassIdentifier")

-- see https://hackage.haskell.org/package/ghc-9.8.1/docs/src/GHC.Types.Name.Occurrence.html#mkClassDataConOcc
isClassNodeIdentifier :: Identifier -> IdentifierDetails a -> Bool
isClassNodeIdentifier (Right i) ident  | 'C':':':_ <- unpackFS $ occNameFS $ occName i = (isNothing . identType) ident && Use `Set.member` identInfo ident
isClassNodeIdentifier _ _ = False

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

makeMethodDefinition :: HscEnv -> TcGblEnv -> InstanceBindTypeSig -> MethodDefinition
makeMethodDefinition hsc gblEnv sig = (name, signature)
    where
        name = T.drop (T.length bindingPrefix) (printOutputable  (bindName sig))
        signature = prettyBindingNameString (printOutputable (bindName sig)) <> " :: " <> T.pack (showDoc hsc gblEnv (bindType sig))

makeMethodDefinitions :: HscEnv -> TcGblEnv -> Range -> [InstanceBindTypeSig] -> [MethodDefinition]
makeMethodDefinitions hsc gblEnv range sigs =
    [ makeMethodDefinition hsc gblEnv sig
    | sig <- sigs
    , inRange range (getSrcSpan $ bindName sig)
    ]

signatureToName :: InstanceBindTypeSig -> T.Text
signatureToName sig = T.drop (T.length bindingPrefix) (printOutputable (bindName sig))

-- Return [groupName text, [(methodName text, signature text)]]
minDefToMethodGroups :: HscEnv -> TcGblEnv -> Range -> [InstanceBindTypeSig] -> BooleanFormula Name -> [MethodGroup]
minDefToMethodGroups hsc gblEnv range sigs minDef = makeMethodGroup <$> go minDef
    where
        makeMethodGroup methodDefinitions =
            let name = mconcat $ intersperse "," $ (\x -> "'" <> x <> "'") . fst <$> methodDefinitions
            in  (name, methodDefinitions)

        go (Var mn)   = pure $ makeMethodDefinitions hsc gblEnv range $ filter ((==) (printOutputable mn) . signatureToName) sigs
        go (Or ms)    = concatMap (go . unLoc) ms
        go (And ms)   = foldr (liftA2 (<>) . go . unLoc) [[]] ms
        go (Parens m) = go (unLoc m)

