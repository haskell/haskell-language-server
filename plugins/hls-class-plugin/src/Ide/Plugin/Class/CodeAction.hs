{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.CodeAction where

import           Control.Lens                         hiding (List, use)
import           Control.Monad.Error.Class            (MonadError (throwError))
import           Control.Monad.Extra
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (ExceptT)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           hiding (Null)
import           Data.Bifunctor                       (second)
import           Data.Either.Extra                    (rights)
import           Data.List
import           Data.List.Extra                      (nubOrdOn)
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isNothing, listToMaybe,
                                                       mapMaybe)
import qualified Data.Set                             as Set
import qualified Data.Text                            as T
import           Development.IDE
import           Development.IDE.Core.Compile         (sourceTypecheck)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (fromCurrentRange)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.Spans.AtPoint        (pointCommand)
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
codeAction recorder state plId (CodeActionParams _ _ docId _ context) = do
    verTxtDocId <- lift $ pluginGetVersionedTextDoc docId
    nfp <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
    actions <- join <$> mapM (mkActions nfp verTxtDocId) methodDiags
    pure $ InL actions
    where
        diags = context ^. L.diagnostics

        ghcDiags = filter (\d -> d ^. L.source == Just sourceTypecheck) diags
        methodDiags = filter (\d -> isClassMethodWarning (d ^. L.message)) ghcDiags

        mkActions
            :: NormalizedFilePath
            -> VersionedTextDocumentIdentifier
            -> Diagnostic
            -> ExceptT PluginError (PluginM Ide.Plugin.Config.Config) [Command |? CodeAction]
        mkActions docPath verTxtDocId diag = do
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
            implemented <- findImplementedMethods ast instancePosition
            logWith recorder Info (LogImplementedMethods cls implemented)
            pure
                $ concatMap mkAction
                $ nubOrdOn snd
                $ filter ((/=) mempty . snd)
                $ fmap (second (filter (\(bind, _) -> bind `notElem` implemented)))
                $ mkMethodGroups hsc gblEnv range sigs cls
            where
                range = diag ^. L.range

                mkMethodGroups :: HscEnv -> TcGblEnv -> Range -> [InstanceBindTypeSig] -> Class -> [MethodGroup]
                mkMethodGroups hsc gblEnv range sigs cls = minimalDef <> [allClassMethods]
                    where
                        minimalDef = minDefToMethodGroups hsc gblEnv range sigs $ classMinimalDef cls
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

        findImplementedMethods
            :: HieASTs a
            -> Position
            -> ExceptT PluginError (PluginM Ide.Plugin.Config.Config) [T.Text]
        findImplementedMethods asts instancePosition = do
            pure
                $ concat
                $ pointCommand asts instancePosition
                $ map (T.pack . getOccString) . rights . findInstanceValBindIdentifiers

        -- | Recurses through the given AST to find identifiers which are
        -- 'InstanceValBind's.
        findInstanceValBindIdentifiers :: HieAST a -> [Identifier]
        findInstanceValBindIdentifiers ast =
            let valBindIds = Map.keys
                            . Map.filter (any isInstanceValBind . identInfo)
                            $ getNodeIds ast
            in valBindIds <> concatMap findInstanceValBindIdentifiers (nodeChildren ast)

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

isClassMethodWarning :: T.Text -> Bool
isClassMethodWarning = T.isPrefixOf "• No explicit implementation for"

isInstanceValBind :: ContextInfo -> Bool
isInstanceValBind (ValBind InstanceBind _ _) = True
isInstanceValBind _                          = False

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

