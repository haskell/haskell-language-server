{-# LANGUAGE GADTs           #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.CodeAction where

import           Control.Applicative                  (liftA2)
import           Control.Lens                         hiding (List, use)
import           Control.Monad.Extra
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except           (ExceptT, throwE)
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import           Data.Bifunctor                       (second)
import           Data.Either.Extra                    (rights)
import           Data.List
import qualified Data.Map.Strict                      as Map
import           Data.Maybe                           (isNothing, listToMaybe,
                                                       mapMaybe)
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
import qualified Ide.Plugin.Config
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens              as J

addMethodPlaceholders :: PluginId -> CommandFunction IdeState AddMinimalMethodsParams
addMethodPlaceholders _ state param@AddMinimalMethodsParams{..} = do
    caps <- getClientCapabilities
    pluginResponse $ do
        nfp <- getNormalizedFilePath uri
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
        toTextDocumentEdit edit =
            TextDocumentEdit (VersionedTextDocumentIdentifier uri (Just 0)) (List [InL edit])

        mergeEdit :: WorkspaceEdit -> [TextEdit] -> WorkspaceEdit
        mergeEdit WorkspaceEdit{..} edits = WorkspaceEdit
            { _documentChanges =
                (\(List x) -> List $ x ++ map (InL . toTextDocumentEdit) edits)
                    <$> _documentChanges
            , ..
            }

        workspaceEdit caps old new
            = diffText caps (uri, old) new IncludeDeletions

-- |
-- This implementation is ad-hoc in a sense that the diagnostic detection mechanism is
-- sensitive to the format of diagnostic messages from GHC.
codeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState TextDocumentCodeAction
codeAction recorder state plId (CodeActionParams _ _ docId _ context) = pluginResponse $ do
    nfp <- getNormalizedFilePath uri
    actions <- join <$> mapM (mkActions nfp) methodDiags
    pure $ List actions
    where
        uri = docId ^. J.uri
        List diags = context ^. J.diagnostics

        ghcDiags = filter (\d -> d ^. J.source == Just "typecheck") diags
        methodDiags = filter (\d -> isClassMethodWarning (d ^. J.message)) ghcDiags

        mkActions
            :: NormalizedFilePath
            -> Diagnostic
            -> ExceptT String (LspT Ide.Plugin.Config.Config IO) [Command |? CodeAction]
        mkActions docPath diag = do
            (HAR {hieAst = ast}, pmap) <- handleMaybeM "Unable to GetHieAst"
                . liftIO
                . runAction "classplugin.findClassIdentifier.GetHieAst" state
                $ useWithStale GetHieAst docPath
            instancePosition <- handleMaybe "No range" $
                              fromCurrentRange pmap range ^? _Just . J.start
                              & fmap (J.character -~ 1)
            ident <- findClassIdentifier ast instancePosition
            cls <- findClassFromIdentifier docPath ident
            InstanceBindTypeSigsResult sigs <- handleMaybeM "Unable to GetInstanceBindTypeSigs"
                $ liftIO
                $ runAction "classplugin.codeAction.GetInstanceBindTypeSigs" state
                $ use GetInstanceBindTypeSigs docPath
            implemented <- findImplementedMethods ast instancePosition
            logWith recorder Info (LogImplementedMethods cls implemented)
            pure
                $ concatMap mkAction
                $ nubBy (\(_, x) (_,y) -> x == y)
                $ filter ((/=) mempty . snd)
                $ fmap (second (filter (\(bind, _) -> bind `notElem` implemented)))
                $ mkSuggestions range sigs cls
            where
                range = diag ^. J.range
                mkSuggestions range sigs cls = minimalDef <> [allClassMethods]
                    where
                        minimalDef = minDefToMethodGroups range sigs $ classMinimalDef cls
                        allClassMethods = foo range sigs

                mkAction :: Suggestion -> [Command |? CodeAction]
                mkAction (name, methodGroup)
                    = [ mkCodeAction title
                            $ mkLspCommand plId codeActionCommandId title
                                (Just $ mkCmdParams methodGroup False)
                      , mkCodeAction titleWithSig
                            $ mkLspCommand plId codeActionCommandId titleWithSig
                                (Just $ mkCmdParams methodGroup True)
                      ]
                    where
                        title = "Add placeholders for " <> name
                        titleWithSig = title <> " with signature(s)"

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

        findClassIdentifier hf instancePosition =
            handleMaybe "No Identifier found"
                $ listToMaybe
                $ mapMaybe listToMaybe
                $ pointCommand hf instancePosition
                    ( (Map.keys . Map.filter isClassNodeIdentifier . getNodeIds)
                        <=< nodeChildren
                    )

        findImplementedMethods
            :: HieASTs a
            -> Position
            -> ExceptT String (LspT Ide.Plugin.Config.Config IO) [T.Text]
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
            (hscEnv -> hscenv, _) <- handleMaybeM "Unable to GhcSessionDeps"
                . liftIO
                . runAction "classplugin.findClassFromIdentifier.GhcSessionDeps" state
                $ useWithStale GhcSessionDeps docPath
            (tmrTypechecked -> thisMod, _) <- handleMaybeM "Unable to TypeCheck"
                . liftIO
                . runAction "classplugin.findClassFromIdentifier.TypeCheck" state
                $ useWithStale TypeCheck docPath
            handleMaybeM "TcEnv"
                . liftIO
                . fmap snd
                . initTcWithGbl hscenv thisMod ghostSpan $ do
                    tcthing <- tcLookup name
                    case tcthing of
                        AGlobal (AConLike (RealDataCon con))
                            | Just cls <- tyConClass_maybe (dataConOrigTyCon con) -> pure cls
                        _ -> fail "Ide.Plugin.Class.findClassFromIdentifier"
        findClassFromIdentifier _ (Left _) = throwE "Ide.Plugin.Class.findClassIdentifier"

isClassNodeIdentifier :: IdentifierDetails a -> Bool
isClassNodeIdentifier ident = (isNothing . identType) ident && Use `Set.member` identInfo ident

isClassMethodWarning :: T.Text -> Bool
isClassMethodWarning = T.isPrefixOf "• No explicit implementation for"

isInstanceValBind :: ContextInfo -> Bool
isInstanceValBind (ValBind InstanceBind _ _) = True
isInstanceValBind _                          = False

type MethodSig = T.Text
type MethodName = T.Text
type MethodGroup = (MethodName, MethodSig)
type Suggestion = (T.Text, [MethodGroup])

makeMethodGroup :: InstanceBindTypeSig -> MethodGroup
makeMethodGroup sig = (name, signature) 
    where
        name = T.drop (T.length bindingPrefix) (printOutputable  (bindName sig))
        signature = bindRendered sig

foo :: Range -> [InstanceBindTypeSig] -> Suggestion
foo range sigs = ("all missing methods", methodGroups)
    where
        methodGroups = [ makeMethodGroup sig
                        | sig <- sigs
                        , inRange range (getSrcSpan $ bindName sig)
                       ]

-- Return (name text, signature text)
minDefToMethodGroups :: Range -> [InstanceBindTypeSig] -> BooleanFormula Name -> [Suggestion]
minDefToMethodGroups range sigs minDef = suggestions
    where
        makeSuggestion methodGroup = 
            let name = mconcat $ intersperse "," $ (\x -> "'" <> x <> "'") . fst <$> methodGroup
            in  (name, methodGroup)

        suggestions = makeSuggestion <$> go minDef

        go (Var mn)   = [[ makeMethodGroup sig
                        | sig <- sigs
                        , inRange range (getSrcSpan $ bindName sig)
                        , printOutputable mn == T.drop (T.length bindingPrefix) (printOutputable (bindName sig))
                        ]]
        go (Or ms)    = concatMap (go . unLoc) ms
        go (And ms)   = foldr (liftA2 (<>)) [[]] (fmap (go . unLoc) ms)
        go (Parens m) = go (unLoc m)

