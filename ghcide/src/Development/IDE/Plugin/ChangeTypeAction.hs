{-# LANGUAGE ConstraintKinds #-}
-- | An HLS plugin to provide code actions to change type signatures
module Development.IDE.Plugin.ChangeTypeAction (descriptor) where

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Except     (ExceptT)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                     (listToMaybe, mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Development.IDE.Core.RuleTypes (GetParsedModule (GetParsedModule))
import           Development.IDE.Core.Service   (IdeState, runAction)
import           Development.IDE.Core.Shake     (use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (realSrcSpanToRange)
import           Ide.PluginUtils                (getNormalizedFilePath,
                                                 handleMaybeM, response)
import           Ide.Types                      (PluginDescriptor (..),
                                                 PluginId, PluginMethodHandler,
                                                 defaultPluginDescriptor,
                                                 mkPluginHandler)
import           Language.LSP.Types             (CodeAction (..),
                                                 CodeActionContext (CodeActionContext),
                                                 CodeActionKind (CodeActionQuickFix),
                                                 CodeActionParams (..), Command,
                                                 Diagnostic (..), List (..),
                                                 Method (TextDocumentCodeAction),
                                                 NormalizedFilePath,
                                                 SMethod (..),
                                                 TextDocumentIdentifier (TextDocumentIdentifier),
                                                 TextEdit (TextEdit), Uri,
                                                 WorkspaceEdit (WorkspaceEdit),
                                                 type (|?) (InR))
import           Text.Regex.TDFA                ((=~))

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId) { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionHandler }

codeActionHandler :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionHandler ideState plId CodeActionParams {_textDocument = TextDocumentIdentifier uri, _context = CodeActionContext (List diags) _} = response $ do
      nfp <- getNormalizedFilePath plId (TextDocumentIdentifier uri)
      decls <- getDecls ideState nfp
      let actions = generateActions uri decls diags
      pure $ List actions

getDecls :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m [LHsDecl GhcPs]
getDecls state = handleMaybeM "Error: Could not get Parsed Module"
    . liftIO
    . fmap (fmap (hsmodDecls . unLoc . pm_parsed_source))
    . runAction "changeSignature.GetParsedModule" state
    . use GetParsedModule

data ChangeSignature = ChangeSignature { expectedType :: Text
                                       , actualType   :: Text
                                       , declName     :: Text
                                       , declSrcSpan  :: Maybe SrcSpan
                                       , diagnostic   :: Diagnostic
                                       , uri          :: Uri
                                       }

-- Needed to trackdown OccNames in signatures
type SigName p = (HasOccName (IdP (GhcPass p)))

-- | Generate CodeActions from a list of Diagnostics
generateActions :: SigName p => Uri -> [LHsDecl (GhcPass p)] -> [Diagnostic] -> [Command |? CodeAction]
generateActions uri = mapMaybe . generateAction uri

-- | Create a CodeAction from a Diagnostic
generateAction :: SigName p => Uri -> [LHsDecl (GhcPass p)] -> Diagnostic -> Maybe (Command |? CodeAction)
generateAction uri decls diag = diagnosticToChangeSig uri decls diag >>= changeSigToCodeAction

-- | Convert a diagnostic into a ChangeSignature and add the proper SrcSpan
diagnosticToChangeSig :: SigName p => Uri -> [LHsDecl (GhcPass p)] -> Diagnostic -> Maybe ChangeSignature
diagnosticToChangeSig uri decls diag = addSrcSpan decls <$> matchingDiagnostic uri diag

-- | If a diagnostic has the proper message create a ChangeSignature from it
matchingDiagnostic :: Uri -> Diagnostic -> Maybe ChangeSignature
matchingDiagnostic uri diag@Diagnostic{_message} = unwrapMatch $ _message =~ ("Expected type: (.+)\n +Actual type: (.+)\n.*\n +In an equation for ‘(.+)’" :: Text)
    where
        unwrapMatch :: (Text, Text, Text, [Text]) -> Maybe ChangeSignature
        unwrapMatch (_, _, _, [exp, act, name]) = Just $ ChangeSignature exp act name Nothing diag uri
        unwrapMatch _                           = Nothing

-- | Given a String with the name of a declaration, find that declarations type signature location
-- This is a modified version of functions found in Development.IDE.Plugin.CodeAction
findSigLocOfStringDecl :: SigName p => [LHsDecl (GhcPass p)] -> String -> Maybe SrcSpan
findSigLocOfStringDecl decls declName =
  listToMaybe
    [ srcSpan
      | L srcSpan (SigD _ (TypeSig _ idsSig _)) <- decls,
        any ((==) declName . occNameString . occName . unLoc) idsSig
    ]

-- | Update a ChangeSignature to potentially populate `declSrcSpan`
addSrcSpan :: SigName p => [LHsDecl (GhcPass p)] -> ChangeSignature -> ChangeSignature
addSrcSpan _ self@(ChangeSignature _ _ _ (Just _) _ _) = self
addSrcSpan decls chgSig@ChangeSignature{..} = chgSig { declSrcSpan = findSigLocOfStringDecl decls (T.unpack declName) }

changeSigToCodeAction :: ChangeSignature -> Maybe (Command |? CodeAction)
-- Does not generate a Code action if declSrcSpan is Nothing
changeSigToCodeAction ChangeSignature{..} = declSrcSpan *> Just (InR CodeAction { _title       = mkChangeSigTitle declName actualType
                                                                                , _kind        = Just CodeActionQuickFix
                                                                                , _diagnostics = Just $ List [diagnostic]
                                                                                , _isPreferred = Nothing
                                                                                , _disabled    = Nothing
                                                                                -- This CAN but probably never will be Nothing
                                                                                , _edit        = mkChangeSigEdit uri declSrcSpan (mkNewSignature declName actualType)
                                                                                , _command     = Nothing
                                                                                , _xdata       = Nothing
                                                                                })
mkChangeSigTitle :: Text -> Text -> Text
mkChangeSigTitle declName actualType = "change signature for ‘" <> declName <> "’ to: " <> actualType

mkChangeSigEdit :: Uri -> Maybe SrcSpan -> Text -> Maybe WorkspaceEdit
mkChangeSigEdit uri (Just (RealSrcSpan ss _)) replacement =
        let txtEdit = TextEdit (realSrcSpanToRange ss) replacement
            changes = Just $ Map.singleton uri (List [txtEdit])
        in Just $ WorkspaceEdit changes Nothing Nothing
mkChangeSigEdit _ _ _                                   = Nothing

mkNewSignature :: Text -> Text -> Text
mkNewSignature declName actualType = declName <> " :: " <> actualType

--------------------------------------------------------------------------------
-- test :: Set.Set Int -> Int
-- test = go
--     where
--         go = head . Set.toList
