-- | An HLS plugin to provide code actions to change type signatures
module Ide.Plugin.ChangeTypeSignature (descriptor) where

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
      let actions = mapMaybe (generateAction uri decls) diags
      pure $ List actions

getDecls :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m [LHsDecl GhcPs]
getDecls state = handleMaybeM "Error: Could not get Parsed Module"
    . liftIO
    . fmap (fmap (hsmodDecls . unLoc . pm_parsed_source))
    . runAction "changeSignature.GetParsedModule" state
    . use GetParsedModule

-- | DataType that encodes the necessary information for changing a type signature:
-- `expectedType` - The Expected Type
-- `actualType`   - The Actual Type
-- `declName`     - The Declaration Name that we are going to change
-- `declSrcSpan`  - The Source Span where the signature is located
-- `diagnostic`   - The Error Diagnostic we aim to solve
-- `uri`          - The URI the declaration is located in
data ChangeSignature = ChangeSignature { expectedType :: Text
                                       , actualType   :: Text
                                       , declName     :: Text
                                       , declSrcSpan  :: Maybe RealSrcSpan
                                       , diagnostic   :: Diagnostic
                                       , uri          :: Uri
                                       }

-- | Constraint needed to trackdown OccNames in signatures
type SigName p = (HasOccName (IdP (GhcPass p)))

-- | Create a CodeAction from a Diagnostic
generateAction :: SigName p => Uri -> [LHsDecl (GhcPass p)] -> Diagnostic -> Maybe (Command |? CodeAction)
generateAction uri decls diag = diagnosticToChangeSig uri decls diag >>= changeSigToCodeAction

-- | Convert a diagnostic into a ChangeSignature and add the proper SrcSpan
diagnosticToChangeSig :: SigName p => Uri -> [LHsDecl (GhcPass p)] -> Diagnostic -> Maybe ChangeSignature
diagnosticToChangeSig uri decls diag = addSrcSpan decls <$> matchingDiagnostic uri diag

-- | If a diagnostic has the proper message create a ChangeSignature from it
matchingDiagnostic :: Uri -> Diagnostic -> Maybe ChangeSignature
matchingDiagnostic uri diag@Diagnostic{_message} = unwrapMatch $ _message =~ expectedMessage
    where
        expectedMessage = "Expected type: (.+)\n +Actual type: (.+)\n.*\n +In an equation for ‘(.+)’" :: Text
        unwrapMatch :: (Text, Text, Text, [Text]) -> Maybe ChangeSignature
        unwrapMatch (_, _, _, [exp, act, name]) = Just $ ChangeSignature exp act name Nothing diag uri
        unwrapMatch _                           = Nothing

-- | Given a String with the name of a declaration, find that declarations type signature location
-- This is a modified version of functions found in Development.IDE.Plugin.CodeAction
-- This function returns the actual location of the signature rather than the actual signature
-- We also don't have access to `fun_id` or other actual `id` so we must use string compare instead
findSigLocOfStringDecl :: SigName p => [LHsDecl (GhcPass p)] -> String -> Maybe RealSrcSpan
findSigLocOfStringDecl decls declName =
  listToMaybe
    [ locA rss
      | L (RealSrcSpan rss _) (SigD _ (TypeSig _ idsSig _)) <- decls,
        any ((==) declName . occNameString . occName . unLoc) idsSig
    ]

-- | Update a ChangeSignature to potentially populate `declSrcSpan`
addSrcSpan :: SigName p => [LHsDecl (GhcPass p)] -> ChangeSignature -> ChangeSignature
addSrcSpan _ self@(ChangeSignature _ _ _ (Just _) _ _) = self
addSrcSpan decls chgSig@ChangeSignature{..} = chgSig { declSrcSpan = findSigLocOfStringDecl decls (T.unpack declName) }

changeSigToCodeAction :: ChangeSignature -> Maybe (Command |? CodeAction)
-- Does not generate a Code action if declSrcSpan is Nothing
changeSigToCodeAction ChangeSignature{..} = do
    realSrcSpan <- declSrcSpan
    pure (InR CodeAction { _title       = mkChangeSigTitle declName actualType
                         , _kind        = Just CodeActionQuickFix
                         , _diagnostics = Just $ List [diagnostic]
                         , _isPreferred = Nothing
                         , _disabled    = Nothing
                         , _edit        = Just $ mkChangeSigEdit uri realSrcSpan (mkNewSignature declName actualType)
                         , _command     = Nothing
                         , _xdata       = Nothing
                         })
mkChangeSigTitle :: Text -> Text -> Text
mkChangeSigTitle declName actualType = "change signature for ‘" <> declName <> "’ to: " <> actualType

mkChangeSigEdit :: Uri -> RealSrcSpan -> Text -> WorkspaceEdit
mkChangeSigEdit uri ss replacement =
        let txtEdit = TextEdit (realSrcSpanToRange ss) replacement
            changes = Just $ Map.singleton uri (List [txtEdit])
        in WorkspaceEdit changes Nothing Nothing

mkNewSignature :: Text -> Text -> Text
mkNewSignature declName actualType = declName <> " :: " <> actualType

--------------------------------------------------------------------------------
-- test :: Set.Set Int -> Int
-- test = go
--     where
--         go = head . Set.toList
