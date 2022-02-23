{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
-- | An HLS plugin to provide code actions to change type signatures
module Ide.Plugin.ChangeTypeSignature (descriptor
                                      -- * For Unit Tests
                                      , errorMessageRegexes
                                      ) where

import           Control.Monad                       (forM, guard, join)
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Control.Monad.Trans.Except          (ExceptT)
import           Data.Char                           (isAlpha, isAlphaNum,
                                                      isNumber, isPunctuation)
import           Data.Foldable                       (asum, traverse_)
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (find, nub)
import qualified Data.List.NonEmpty                  as NE
import           Data.Maybe                          (isJust, isNothing,
                                                      listToMaybe, mapMaybe)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Development.IDE.Core.RuleTypes      (GetBindings (GetBindings),
                                                      GetParsedModule (GetParsedModule))
import           Development.IDE.Core.Service        (IdeState, ideLogger,
                                                      runAction)
import           Development.IDE.Core.Shake          (use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error           (realSrcSpanToRange)
import           Development.IDE.GHC.Util            (prettyPrint,
                                                      unsafePrintSDoc)
import           Development.IDE.Spans.LocalBindings (Bindings, getFuzzyScope)
import           Development.IDE.Types.Location      (Range (Range))
import           Development.IDE.Types.Logger        (logDebug, logInfo)
import           Generics.SYB                        (extQ, mkQ, something,
                                                      somewhere)
import           Ide.PluginUtils                     (getNormalizedFilePath,
                                                      handleMaybeM, response)
import           Ide.Types                           (PluginDescriptor (..),
                                                      PluginId,
                                                      PluginMethodHandler,
                                                      defaultPluginDescriptor,
                                                      mkPluginHandler)
import           Language.LSP.Types                  (CodeAction (..),
                                                      CodeActionContext (CodeActionContext),
                                                      CodeActionKind (CodeActionQuickFix, CodeActionUnknown),
                                                      CodeActionParams (..),
                                                      Command, Diagnostic (..),
                                                      List (..),
                                                      Method (TextDocumentCodeAction),
                                                      NormalizedFilePath,
                                                      SMethod (..),
                                                      TextDocumentIdentifier (TextDocumentIdentifier),
                                                      TextEdit (TextEdit), Uri,
                                                      WorkspaceEdit (WorkspaceEdit),
                                                      type (|?) (InR))
import           Text.Regex.TDFA                     (AllTextMatches (getAllTextMatches),
                                                      (=~))

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId) { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionHandler }

codeActionHandler :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionHandler ideState plId CodeActionParams {_textDocument = TextDocumentIdentifier uri, _context = CodeActionContext (List diags) _, _range} = response $ do
      nfp <- getNormalizedFilePath plId (TextDocumentIdentifier uri)
      -- for changing top-level bindings signatures
      decls <- getDecls ideState nfp

      let actions = mapMaybe (generateAction uri decls) diags

      -- liftIO $ logInfo (ideLogger ideState) $ T.pack $ show binds
      pure $ List actions

getDecls :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m [LHsDecl GhcPs]
getDecls state = handleMaybeM "Error: Could not get Parsed Module"
    . liftIO
    . fmap (fmap (hsmodDecls . unLoc . pm_parsed_source))
    . runAction "changeSignature.GetParsedModule" state
    . use GetParsedModule

-- | Text representing a Declaration's Name
type DeclName = Text
-- | The signature provided by GHC Error Message (Expected type)
type ExpectedSig = Text
-- | The signature provided by GHC Error Message (Actual type)
type ActualSig = Text
-- | The signature defined by the user
type DefinedSig = Text

-- | DataType that encodes the necessary information for changing a type signature
data ChangeSignature = ChangeSignature {
                         -- | The expected type based on Signature
                         expectedType  :: ExpectedSig
                         -- | the Actual Type based on definition
                         , actualType  :: ActualSig
                         -- | the declaration name to be updated
                         , declName    :: DeclName
                         -- | the location of the declaration signature
                         , declSrcSpan :: RealSrcSpan
                         -- | the diagnostic to solve
                         , diagnostic  :: Diagnostic
                                       }

-- | Constraint needed to trackdown OccNames in signatures
type SigName = (HasOccName (IdP GhcPs))

-- | Create a CodeAction from a Diagnostic
generateAction :: SigName => Uri -> [LHsDecl GhcPs] -> Diagnostic -> Maybe (Command |? CodeAction)
generateAction uri decls diag = changeSigToCodeAction uri <$> diagnosticToChangeSig decls diag

-- | Convert a diagnostic into a ChangeSignature and add the proper SrcSpan
diagnosticToChangeSig :: SigName => [LHsDecl GhcPs] -> Diagnostic -> Maybe ChangeSignature
diagnosticToChangeSig decls diagnostic = do
    -- regex match on the GHC Error Message
    (expectedType, actualType, declName) <- matchingDiagnostic diagnostic
    -- Find the definition and it's location
    declSrcSpan <- findSigLocOfStringDecl decls expectedType (T.unpack declName)
    pure $ ChangeSignature{..}


-- | If a diagnostic has the proper message create a ChangeSignature from it
matchingDiagnostic :: Diagnostic -> Maybe (ExpectedSig, ActualSig, DeclName)
matchingDiagnostic diag@Diagnostic{_message} = asum $ map (unwrapMatch . (=~) _message) errorMessageRegexes
    where
        unwrapMatch :: (Text, Text, Text, [Text]) -> Maybe (ExpectedSig, ActualSig, DeclName)
        -- due to using (.|\n) in regex we have to drop the erroneous, but necessary ("." doesn't match newlines), match
        unwrapMatch (_, _, _, [exp, act, _, name]) = Just (exp, act, name)
        unwrapMatch _                              = Nothing

-- | List of regexes that match various Error Messages
errorMessageRegexes :: [Text]
errorMessageRegexes = [ -- be sure to add new Error Messages Regexes at the bottom to not fail any existing tests
    "Expected type: (.+)\n +Actual type: (.+)\n(.|\n)+In an equation for ‘(.+)’"
    , "Couldn't match expected type ‘(.+)’ with actual type ‘(.+)’\n(.|\n)+In an equation for ‘(.+)’"
    ]

-- | Given a String with the name of a declaration, GHC's "Expected Type", find the declaration that matches
-- both the name given and the Expected Type, and return the type signature location
findSigLocOfStringDecl :: SigName => [LHsDecl GhcPs] -> ExpectedSig -> String -> Maybe RealSrcSpan
findSigLocOfStringDecl decls expectedType declName = something (const Nothing `extQ` findSig `extQ` findLocalSig) decls
    where
        -- search for Top Level Signatures
        findSig :: GenLocated SrcSpan (HsDecl GhcPs) -> Maybe RealSrcSpan
        findSig = \case
            L (locA -> (RealSrcSpan rss _)) (SigD _ sig) -> case sig of
              ts@(TypeSig _ idsSig _) -> isMatch ts idsSig >> pure rss
              _                       -> Nothing
            _ -> Nothing

        -- search for Local Signatures
        findLocalSig :: GenLocated SrcSpan (Sig GhcPs) -> Maybe RealSrcSpan
        findLocalSig = \case
          (L (locA -> (RealSrcSpan rss _)) ts@(TypeSig _ idsSig _)) -> isMatch ts idsSig >> pure rss
          _          -> Nothing

        -- Does the declName match? and does the expected signature match?
        isMatch ts idsSig = do
                ghcSig <- sigToText ts
                guard (any compareId idsSig && expectedType == ghcSig)

        -- Given an IdP check to see if it matches the declName
        compareId (L _ id') = declName == occNameString (occName id')



-- | Pretty Print the Type Signature (to validate GHC Error Message)
sigToText :: Sig GhcPs -> Maybe Text
sigToText = \case
  ts@TypeSig {} -> stripSignature $ T.pack $ prettyPrint ts
  _             -> Nothing

stripSignature :: Text -> Maybe Text
-- for whatever reason incoming signatures MAY have new lines after "::" or "=>"
stripSignature sig = case T.filter (/= '\n') sig =~ sigRegex :: (Text, Text, Text, [Text]) of
                        -- No constraints (Monad m =>)
                         (_, _, _, [sig'])    -> Just $ T.strip sig'
                        -- Ignore constraints (Monad m =>)
                         (_, _, _, [_, sig']) -> Just $ T.strip sig'
                         _                    -> Nothing
    where
        -- we want to test everthing after the constraints (GHC never gives us the constraint in the expected signature)
        sigRegex = ".* :: (.*=>)?(.*)" :: Text


changeSigToCodeAction :: Uri -> ChangeSignature -> Command |? CodeAction
changeSigToCodeAction uri ChangeSignature{..} = InR CodeAction { _title       = mkChangeSigTitle declName actualType
                                                               , _kind        = Just (CodeActionUnknown "quickfix.changeSignature")
                                                               , _diagnostics = Just $ List [diagnostic]
                                                               , _isPreferred = Nothing
                                                               , _disabled    = Nothing
                                                               , _edit        = Just $ mkChangeSigEdit uri declSrcSpan (mkNewSignature declName actualType)
                                                               , _command     = Nothing
                                                               , _xdata       = Nothing
                                                               }

mkChangeSigTitle :: Text -> Text -> Text
mkChangeSigTitle declName actualType = "Change signature for ‘" <> declName <> "’ to: " <> actualType

mkChangeSigEdit :: Uri -> RealSrcSpan -> Text -> WorkspaceEdit
mkChangeSigEdit uri ss replacement =
        let txtEdit = TextEdit (realSrcSpanToRange ss) replacement
            changes = Just $ Map.singleton uri (List [txtEdit])
        in WorkspaceEdit changes Nothing Nothing

mkNewSignature :: Text -> Text -> Text
mkNewSignature declName actualType = declName <> " :: " <> actualType
