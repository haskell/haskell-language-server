{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
-- | An HLS plugin to provide code actions to change type signatures
module Ide.Plugin.ChangeTypeSignature (descriptor
                                      -- * For Unit Tests
                                      , errorMessageRegexes
                                      ) where

import           Control.Monad                  (guard)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Except     (ExceptT)
import           Data.Foldable                  (asum)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Development.IDE                (realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes (GetParsedModule (GetParsedModule))
import           Development.IDE.Core.Service   (IdeState, runAction)
import           Development.IDE.Core.Shake     (use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util       (printOutputable)
import           Generics.SYB                   (extQ, something)
import           Ide.PluginUtils                (getNormalizedFilePath,
                                                 handleMaybeM, pluginResponse)
import           Ide.Types                      (PluginDescriptor (..),
                                                 PluginId (PluginId),
                                                 PluginMethodHandler,
                                                 defaultPluginDescriptor,
                                                 mkPluginHandler)
import           Language.LSP.Types
import           Text.Regex.TDFA                ((=~))

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId) { pluginHandlers = mkPluginHandler STextDocumentCodeAction (codeActionHandler plId) }

codeActionHandler :: PluginId -> PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionHandler plId ideState _ CodeActionParams {_textDocument = TextDocumentIdentifier uri, _context = CodeActionContext (List diags) _} = pluginResponse $ do
      nfp <- getNormalizedFilePath uri
      decls <- getDecls plId ideState nfp
      let actions = mapMaybe (generateAction plId uri decls) diags
      pure $ List actions

getDecls :: MonadIO m => PluginId -> IdeState -> NormalizedFilePath -> ExceptT String m [LHsDecl GhcPs]
getDecls (PluginId changeTypeSignatureId) state = handleMaybeM "Could not get Parsed Module"
    . liftIO
    . fmap (fmap (hsmodDecls . unLoc . pm_parsed_source))
    . runAction (T.unpack changeTypeSignatureId <> ".GetParsedModule") state
    . use GetParsedModule

-- | Text representing a Declaration's Name
type DeclName = Text
-- | The signature provided by GHC Error Message (Expected type)
type ExpectedSig = Text
-- | The signature provided by GHC Error Message (Actual type)
type ActualSig = Text

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
generateAction :: SigName => PluginId -> Uri -> [LHsDecl GhcPs] -> Diagnostic -> Maybe (Command |? CodeAction)
generateAction plId uri decls diag = changeSigToCodeAction plId uri <$> diagnosticToChangeSig decls diag

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
matchingDiagnostic Diagnostic{_message} = asum $ map (unwrapMatch . (=~) _message) errorMessageRegexes
    where
        unwrapMatch :: (Text, Text, Text, [Text]) -> Maybe (ExpectedSig, ActualSig, DeclName)
        -- due to using (.|\n) in regex we have to drop the erroneous, but necessary ("." doesn't match newlines), match
        unwrapMatch (_, _, _, [expect, actual, _, name]) = Just (expect, actual, name)
        unwrapMatch _                              = Nothing

-- | List of regexes that match various Error Messages
errorMessageRegexes :: [Text]
errorMessageRegexes = [ -- be sure to add new Error Messages Regexes at the bottom to not fail any existing tests
    "Expected type: (.+)\n +Actual type: (.+)\n(.|\n)+In an equation for ‘(.+)’"
    , "Couldn't match expected type ‘(.+)’ with actual type ‘(.+)’\n(.|\n)+In an equation for ‘(.+)’"
    -- GHC >9.2 version of the first error regex
    , "Expected: (.+)\n +Actual: (.+)\n(.|\n)+In an equation for ‘(.+)’"
    ]

-- | Given a String with the name of a declaration, GHC's "Expected Type", find the declaration that matches
-- both the name given and the Expected Type, and return the type signature location
findSigLocOfStringDecl :: SigName => [LHsDecl GhcPs] -> ExpectedSig -> String -> Maybe RealSrcSpan
findSigLocOfStringDecl decls expectedType declName = something (const Nothing `extQ` findSig `extQ` findLocalSig) decls
    where
        -- search for Top Level Signatures
        findSig :: LHsDecl GhcPs -> Maybe RealSrcSpan
        findSig = \case
            L (locA -> (RealSrcSpan rss _)) (SigD _ sig) -> case sig of
              ts@(TypeSig _ idsSig _) -> isMatch ts idsSig >> pure rss
              _                       -> Nothing
            _ -> Nothing

        -- search for Local Signatures
        findLocalSig :: LSig GhcPs -> Maybe RealSrcSpan
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
  ts@TypeSig {} -> Just $ stripSignature $ printOutputable ts
  _             -> Nothing

stripSignature :: Text -> Text
-- for whatever reason incoming signatures MAY have new lines after "::" or "=>"
stripSignature (T.filter (/= '\n') -> sig) = if T.isInfixOf " => " sig
                                                -- remove constraints
                                                then T.strip $ snd $ T.breakOnEnd " => " sig
                                                else T.strip $ snd $ T.breakOnEnd " :: " sig

changeSigToCodeAction :: PluginId -> Uri -> ChangeSignature -> Command |? CodeAction
changeSigToCodeAction (PluginId changeTypeSignatureId) uri ChangeSignature{..} = InR CodeAction { _title       = mkChangeSigTitle declName actualType
                                                               , _kind        = Just (CodeActionUnknown ("quickfix." <> changeTypeSignatureId))
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
