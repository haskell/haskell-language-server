{-# LANGUAGE CPP          #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}
-- | An HLS plugin to provide code actions to change type signatures
module Ide.Plugin.ChangeTypeSignature (descriptor
                                      -- * For Unit Tests
                                      , Log(..)
                                      , errorMessageRegexes
                                      ) where

import           Control.Lens
import           Control.Monad                     (guard)
import           Control.Monad.IO.Class            (MonadIO)
import           Control.Monad.Trans.Class         (MonadTrans (lift))
import           Control.Monad.Trans.Except        (ExceptT (..))
import           Control.Monad.Trans.Maybe         (MaybeT (..), hoistMaybe)
import           Data.Foldable                     (asum)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Development.IDE                   (FileDiagnostic,
                                                    IdeState (..), Pretty (..),
                                                    Priority (..), Recorder,
                                                    WithPriority,
                                                    fdLspDiagnosticL,
                                                    fdStructuredMessageL,
                                                    logWith, realSrcSpanToRange)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes    (GetParsedModule (GetParsedModule))
import           Development.IDE.GHC.Compat        hiding (vcat)
import           Development.IDE.GHC.Compat.Error  (_MismatchMessage,
                                                    _TcRnMessageWithCtx,
                                                    _TcRnMessageWithInfo,
                                                    _TcRnSolverReport,
                                                    _TypeEqMismatchActual,
                                                    _TypeEqMismatchExpected,
                                                    msgEnvelopeErrorL,
                                                    reportContentL)
import           Development.IDE.GHC.Util          (printOutputable)
import           Development.IDE.Types.Diagnostics (_SomeStructuredMessage)
import           Generics.SYB                      (extQ, something)
import           GHC.Tc.Errors.Types               (ErrInfo (..),
                                                    TcRnMessageDetailed (..))
#if MIN_VERSION_ghc(9,13,0)
import           GHC.Tc.Errors.Ppr                 (pprErrCtxtMsg)
import           GHC.Utils.Outputable              (vcat)
#endif
import qualified Ide.Logger                        as Logger
import           Ide.Plugin.Error                  (PluginError,
                                                    getNormalizedFilePathE)
import           Ide.Types                         (Config, HandlerM,
                                                    PluginDescriptor (..),
                                                    PluginId (PluginId),
                                                    PluginMethodHandler,
                                                    defaultPluginDescriptor,
                                                    mkPluginHandler)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Text.Regex.TDFA                   ((=~))

data Log
    = LogErrInfoCtxt ErrInfo
    | LogFindSigLocFailure DeclName

instance Pretty Log where
    pretty = \case
#if MIN_VERSION_ghc(9,13,0)
        LogErrInfoCtxt (ErrInfo ctxt _ _) ->
            fromSDoc (vcat $ map pprErrCtxtMsg ctxt)
#else
        LogErrInfoCtxt (ErrInfo ctxt suppl) ->
            Logger.vcat [fromSDoc ctxt, fromSDoc suppl]
#endif
        LogFindSigLocFailure name ->
            pretty ("Lookup signature location failure: " <> name)
        where
            fromSDoc = pretty . printOutputable

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provides a code action to change the type signature of a binding if it is wrong")
        { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction (codeActionHandler recorder plId)
        }

codeActionHandler
    :: Recorder (WithPriority Log)
    -> PluginId
    -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
codeActionHandler recorder plId ideState _ CodeActionParams{_textDocument, _range} = do
    let TextDocumentIdentifier uri = _textDocument
    nfp <- getNormalizedFilePathE uri
    decls <- getDecls plId ideState nfp

    activeDiagnosticsInRange (shakeExtras ideState) nfp _range >>= \case
        Nothing -> pure (InL [])
        Just fileDiags -> do
            actions <- lift $ mapM (generateAction recorder plId uri decls) fileDiags
            pure (InL (catMaybes actions))

getDecls :: MonadIO m => PluginId -> IdeState -> NormalizedFilePath -> ExceptT PluginError m [LHsDecl GhcPs]
getDecls (PluginId changeTypeSignatureId) state =
    runActionE (T.unpack changeTypeSignatureId <> ".GetParsedModule") state
    . fmap (hsmodDecls . unLoc . pm_parsed_source)
    . useE GetParsedModule

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
                         , diagnostic  :: FileDiagnostic
                         }

-- | Create a CodeAction from a Diagnostic
generateAction
    :: Recorder (WithPriority Log)
    -> PluginId
    -> Uri
    -> [LHsDecl GhcPs]
    -> FileDiagnostic
    -> HandlerM Config (Maybe (Command |? CodeAction))
generateAction recorder plId uri decls fileDiag = do
    changeSig <- diagnosticToChangeSig recorder decls fileDiag
    pure $
        changeSigToCodeAction plId uri <$> changeSig

-- | Convert a diagnostic into a ChangeSignature and add the proper SrcSpan
diagnosticToChangeSig
    :: Recorder (WithPriority Log)
    -> [LHsDecl GhcPs]
    -> FileDiagnostic
    -> HandlerM Config (Maybe ChangeSignature)
diagnosticToChangeSig recorder decls diagnostic = runMaybeT $ do
    -- Extract expected, actual, and extra error info
    (expectedType, actualType, errInfo) <- hoistMaybe $ do
        msg <- diagnostic ^. fdStructuredMessageL ^? _SomeStructuredMessage
        tcRnMsg <- msg ^. msgEnvelopeErrorL ^? _TcRnMessageWithCtx
        (_, TcRnMessageDetailed errInfo tcRnMsg') <- tcRnMsg ^? _TcRnMessageWithInfo
        solverReport <- tcRnMsg' ^? _TcRnSolverReport . _1 . reportContentL
        mismatch <- solverReport ^? _MismatchMessage
        expectedType <- mismatch ^? _TypeEqMismatchExpected
        actualType <- mismatch ^? _TypeEqMismatchActual

        pure (showType expectedType, showType actualType, errInfo)

    logWith recorder Debug (LogErrInfoCtxt errInfo)

    -- Extract the declName from the extra error text
    declName <- hoistMaybe (matchingDiagnostic errInfo)

    -- Look up location of declName. If it fails, log it
    declSrcSpan <-
        case findSigLocOfStringDecl decls expectedType (T.unpack declName) of
            Just x -> pure x
            Nothing -> do
                logWith recorder Debug (LogFindSigLocFailure declName)
                hoistMaybe Nothing

    pure ChangeSignature{..}
    where
        showType :: Type -> Text
        showType = T.pack . showSDocUnsafe . pprTidiedType

-- | If a diagnostic has the proper message create a ChangeSignature from it
matchingDiagnostic :: ErrInfo -> Maybe DeclName
matchingDiagnostic ErrInfo{errInfoContext} =
    asum $ map (unwrapMatch . (=~) errInfoTxt) errorMessageRegexes
    where
        unwrapMatch :: (Text, Text, Text, [Text]) -> Maybe DeclName
        unwrapMatch (_, _, _, [name]) = Just name
        unwrapMatch _                 = Nothing

#if MIN_VERSION_ghc(9,13,0)
        errInfoTxt = printOutputable (vcat $ map pprErrCtxtMsg errInfoContext)
#else
        errInfoTxt = printOutputable errInfoContext
#endif

-- | List of regexes that match various Error Messages
errorMessageRegexes :: [Text]
errorMessageRegexes = [ -- be sure to add new Error Messages Regexes at the bottom to not fail any existing tests
    "In an equation for ‘(.+)’:"
    ]

-- | Given a String with the name of a declaration, GHC's "Expected Type", find the declaration that matches
-- both the name given and the Expected Type, and return the type signature location
findSigLocOfStringDecl :: [LHsDecl GhcPs] -> ExpectedSig -> String -> Maybe RealSrcSpan
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
changeSigToCodeAction (PluginId changeTypeSignatureId) uri ChangeSignature{..} =
    InR CodeAction { _title       = mkChangeSigTitle declName actualType
                   , _kind        = Just (CodeActionKind_Custom ("quickfix." <> changeTypeSignatureId))
                   , _diagnostics = Just [diagnostic ^. fdLspDiagnosticL ]
                   , _isPreferred = Nothing
                   , _disabled    = Nothing
                   , _edit        = Just $ mkChangeSigEdit uri declSrcSpan (mkNewSignature declName actualType)
                   , _command     = Nothing
                   , _data_       = Nothing
                   }

mkChangeSigTitle :: Text -> Text -> Text
mkChangeSigTitle declName actualType = "Change signature for ‘" <> declName <> "’ to: " <> actualType

mkChangeSigEdit :: Uri -> RealSrcSpan -> Text -> WorkspaceEdit
mkChangeSigEdit uri ss replacement =
        let txtEdit = TextEdit (realSrcSpanToRange ss) replacement
            changes = Just $ Map.singleton uri [txtEdit]
        in WorkspaceEdit changes Nothing Nothing

mkNewSignature :: Text -> Text -> Text
mkNewSignature declName actualType = declName <> " :: " <> actualType
