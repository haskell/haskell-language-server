{-# LANGUAGE LambdaCase #-}
-- | An HLS plugin to provide code actions to change type signatures
module Ide.Plugin.ChangeTypeSignature (descriptor
                                      -- * For Unit Tests
                                      , errorMessageRegexes
                                      , tidyTypeSignature) where

import           Control.Monad                  (join)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Except     (ExceptT)
import           Data.Char                      (isAlpha, isAlphaNum, isNumber,
                                                 isPunctuation)
import qualified Data.HashMap.Strict            as Map
import           Data.List                      (find, nub)
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (isJust, isNothing, listToMaybe,
                                                 mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Traversable               (forM)
import           Development.IDE.Core.RuleTypes (GetParsedModule (GetParsedModule))
import           Development.IDE.Core.Service   (IdeState, ideLogger, runAction)
import           Development.IDE.Core.Shake     (use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (realSrcSpanToRange)
import           Development.IDE.GHC.Util       (unsafePrintSDoc)
import           Development.IDE.Types.Logger   (logDebug)
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
import           Text.Regex.TDFA                (AllTextMatches (getAllTextMatches),
                                                 (=~))

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId) { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionHandler }

codeActionHandler :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionHandler ideState plId CodeActionParams {_textDocument = TextDocumentIdentifier uri, _context = CodeActionContext (List diags) _} = response $ do
      nfp <- getNormalizedFilePath plId (TextDocumentIdentifier uri)
      decls <- getDecls ideState nfp
      let sigs = [ sigToText ts | L (RealSrcSpan rss _) (SigD _ ts@(TypeSig _ idsSig _)) <- decls]
      let actions = mapMaybe (generateAction uri decls) diags
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
                         expectedType :: ExpectedSig
                                       ,
                         -- | the Actual Type based on definition
                         actualType   :: ActualSig
                                       ,
                         -- | the declaration name to be updated
                         declName     :: DeclName
                                       ,
                         -- | the location of the declaration signature
                         declSrcSpan  :: RealSrcSpan
                                       ,
                         -- | the diagnostic to solve
                         diagnostic   :: Diagnostic
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
    (expectedType, actualType', declName) <- matchingDiagnostic diagnostic
    let actualType = tidyTypeSignature actualType'
    -- Find the definition and it's location
    (declSrcSpan, ghcSig) <- findSigLocOfStringDecl decls (T.unpack declName)
    -- Make sure the given "Actual Type" is a full signature
    isValidMessage expectedType ghcSig
    pure $ ChangeSignature{..}

-- | Does the GHC Error Message give us a Signature we can use?
-- We only want to change signatures when the "expected signature" given by GHC
-- matches the one given by the user
isValidMessage :: DefinedSig -> ExpectedSig -> Maybe ()
isValidMessage defSig expSig = if defSig == expSig then Just () else Nothing

-- | If a diagnostic has the proper message create a ChangeSignature from it
matchingDiagnostic :: Diagnostic -> Maybe (ExpectedSig, ActualSig, DeclName)
matchingDiagnostic diag@Diagnostic{_message} = case map (unwrapMatch . (=~) _message) errorMessageRegexes of
                                                       []  -> Nothing
                                                       css -> join $ find isJust css
    where
        unwrapMatch :: (Text, Text, Text, [Text]) -> Maybe (ExpectedSig, ActualSig, DeclName)
        -- due to using (.|\n) in regex we have to drop the erroneous, but necessary ("." doesn't match newlines), match
        unwrapMatch (_, _, _, [exp, act, _, name]) = Just (exp, act, name)
        unwrapMatch _                              = Nothing

-- | List of regexes that match various Error Messages
errorMessageRegexes :: [Text]
errorMessageRegexes = [ -- be sure to add new Error Messages Regexes at the bottom to not fail any existing tests
    "Expected type: (" <> typeSigRegex <> ")\n +Actual type: (" <> typeSigRegex <> ")\n(.|\n)+In an equation for ‘(.+)’"
    , "Couldn't match expected type ‘(" <> typeSigRegex <> ")’ with actual type ‘(" <> typeSigRegex <> ")’\n(.|\n)+In an equation for ‘(.+)’"
    ]
    where
        -- The brackets in position 1 and 2 MUST BE IN THAT ORDER
        -- Any other combination of brackets seems to fail
        typeSigRegex = "[][a-zA-Z0-9 ->]+"

-- | Given a String with the name of a declaration, find that declarations and give back
-- the type signature location and the full signature
-- This is a modified version of functions found in Development.IDE.Plugin.CodeAction
-- This function returns the actual location of the signature rather than the actual signature
-- We also don't have access to `fun_id` or other actual `id` so we must use string compare instead
findSigLocOfStringDecl :: SigName => [LHsDecl GhcPs] -> String -> Maybe (RealSrcSpan, DefinedSig)
findSigLocOfStringDecl decls declName = do
    -- can we simplify this logic? Just want to make sure ghcSig is a Just value
    (rss, Just ghcSig) <- listToMaybe [ (locA rss, sigToText ts)
                                        | L (RealSrcSpan rss _) (SigD _ ts@(TypeSig _ idsSig _)) <- decls,
                                        any ((==) declName . occNameString . occName . unLoc) idsSig
                                      ]
    pure (rss, ghcSig)

-- | Pretty Print the Type Signature (to validate GHC Error Message)
sigToText :: Sig GhcPs -> Maybe Text
sigToText = \case
  ts@TypeSig {} -> stripSignature (T.pack $ showSDocUnsafe $ ppr ts)
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
                                                               , _kind        = Just CodeActionQuickFix
                                                               , _diagnostics = Just $ List [diagnostic]
                                                               , _isPreferred = Nothing
                                                               , _disabled    = Nothing
                                                               , _edit        = Just $ mkChangeSigEdit uri declSrcSpan (mkNewSignature declName actualType)
                                                               , _command     = Nothing
                                                               , _xdata       = Nothing
                                                               }

mkChangeSigTitle :: Text -> Text -> Text
mkChangeSigTitle declName actualType = "change signature for ‘" <> declName <> "’ to: " <> actualType

mkChangeSigEdit :: Uri -> RealSrcSpan -> Text -> WorkspaceEdit
mkChangeSigEdit uri ss replacement =
        let txtEdit = TextEdit (realSrcSpanToRange ss) replacement
            changes = Just $ Map.singleton uri (List [txtEdit])
        in WorkspaceEdit changes Nothing Nothing

mkNewSignature :: Text -> Text -> Text
mkNewSignature declName actualType = declName <> " :: " <> actualType

-- | Cleans up type signatures with oddly-named type variables (a0, m0 ...)
tidyTypeSignature :: Text -> Text
tidyTypeSignature actualType =
    replaceAllFTyVars
    $ replaceAllMTyVars
    $ replaceAllTTyVars
    $ replaceAllUniqueTyVars actualType
    where
        -- split on whitespace and remove any non-alphaNum chars
        words' = filter (not . T.null) $ map (T.filter isAlphaNum) $ T.words actualType
        -- words' = T.words actualType
        -- find the tyVars which are just alphaChars (a, b, thing ...)
        tidyTyVars = nub $ filter (T.all isAlpha) words'
        -- find tyVars with numbers (a0, b1, c5 ...)
        untidyTyVars = nub $ filter (T.any isNumber) words'

        freshTcVars' = freshTcVars tidyTyVars
        (fTyVars, mTyVars, tTyVars, uniqueTyVars)  = untidyTyVarsToGroup untidyTyVars

        --  From a list of type variables replace all instances with the list of freshVars provided
        replaceAllTyVars :: [Text] -> Text -> [Text] -> Text
        replaceAllTyVars freshVars sig untidy = snd $ foldl replaceWithFreshVar (freshVars, sig) untidy

        -- we must reverse the untidy vars as they are "backwards" when lookint at type sig from Left to Right
        replaceAllUniqueTyVars sig = replaceAllTyVars (filter (`notElem` tidyTyVars) freshUniqueVars) sig (reverse uniqueTyVars)
        replaceAllFTyVars sig = replaceAllTyVars (freshTcVars' "f") sig (reverse fTyVars)
        replaceAllMTyVars sig = replaceAllTyVars (freshTcVars' "m") sig (reverse mTyVars)
        replaceAllTTyVars sig = replaceAllTyVars (freshTcVars' "t") sig (reverse tTyVars)

replaceWithFreshVar :: ([Text], Text) -> Text -> ([Text], Text)
replaceWithFreshVar (replacement:rest, haystack) needle = (rest, T.replace needle replacement haystack)
replaceWithFreshVar _ _ = error "How do I handle this..., the pattern match for infinite list shouldn't fail"

freshTcVars :: [Text] -> Text -> [Text]
freshTcVars used var = filter (`notElem` used) $ var : [var <> T.pack (show i) | i <- [0 .. ]]

freshUniqueVars ::  [Text]
freshUniqueVars = map T.singleton validVars <> [ a <> b | a <- freshUniqueVars, b <- freshUniqueVars ]
    where
        validVars = filter (`notElem` ['f', 'm', 't']) ['a'..'z']

-- | divvy up UntidyTyVars into their tyVar category
untidyTyVarsToGroup :: [Text] -> ([Text], [Text], [Text], [Text])
untidyTyVarsToGroup untidy = foldl dispatchVar ([], [], [], []) untidy
    where
        dispatchVar acc@(fVars, mVars, tVars, uVars) var = case T.uncons var of
            Just ('f', rest) -> (var:fVars, mVars, tVars, uVars)
            Just ('m', rest) -> (fVars, var:mVars, tVars, uVars)
            Just ('t', rest) -> (fVars, mVars, var:tVars, uVars)
            Just _           -> (fVars, mVars, tVars, var:uVars)
            _                -> acc
