-- | An HLS plugin to provide code lenses for type signatures
module Development.IDE.Plugin.TypeLenses
  ( descriptor,
    suggestSignature,
    typeLensCommandId,
  )
where

import Data.Aeson.Types (Value (..), toJSON)
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Development.IDE.Core.RuleTypes (TypeCheck (TypeCheck))
import Development.IDE.Core.Rules (IdeState, runAction)
import Development.IDE.Core.Service (getDiagnostics)
import Development.IDE.Core.Shake (getHiddenDiagnostics, use)
import Development.IDE.Types.Location
  ( Position (Position, _character, _line),
    Range (Range, _end, _start),
    toNormalizedFilePath',
    uriToFilePath',
  )
import Ide.PluginUtils (mkLspCommand)
import Ide.Types
  ( CommandFunction,
    CommandId (CommandId),
    PluginCommand (PluginCommand),
    PluginDescriptor (pluginCodeLensProvider, pluginCommands),
    PluginId,
    defaultPluginDescriptor,
  )
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.Types
  ( ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
    CodeLens (CodeLens),
    CodeLensParams (CodeLensParams, _textDocument),
    Diagnostic (..),
    List (..),
    ResponseError,
    ServerMethod (WorkspaceApplyEdit),
    TextDocumentIdentifier (TextDocumentIdentifier),
    TextEdit (TextEdit),
    WorkspaceEdit (WorkspaceEdit),
  )
import Text.Regex.TDFA ((=~))

typeLensCommandId :: T.Text
typeLensCommandId = "typesignature.add"

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCodeLensProvider = Just codeLensProvider,
      pluginCommands = [PluginCommand (CommandId typeLensCommandId) "adds a signature" commandHandler]
    }

codeLensProvider ::
  LSP.LspFuncs c ->
  IdeState ->
  PluginId ->
  CodeLensParams ->
  IO (Either ResponseError (List CodeLens))
codeLensProvider _lsp ideState pId CodeLensParams {_textDocument = TextDocumentIdentifier uri} = do
  fmap (Right . List) $ case uriToFilePath' uri of
    Just (toNormalizedFilePath' -> filePath) -> do
      _ <- runAction "codeLens" ideState (use TypeCheck filePath)
      diag <- getDiagnostics ideState
      hDiag <- getHiddenDiagnostics ideState
      sequence
        [ generateLens pId _range title edit
          | (dFile, _, dDiag@Diagnostic {_range = _range}) <- diag ++ hDiag,
            dFile == filePath,
            (title, tedit) <- suggestSignature False dDiag,
            let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ]
    Nothing -> pure []

generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> IO CodeLens
generateLens pId _range title edit = do
  cId <- mkLspCommand pId (CommandId typeLensCommandId) title (Just [toJSON edit])
  return $ CodeLens _range (Just cId) Nothing

commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _lsp _ideState wedit =
    return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams wedit))

suggestSignature :: Bool -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix Diagnostic {_range = _range@Range {..}, ..}
  | _message
      =~ ("(Top-level binding|Polymorphic local binding|Pattern synonym) with no type signature" :: T.Text) =
    let signature =
          removeInitialForAll $
            T.takeWhile (\x -> x /= '*' && x /= '•') $
              T.strip $ unifySpaces $ last $ T.splitOn "type signature: " $ filterNewlines _message
        startOfLine = Position (_line _start) startCharacter
        beforeLine = Range startOfLine startOfLine
        title = if isQuickFix then "add signature: " <> signature else signature
        action = TextEdit beforeLine $ signature <> "\n" <> T.replicate startCharacter " "
     in [(title, [action])]
  where
    removeInitialForAll :: T.Text -> T.Text
    removeInitialForAll (T.breakOnEnd " :: " -> (nm, ty))
      | "forall" `T.isPrefixOf` ty = nm <> T.drop 2 (snd (T.breakOn "." ty))
      | otherwise = nm <> ty
    startCharacter
      | "Polymorphic local binding" `T.isPrefixOf` _message =
        _character _start
      | otherwise =
        0
suggestSignature _ _ = []

unifySpaces :: T.Text -> T.Text
unifySpaces = T.unwords . T.words

filterNewlines :: T.Text -> T.Text
filterNewlines = T.concat . T.lines
