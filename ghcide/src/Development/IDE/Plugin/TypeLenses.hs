-- | An HLS plugin to provide code lenses for type signatures
module Development.IDE.Plugin.TypeLenses
  ( descriptor,
    suggestSignature,
    typeLensCommandId,
  )
where

import           ConLike                             (ConLike (PatSynCon))
import           Control.Applicative                 ((<|>))
import           Control.Monad.IO.Class
import           Data.Aeson.Types                    (Value (..), toJSON)
import           Data.Generics                       (mkQ, something)
import qualified Data.HashMap.Strict                 as Map
import           Data.List                           (find)
import qualified Data.Text                           as T
import           Development.IDE.Core.Compile        (TcModuleResult (..))
import           Development.IDE.Core.RuleTypes      (GetBindings (GetBindings),
                                                      TypeCheck (TypeCheck))
import           Development.IDE.Core.Rules          (IdeState, runAction)
import           Development.IDE.Core.Service        (getDiagnostics)
import           Development.IDE.Core.Shake          (getHiddenDiagnostics, use)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util            (printName)
import           Development.IDE.Spans.Common        (safeTyThingType)
import           Development.IDE.Spans.LocalBindings (Bindings, getFuzzyScope)
import           Development.IDE.Types.Location      (Position (Position, _character, _line),
                                                      Range (Range, _end, _start),
                                                      toNormalizedFilePath',
                                                      uriToFilePath')
import           HscTypes                            (lookupTypeEnv,
                                                      mkPrintUnqualified)
import           Ide.PluginUtils                     (mkLspCommand)
import           Ide.Types                           (CommandFunction,
                                                      CommandId (CommandId),
                                                      PluginCommand (PluginCommand),
                                                      PluginDescriptor (..),
                                                      PluginId,
                                                      defaultPluginDescriptor,
                                                      mkPluginHandler)
import qualified Language.LSP.Server                 as LSP
import           Language.LSP.Types                  (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                      CodeLens (CodeLens),
                                                      CodeLensParams (CodeLensParams, _textDocument),
                                                      Diagnostic (..),
                                                      List (..), ResponseError,
                                                      SMethod (..),
                                                      TextDocumentIdentifier (TextDocumentIdentifier),
                                                      TextEdit (TextEdit),
                                                      WorkspaceEdit (WorkspaceEdit))
import           Outputable                          (showSDocForUser)
import           TcRnTypes                           (TcGblEnv (TcGblEnv, tcg_rdr_env, tcg_rn_decls, tcg_type_env))
import           TcType                              (pprSigmaType)
import           Text.Regex.TDFA                     ((=~))

typeLensCommandId :: T.Text
typeLensCommandId = "typesignature.add"

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeLens codeLensProvider,
      pluginCommands = [PluginCommand (CommandId typeLensCommandId) "adds a signature" commandHandler]
    }

codeLensProvider ::
  IdeState ->
  PluginId ->
  CodeLensParams ->
  LSP.LspM c (Either ResponseError (List CodeLens))
codeLensProvider ideState pId CodeLensParams {_textDocument = TextDocumentIdentifier uri} = do
  fmap (Right . List) $ case uriToFilePath' uri of
    Just (toNormalizedFilePath' -> filePath) -> liftIO $ do
      tmr <- runAction "codeLens.TypeCheck" ideState (use TypeCheck filePath)
      bindings <- runAction "bindings.GetBindings" ideState (use GetBindings filePath)
      diag <- getDiagnostics ideState
      hDiag <- getHiddenDiagnostics ideState
      sequence
        [ generateLens pId _range title edit
          | (dFile, _, dDiag@Diagnostic {_range = _range}) <- diag ++ hDiag,
            dFile == filePath,
            (title, tedit) <- suggestSignature False tmr bindings dDiag,
            let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ]
    Nothing -> pure []

generateLens :: PluginId -> Range -> T.Text -> WorkspaceEdit -> IO CodeLens
generateLens pId _range title edit = do
  let cId = mkLspCommand pId (CommandId typeLensCommandId) title (Just [toJSON edit])
  return $ CodeLens _range (Just cId) Nothing

commandHandler :: CommandFunction IdeState WorkspaceEdit
commandHandler _ideState wedit = do
  _ <- LSP.sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedit) (\_ -> pure ())
  return $ Right Null

suggestSignature :: Bool -> Maybe TcModuleResult -> Maybe Bindings -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix mTmr mBindings Diagnostic {_message, _range = Range {..}}
  | _message
      =~ ("(Top-level binding|Polymorphic local binding|Pattern synonym) with no type signature" :: T.Text),
    Just bindings <- mBindings,
    Just TcModuleResult {tmrTypechecked = TcGblEnv {tcg_type_env, tcg_rn_decls, tcg_rdr_env}} <- mTmr,
    localScope <- getFuzzyScope bindings _start _end,
    Just group <- tcg_rn_decls,
    Just name <- getFirstIdAtLine (succ $ _line _start) group,
    Just (isPatSyn, ty) <-
      (lookupTypeEnv tcg_type_env name >>= \x -> (isTyThingPatSyn x,) <$> safeTyThingType x)
        <|> ((False,) <$> (find (\(x, _) -> x == name) localScope >>= snd)),
    tyMsg <- showSDocForUser unsafeGlobalDynFlags (mkPrintUnqualified unsafeGlobalDynFlags tcg_rdr_env) $ pprSigmaType ty,
    signature <- T.pack $ (if isPatSyn then "pattern " else "") <> printName name <> " :: " <> tyMsg,
    startCharacter <- if "local binding" `T.isInfixOf` _message then _character _start else 0,
    startOfLine <- Position (_line _start) startCharacter,
    beforeLine <- Range startOfLine startOfLine,
    title <- if isQuickFix then "add signature: " <> signature else signature,
    action <- TextEdit beforeLine $ signature <> "\n" <> T.replicate startCharacter " " =
    [(title, [action])]
  | otherwise = []

isTyThingPatSyn :: TyThing -> Bool
isTyThingPatSyn (AConLike (PatSynCon _)) = True
isTyThingPatSyn _                        = False

getFirstIdAtLine :: Int -> HsGroup GhcRn -> Maybe Name
getFirstIdAtLine line = something (mkQ Nothing f)
  where
    f :: Located Name -> Maybe Name
    f (L l name)
      | RealSrcSpan s <- l,
        srcSpanStartLine s == line =
        Just name
      | otherwise = Nothing
