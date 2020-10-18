{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
-- TODO1 added by Example Plugin directly
-- TODO1 added by Example Plugin directly
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.RecordSnippets
  (
    descriptor
  ) where

--import Control.DeepSeq ( NFData )
import Control.Monad.Trans.Maybe
import Data.Aeson
--import Data.Binary
--import Data.Functor
import qualified Data.HashMap.Strict as Map
--import Data.Hashable
import qualified Data.Text as T
--import Data.Typeable
import Development.IDE as D
--import Development.IDE.GHC.Compat (parsedSource, ParsedModule(ParsedModule))
import Development.IDE.Core.Rules (useE)
import Development.IDE.Core.Shake (getDiagnostics, getHiddenDiagnostics)
import GHC.Generics
import Ide.Plugin
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()
import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS
import Development.IDE.GHC.Compat
import Data.Maybe (catMaybes)
import GhcPlugins (occNameString, rdrNameOcc)
import Language.Haskell.GHC.ExactPrint.Utils (showGhc)
import Data.List (intercalate)
--import Development.IDE.Plugin.Completions

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginCompletionProvider = Just getCompletionsLSP
  }

-- ---------------------------------------------------------------------

_hover :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))
_hover = request "Hover" _blah (Right Nothing) foundHover

_blah :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
_blah _ (Position line col)
  = return $ Just (Just (Range (Position line col) (Position (line+1) 0)), ["example hover 1\n"])

-- ---------------------------------------------------------------------
-- Generating Diagnostics via rules
-- ---------------------------------------------------------------------

-- data Example = Example
--     deriving (Eq, Show, Typeable, Generic)
-- instance Hashable Example
-- instance NFData   Example
-- instance Binary   Example

-- type instance RuleResult Example = ()

-- exampleRules :: Rules ()
-- exampleRules = do
--   define $ \Example file -> do
--     _pm <- getParsedModule file
--     let diag = mkDiag file "example" DsError (Range (Position 0 0) (Position 1 0)) "example diagnostic, hello world"
--     return ([diag], Just ())

--   action $ do
--     files <- getFilesOfInterest
--     void $ uses Example $ Map.keys files

-- mkDiag :: NormalizedFilePath
--        -> DiagnosticSource
--        -> DiagnosticSeverity
--        -> Range
--        -> T.Text
--        -> FileDiagnostic
-- mkDiag file diagSource sev loc msg = (file, D.ShowDiag,)
--     Diagnostic
--     { _range    = loc
--     , _severity = Just sev
--     , _source   = Just diagSource
--     , _message  = msg
--     , _code     = Nothing
--     , _tags     = Nothing
--     , _relatedInformation = Nothing
--     }

-- ---------------------------------------------------------------------
-- code actions
-- ---------------------------------------------------------------------

-- | Generate code actions.
_codeAction :: CodeActionProvider
_codeAction _lf state _pid (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List _xs} = do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    Just (ParsedModule{},_) <- runIdeAction "example" (shakeExtras state) $ useWithStaleFast GetParsedModule nfp
    let
      title = "Add TODO Item 1"
      tedit = [TextEdit (Range (Position 2 0) (Position 2 0))
               "-- TODO1 added by Example Plugin directly\n"]
      edit  = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
    pure $ Right $ List
        [ CACodeAction $ CodeAction title (Just CodeActionQuickFix) (Just $ List []) (Just edit) Nothing ]

-- ---------------------------------------------------------------------

_codeLens :: CodeLensProvider
_codeLens _lf ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = do
    logInfo (ideLogger ideState) "Example.codeLens entered (ideLogger)" -- AZ
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> filePath) -> do
        _ <- runIdeAction "Example.codeLens" (shakeExtras ideState) $ runMaybeT $ useE TypeCheck filePath
        _diag <- getDiagnostics ideState
        _hDiag <- getHiddenDiagnostics ideState
        let
          title = "Add TODO Item via Code Lens"
          -- tedit = [TextEdit (Range (Position 3 0) (Position 3 0))
          --      "-- TODO added by Example Plugin via code lens action\n"]
          -- edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
          range = Range (Position 3 0) (Position 4 0)
        let cmdParams = AddTodoParams uri "do abc"
        cmd <- mkLspCommand plId "codelens.todo" title (Just [(toJSON cmdParams)])
        pure $ Right $ List [ CodeLens range (Just cmd) Nothing ]
      Nothing -> pure $ Right $ List []

-- ---------------------------------------------------------------------
-- | Parameters for the addTodo PluginCommand.
data AddTodoParams = AddTodoParams
  { file   :: Uri  -- ^ Uri of the file to add the pragma to
  , todoText :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

_addTodoCmd :: CommandFunction AddTodoParams
_addTodoCmd _lf _ide (AddTodoParams uri todoText) = do
  let
    pos = Position 3 0
    textEdits = List
      [TextEdit (Range pos pos)
                  ("-- TODO:" <> todoText <> "\n")
      ]
    res = WorkspaceEdit
      (Just $ Map.singleton uri textEdits)
      Nothing
  return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams res))

-- ---------------------------------------------------------------------

foundHover :: (Maybe Range, [T.Text]) -> Either ResponseError (Maybe Hover)
foundHover (mbRange, contents) =
  Right $ Just $ Hover (HoverContents $ MarkupContent MkMarkdown
                        $ T.intercalate sectionSeparator contents) mbRange


-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> Action (Maybe a))
  -> Either ResponseError b
  -> (a -> Either ResponseError b)
  -> IdeState
  -> TextDocumentPositionParams
  -> IO (Either ResponseError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos _) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b)
                  -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runAction "Example" ide $ getResults filePath pos

-- ---------------------------------------------------------------------

_symbols :: SymbolsProvider
_symbols _lf _ide (DocumentSymbolParams _doc _mt)
    = pure $ Right [r]
    where
        r = DocumentSymbol name detail kind deprecation range selR chList
        name = "Example_symbol_name"
        detail = Nothing
        kind = SkVariable
        deprecation = Nothing
        range = Range (Position 2 0) (Position 2 5)
        selR = range
        chList = Nothing

-- ---------------------------------------------------------------------

getCompletionsLSP
    :: LSP.LspFuncs cofd
    -> IdeState
    -> CompletionParams
    -> IO (Either ResponseError CompletionResponseResult)
getCompletionsLSP lsp ide
  CompletionParams{_textDocument=TextDocumentIdentifier uri
                  ,_position=position
                  ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
      (Just cnts, Just path) -> do
        let npath = toNormalizedFilePath' path
        pm <- runIdeAction "RecordsSnippets" (shakeExtras ide) $ do
            --opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
            --compls <- useWithStaleFast ProduceCompletions npath
            pm <- useWithStaleFast GetParsedModule npath
            --binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
            pure pm
        case pm of
          Just (parsedMod, _) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            case (pfix, completionContext) of
              (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."})
                -> return (Completions $ List [])
              (Just pfix', _) -> do
                  -- TODO pass the real capabilities here (or remove the logic for snippets)
                --let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                --Completions . List <$> getCompletions ideOpts cci' parsedMod bindMap pfix' fakeClientCapabilities (WithSnippets True)
                  logInfo (ideLogger ide) $ T.pack $ "**********************************"
                  logInfo (ideLogger ide) $ T.pack $ show completionContext
                  logInfo (ideLogger ide) $ T.pack $ show pfix'
                  findLocalCompletions ide parsedMod pfix'
              _ -> return (Completions $ List [])
          _ -> return (Completions $ List [])
      _ -> return (Completions $ List [])


findLocalCompletions :: IdeState -> ParsedModule -> VFS.PosPrefixInfo -> IO CompletionResponseResult
findLocalCompletions ide  _pmod pfix = do
    let _hsmodule = unLoc (parsedSource _pmod)
        hsDecls = hsmodDecls _hsmodule
        ctxStr = (T.unpack . VFS.prefixText $ pfix)
        completionData = findFields ctxStr (unLoc <$> hsDecls)
    x <- sampleCompletion ctxStr completionData
    logInfo (ideLogger ide) $ "*****Showing Completion Data\n"
    logInfo (ideLogger ide) $ T.pack $ show completionData
    return x


findFields :: String -> [HsDecl GhcPs] -> [(String, String)]
findFields  ctxStr decls = name_type
  where
    dataDefns = catMaybes $ findDataDefns <$> decls
    findDataDefns decl =
      case decl of
        TyClD _ (DataDecl{tcdDataDefn}) -> Just tcdDataDefn
        _ -> Nothing
    conDecls = concat [ unLoc <$> dd_cons dataDefn | dataDefn <- dataDefns]
    h98 = catMaybes $ findH98 <$> conDecls


    findH98 conDecl = case conDecl of
      ConDeclH98{..} -> Just (unLoc con_name, con_args)
      ConDeclGADT{} -> Nothing  -- TODO: Expand this out later
      _ -> Nothing

    conArgs = [snd x | x  <- h98, (occNameString . rdrNameOcc . fst $ x) == ctxStr]
    flds = concat . catMaybes $ getFlds <$> conArgs
    getFlds conArg = case conArg of
      RecCon rec -> Just $ unLoc <$> (unLoc rec)
      _ -> Nothing
    name_type = map (\x -> ((showGhc . fst $ x), (showGhc . snd $ x))) (catMaybes $ extract <$> flds)

    extract ConDeclField{..} = let
      fld_type = unLoc cd_fld_type
      fld_name = rdrNameFieldOcc $ unLoc . head $ cd_fld_names --TODO: Why is cd_fld_names a list?
        in
        Just (fld_name, fld_type)
    extract _ = Nothing


sampleCompletion :: String -> [(String, String)] -> IO CompletionResponseResult
sampleCompletion ctxStr completionData = do
  pure $ Completions $ List [r]
  where
    r =
      CompletionItem
        label
        kind
        tags
        detail
        documentation
        deprecated
        preselect
        sortText
        filterText
        insertText
        insertTextFormat
        textEdit
        additionalTextEdits
        commitCharacters
        command
        xd
    label = T.pack ctxStr
    kind = Just CiSnippet
    tags = List []
    detail = Nothing
    documentation = Nothing
    deprecated = Nothing
    preselect = Nothing
    sortText = Nothing
    filterText = Nothing
    insertText = Just $ buildSnippet
    insertTextFormat = Just Snippet
    textEdit = Nothing
    additionalTextEdits = Nothing
    commitCharacters = Nothing
    command = Nothing
    xd = Nothing

    t = zip completionData ([1..]::[Int])
    snippet = intercalate ", " $
        map (\(x, i) -> ((fst x) <> "=${" <> show i <> ":" <> (fst x) <> "}")) t
    buildSnippet = T.pack $ ctxStr <> " {" <> snippet <> "}"


-- completion :: CompletionProvider
-- completion _lf _ide (CompletionParams _doc _pos _mctxt _mt)
--     = pure $ Right $ Completions $ List [r]
--     where
--         r = CompletionItem label kind tags detail documentation deprecated preselect
--                            sortText filterText insertText insertTextFormat
--                            textEdit additionalTextEdits commitCharacters
--                            command xd
--         label = "Example completion"
--         kind = Nothing
--         tags = List []
--         detail = Nothing
--         documentation = Nothing
--         deprecated = Nothing
--         preselect = Nothing
--         sortText = Nothing
--         filterText = Nothing
--         insertText = Just "Record $fld1 $fld2 $fld4"
--         insertTextFormat = Nothing
--         textEdit = Nothing
--         additionalTextEdits = Nothing
--         commitCharacters = Nothing
--         command = Nothing
--         xd = Nothing

-- -- ---------------------------------------------------------------------
