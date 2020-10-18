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

import qualified Data.Text as T
import Development.IDE as D
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
    x <- buildCompletion ctxStr completionData
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


buildCompletion :: String -> [(String, String)] -> IO CompletionResponseResult
buildCompletion ctxStr completionData = do
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
