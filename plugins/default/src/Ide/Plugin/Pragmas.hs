{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  (
      descriptor
  -- ,   commands -- TODO: get rid of this
  ) where

import           Control.Lens                    hiding (List)
import           Data.Aeson
import qualified Data.HashMap.Strict             as H
import qualified Data.Text                       as T
import           Development.IDE                 as D
import qualified GHC.Generics                    as Generics
import           Ide.Types
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J

import           Control.Monad                   (join)
import           Development.IDE.GHC.Compat
import qualified Language.Haskell.LSP.Core       as LSP
import qualified Language.Haskell.LSP.VFS        as VFS

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCodeActionProvider = Just codeActionProvider
  , pluginCompletionProvider = Just completion
  }

-- ---------------------------------------------------------------------

-- | Parameters for the addPragma PluginCommand.
data AddPragmaParams = AddPragmaParams
  { file   :: J.Uri  -- ^ Uri of the file to add the pragma to
  , pragma :: T.Text -- ^ Name of the Pragma to add
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

-- | Add a Pragma to the given URI at the top of the file.
-- Pragma is added to the first line of the Uri.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
mkPragmaEdit :: Uri -> T.Text -> WorkspaceEdit
mkPragmaEdit uri pragmaName = res where
    pos = J.Position 0 0
    textEdits = J.List
      [J.TextEdit (J.Range pos pos)
                  ("{-# LANGUAGE " <> pragmaName <> " #-}\n")
      ]
    res = J.WorkspaceEdit
      (Just $ H.singleton uri textEdits)
      Nothing

-- ---------------------------------------------------------------------
-- | Offer to add a missing Language Pragma to the top of a file.
-- Pragmas are defined by a curated list of known pragmas, see 'possiblePragmas'.
codeActionProvider :: CodeActionProvider IdeState
codeActionProvider _ state _plId docId _ (J.CodeActionContext (J.List diags) _monly) = do
    let mFile = docId ^. J.uri & uriToFilePath <&> toNormalizedFilePath'
    pm <- fmap join $ runAction "addPragma" state $ getParsedModule `traverse` mFile
    let dflags = ms_hspp_opts . pm_mod_summary <$> pm
    -- Filter diagnostics that are from ghcmod
        ghcDiags = filter (\d -> d ^. J.source == Just "typecheck") diags
    -- Get all potential Pragmas for all diagnostics.
        pragmas = concatMap (\d -> genPragma dflags (d ^. J.message)) ghcDiags
    cmds <- mapM mkCodeAction pragmas
    return $ Right $ List cmds
      where
        mkCodeAction pragmaName = do
          let
            codeAction = J.CACodeAction $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) (Just edit) Nothing
            title = "Add \"" <> pragmaName <> "\""
            edit = mkPragmaEdit (docId ^. J.uri) pragmaName
          return codeAction

        genPragma mDynflags target
          | Just dynFlags <- mDynflags,
            -- GHC does not export 'OnOff', so we have to view it as string
            disabled <- [ e | Just e <- T.stripPrefix "Off " . T.pack . prettyPrint <$> extensions dynFlags]
          = [ r | r <- findPragma target, r `notElem` disabled]
          | otherwise = []


-- ---------------------------------------------------------------------

-- | Find all Pragmas are an infix of the search term.
findPragma :: T.Text -> [T.Text]
findPragma str = concatMap check possiblePragmas
  where
    check p = [p | T.isInfixOf p str]

-- ---------------------------------------------------------------------

-- | Possible Pragma names.
-- See discussion at https://github.com/haskell/ghcide/pull/638
possiblePragmas :: [T.Text]
possiblePragmas = [name | FlagSpec{flagSpecName = T.pack -> name} <- xFlags, "Strict" /= name]

-- ---------------------------------------------------------------------

completion :: CompletionProvider IdeState
completion lspFuncs _ide complParams = do
    let (TextDocumentIdentifier uri) = complParams ^. J.textDocument
        position = complParams ^. J.position
    contents <- LSP.getVirtualFileFunc lspFuncs $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just _path) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            return $ result pfix
            where
                result (Just pfix)
                    | "{-# LANGUAGE" `T.isPrefixOf` VFS.fullLine pfix
                    = Completions $ List $ map buildCompletion possiblePragmas
                    | otherwise
                    = Completions $ List []
                result Nothing = Completions $ List []
                buildCompletion p =
                    CompletionItem
                      { _label = p,
                        _kind = Just CiKeyword,
                        _tags = List [],
                        _detail = Nothing,
                        _documentation = Nothing,
                        _deprecated = Nothing,
                        _preselect = Nothing,
                        _sortText = Nothing,
                        _filterText = Nothing,
                        _insertText = Nothing,
                        _insertTextFormat = Nothing,
                        _textEdit = Nothing,
                        _additionalTextEdits = Nothing,
                        _commitCharacters = Nothing,
                        _command = Nothing,
                        _xdata = Nothing
                      }
        _ -> return $ Completions $ List []
