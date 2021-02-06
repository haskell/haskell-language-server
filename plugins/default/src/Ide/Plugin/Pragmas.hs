{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DataKinds             #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  (
      descriptor
  ) where

import           Control.Lens                    hiding (List)
import qualified Data.HashMap.Strict             as H
import qualified Data.Text                       as T
import           Development.IDE                 as D
import           Ide.Types
import           Language.LSP.Types
import qualified Language.LSP.Types      as J
import qualified Language.LSP.Types.Lens as J

import           Control.Monad                   (join)
import           Development.IDE.GHC.Compat
import qualified Language.LSP.Server       as LSP
import qualified Language.LSP.VFS as VFS
import qualified Text.Fuzzy as Fuzzy
import Data.List.Extra (nubOrd)
import Control.Monad.IO.Class

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
                  <> mkPluginHandler STextDocumentCompletion completion
  }

-- ---------------------------------------------------------------------

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
codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider state _plId (CodeActionParams _ _ docId _ (J.CodeActionContext (J.List diags) _monly)) = liftIO $ do
    let mFile = docId ^. J.uri & uriToFilePath <&> toNormalizedFilePath'
    pm <- fmap join $ runAction "addPragma" state $ getParsedModule `traverse` mFile
    let dflags = ms_hspp_opts . pm_mod_summary <$> pm
        -- Get all potential Pragmas for all diagnostics.
        pragmas = nubOrd $ concatMap (\d -> genPragma dflags (d ^. J.message)) diags
    cmds <- mapM mkCodeAction pragmas
    return $ Right $ List cmds
      where
        mkCodeAction pragmaName = do
          let
            codeAction = InR $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing Nothing (Just edit) Nothing
            title = "Add \"" <> pragmaName <> "\""
            edit = mkPragmaEdit (docId ^. J.uri) pragmaName
          return codeAction

        genPragma mDynflags target =
            [ r | r <- findPragma target, r `notElem` disabled]
          where
            disabled
              | Just dynFlags <- mDynflags
                -- GHC does not export 'OnOff', so we have to view it as string
              = [ e | Just e <- T.stripPrefix "Off " . T.pack . prettyPrint <$> extensions dynFlags]
              | otherwise
                -- When the module failed to parse, we don't have access to its
                -- dynFlags. In that case, simply don't disable any pragmas.
              = []

-- ---------------------------------------------------------------------

-- | Find all Pragmas are an infix of the search term.
findPragma :: T.Text -> [T.Text]
findPragma str = concatMap check possiblePragmas
  where
    check p = [p | T.isInfixOf p str]

    -- We exclude the Strict extension as it causes many false positives, see
    -- the discussion at https://github.com/haskell/ghcide/pull/638
    --
    -- We don't include the No- variants, as GHC never suggests disabling an
    -- extension in an error message.
    possiblePragmas :: [T.Text]
    possiblePragmas =
       [ name
       | FlagSpec{flagSpecName = T.pack -> name} <- xFlags
       , "Strict" /= name
       ]

-- ---------------------------------------------------------------------

-- | All language pragmas, including the No- variants
allPragmas :: [T.Text]
allPragmas =
  concat
    [ [name, "No" <> name]
    | FlagSpec{flagSpecName = T.pack -> name} <- xFlags
    ]
  <>
  -- These pragmas are not part of xFlags as they are not reversable
  -- by prepending "No".
  [ -- Safe Haskell
    "Unsafe"
  , "Trustworthy"
  , "Safe"

    -- Language Version Extensions
  , "Haskell98"
  , "Haskell2010"
    -- Maybe, GHC 2021 after its release?
  ]

-- ---------------------------------------------------------------------

completion :: PluginMethodHandler IdeState TextDocumentCompletion
completion _ide _ complParams = do
    let (TextDocumentIdentifier uri) = complParams ^. J.textDocument
        position = complParams ^. J.position
    contents <- LSP.getVirtualFile $ toNormalizedUri uri
    fmap (Right . InL) $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just _path) ->
            result <$> VFS.getCompletionPrefix position cnts
            where
                result (Just pfix)
                    | "{-# LANGUAGE" `T.isPrefixOf` VFS.fullLine pfix
                    = List $ map buildCompletion
                        (Fuzzy.simpleFilter (VFS.prefixText pfix) allPragmas)
                    | otherwise
                    = List []
                result Nothing = List []
                buildCompletion p =
                    CompletionItem
                      { _label = p,
                        _kind = Just CiKeyword,
                        _tags = Nothing,
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
        _ -> return $ List []
