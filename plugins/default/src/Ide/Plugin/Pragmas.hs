{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas (descriptor) where

import           Control.Applicative        ((<|>))
import           Control.Lens               hiding (List)
import           Control.Monad              (join)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict        as H
import           Data.List.Extra            (nubOrdOn)
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Development.IDE            as D
import           Development.IDE.GHC.Compat
import           Ide.Types
import qualified Language.LSP.Server        as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types         as J
import qualified Language.LSP.Types.Lens    as J
import qualified Language.LSP.VFS           as VFS
import qualified Text.Fuzzy                 as Fuzzy

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
                  <> mkPluginHandler STextDocumentCompletion completion
  }

-- ---------------------------------------------------------------------

-- | Title and pragma
type PragmaEdit = (T.Text, Pragma)

data Pragma = LangExt T.Text | OptGHC T.Text
  deriving (Show, Eq, Ord)

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider state _plId (CodeActionParams _ _ docId _ (J.CodeActionContext (J.List diags) _monly)) = do
  let mFile = docId ^. J.uri & uriToFilePath <&> toNormalizedFilePath'
      uri = docId ^. J.uri
  pm <- liftIO $ fmap join $ runAction "Pragmas.GetParsedModule" state $ getParsedModule `traverse` mFile
  let dflags = ms_hspp_opts . pm_mod_summary <$> pm
      insertRange = maybe (Range (Position 0 0) (Position 0 0)) endOfModuleHeader pm
      pedits = nubOrdOn snd . concat $ suggest dflags <$> diags
  return $ Right $ List $ pragmaEditToAction uri insertRange <$> pedits

-- | Add a Pragma to the given URI at the top of the file.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
pragmaEditToAction :: Uri -> Range -> PragmaEdit -> (Command |? CodeAction)
pragmaEditToAction uri range (title, p) =
  InR $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing Nothing (Just edit) Nothing Nothing
  where
    render (OptGHC x)  = "{-# OPTIONS_GHC -Wno-" <> x <> " #-}\n"
    render (LangExt x) = "{-# LANGUAGE " <> x <> " #-}\n"
    textEdits = J.List [J.TextEdit range $ render p]
    edit =
      J.WorkspaceEdit
        (Just $ H.singleton uri textEdits)
        Nothing
        Nothing

suggest :: Maybe DynFlags -> Diagnostic -> [PragmaEdit]
suggest dflags diag =
  suggestAddPragma dflags diag
    ++ suggestDisableWarning diag

-- ---------------------------------------------------------------------

suggestDisableWarning :: Diagnostic -> [PragmaEdit]
suggestDisableWarning Diagnostic {_code}
  | Just (InR (T.stripPrefix "-W" -> Just w)) <- _code =
    pure ("Disable \"" <> w <> "\" warnings", OptGHC w)
  | otherwise = []

-- ---------------------------------------------------------------------

-- | Offer to add a missing Language Pragma to the top of a file.
-- Pragmas are defined by a curated list of known pragmas, see 'possiblePragmas'.
suggestAddPragma :: Maybe DynFlags -> Diagnostic -> [PragmaEdit]
suggestAddPragma mDynflags Diagnostic {_message} = genPragma _message
  where
    genPragma target =
      [("Add \"" <> r <> "\"", LangExt r) | r <- findPragma target, r `notElem` disabled]
    disabled
      | Just dynFlags <- mDynflags =
        -- GHC does not export 'OnOff', so we have to view it as string
        catMaybes $ T.stripPrefix "Off " . T.pack . prettyPrint <$> extensions dynFlags
      | otherwise =
        -- When the module failed to parse, we don't have access to its
        -- dynFlags. In that case, simply don't disable any pragmas.
        []

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
                        _insertTextMode = Nothing,
                        _textEdit = Nothing,
                        _additionalTextEdits = Nothing,
                        _commitCharacters = Nothing,
                        _command = Nothing,
                        _xdata = Nothing
                      }
        _ -> return $ List []

-- ---------------------------------------------------------------------

-- | Find the first non-blank line before the first of (module name / imports / declarations).
-- Useful for inserting pragmas.
endOfModuleHeader :: ParsedModule -> Range
endOfModuleHeader pm =
  let mod = unLoc $ pm_parsed_source pm
      modNameLoc = getLoc <$> hsmodName mod
      firstImportLoc = getLoc <$> listToMaybe (hsmodImports mod)
      firstDeclLoc = getLoc <$> listToMaybe (hsmodDecls mod)
      line = maybe 0 (_line . _start) (modNameLoc <|> firstImportLoc <|> firstDeclLoc >>= srcSpanToRange)
      loc = Position line 0
   in Range loc loc
