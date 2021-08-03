{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  ( descriptor
  ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               hiding (List)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Char                  (isSpace)
import qualified Data.HashMap.Strict        as H
import           Data.List
import           Data.List.Extra            (nubOrdOn)
import           Data.Maybe                 (catMaybes, listToMaybe)
import qualified Data.Text                  as T
import           Development.IDE            as D
import           Development.IDE.GHC.Compat
import           Ide.Types
import qualified Language.LSP.Server        as LSP
import qualified Language.LSP.Types         as J
import qualified Language.LSP.Types.Lens    as J
import qualified Language.LSP.VFS           as VFS
import qualified Text.Fuzzy                 as Fuzzy

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler J.STextDocumentCodeAction codeActionProvider
                  <> mkPluginHandler J.STextDocumentCompletion completion
  }

-- ---------------------------------------------------------------------

-- | Title and pragma
type PragmaEdit = (T.Text, Pragma)

data Pragma = LangExt T.Text | OptGHC T.Text
  deriving (Show, Eq, Ord)

codeActionProvider :: PluginMethodHandler IdeState 'J.TextDocumentCodeAction
codeActionProvider state _plId (J.CodeActionParams _ _ docId _ (J.CodeActionContext (J.List diags) _monly)) = do
  let mFile = docId ^. J.uri & J.uriToFilePath <&> toNormalizedFilePath'
      uri = docId ^. J.uri
  pm <- liftIO $ fmap join $ runAction "Pragmas.GetParsedModule" state $ getParsedModule `traverse` mFile
  mbContents <- liftIO $ fmap (snd =<<) $ runAction "Pragmas.GetFileContents" state $ getFileContents `traverse` mFile
  let dflags = ms_hspp_opts . pm_mod_summary <$> pm
      insertRange = maybe (Range (Position 0 0) (Position 0 0)) findNextPragmaPosition mbContents
      pedits = nubOrdOn snd . concat $ suggest dflags <$> diags
  return $ Right $ List $ pragmaEditToAction uri insertRange <$> pedits

-- | Add a Pragma to the given URI at the top of the file.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
pragmaEditToAction :: Uri -> Range -> PragmaEdit -> (J.Command J.|? J.CodeAction)
pragmaEditToAction uri range (title, p) =
  J.InR $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing Nothing (Just edit) Nothing Nothing
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
  | Just (J.InR (T.stripPrefix "-W" -> Just w)) <- _code
  , w `notElem` warningBlacklist =
    pure ("Disable \"" <> w <> "\" warnings", OptGHC w)
  | otherwise = []

-- Don't suggest disabling type errors as a solution to all type errors
warningBlacklist :: [T.Text]
-- warningBlacklist = []
warningBlacklist = ["deferred-type-errors"]

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

completion :: PluginMethodHandler IdeState 'J.TextDocumentCompletion
completion _ide _ complParams = do
    let (J.TextDocumentIdentifier uri) = complParams ^. J.textDocument
        position = complParams ^. J.position
    contents <- LSP.getVirtualFile $ toNormalizedUri uri
    fmap (Right . J.InL) $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just _path) ->
            result <$> VFS.getCompletionPrefix position cnts
            where
                result (Just pfix)
                    | "{-# LANGUAGE" `T.isPrefixOf` VFS.fullLine pfix
                    = J.List $ map buildCompletion
                        (Fuzzy.simpleFilter (VFS.prefixText pfix) allPragmas)
                    | otherwise
                    = J.List []
                result Nothing = J.List []
                buildCompletion p =
                    J.CompletionItem
                      { _label = p,
                        _kind = Just J.CiKeyword,
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
        _ -> return $ J.List []

-----------------------------------------------------------------------

-- | Find first line after the last file header pragma
-- Defaults to line 0 if the file contains no shebang(s), OPTIONS_GHC pragma(s), or LANGUAGE pragma(s)
-- Otherwise it will be one after the count of line numbers, checking in order: Shebangs -> OPTIONS_GHC -> LANGUAGE
-- Taking the max of these to account for the possibility of interchanging order of these three Pragma types
findNextPragmaPosition :: T.Text -> Range
findNextPragmaPosition contents = Range loc loc
  where
    loc = Position line 0
    line = afterLangPragma . afterOptsGhc $ afterShebang
    afterLangPragma = afterPragma "LANGUAGE" contents'
    afterOptsGhc = afterPragma "OPTIONS_GHC" contents'
    afterShebang = lastLineWithPrefix (T.isPrefixOf "#!") contents' 0
    contents' = T.lines contents

afterPragma :: T.Text -> [T.Text] -> Int -> Int
afterPragma name contents lineNum = lastLineWithPrefix (checkPragma name) contents lineNum

lastLineWithPrefix :: (T.Text -> Bool) -> [T.Text] -> Int -> Int
lastLineWithPrefix p contents lineNum = max lineNum next
  where
    next = maybe lineNum succ $ listToMaybe . reverse $ findIndices p contents

checkPragma :: T.Text -> T.Text -> Bool
checkPragma name = check
  where
    check l = isPragma l && getName l == name
    getName l = T.take (T.length name) $ T.dropWhile isSpace $ T.drop 3 l
    isPragma = T.isPrefixOf "{-#"
