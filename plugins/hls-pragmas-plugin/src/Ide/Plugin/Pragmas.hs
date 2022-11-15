{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  ( descriptor
  -- For testing
  , validPragmas
  ) where

import           Control.Lens                       hiding (List)
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import qualified Data.HashMap.Strict                as H
import           Data.List.Extra                    (nubOrdOn)
import           Data.Maybe                         (catMaybes)
import qualified Data.Text                          as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.Plugin.Completions (ghcideCompletionsPluginPriority)
import qualified Development.IDE.Spans.Pragmas      as Pragmas
import           Ide.Types
import qualified Language.LSP.Server                as LSP
import qualified Language.LSP.Types                 as J
import qualified Language.LSP.Types.Lens            as J
import qualified Language.LSP.VFS                   as VFS
import qualified Text.Fuzzy                         as Fuzzy

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler J.STextDocumentCodeAction codeActionProvider
                  <> mkPluginHandler J.STextDocumentCompletion completion
  , pluginPriority = ghcideCompletionsPluginPriority + 1
  }

-- ---------------------------------------------------------------------
-- | Title and pragma
type PragmaEdit = (T.Text, Pragma)

data Pragma = LangExt T.Text | OptGHC T.Text
  deriving (Show, Eq, Ord)

codeActionProvider :: PluginMethodHandler IdeState 'J.TextDocumentCodeAction
codeActionProvider state _plId (J.CodeActionParams _ _ docId _ (J.CodeActionContext (J.List diags) _monly))
  | let J.TextDocumentIdentifier{ _uri = uri } = docId
  , Just normalizedFilePath <- J.uriToNormalizedFilePath $ toNormalizedUri uri = do
      -- ghc session to get some dynflags even if module isn't parsed
      ghcSession <- liftIO $ runAction "Pragmas.GhcSession" state $ useWithStale GhcSession normalizedFilePath
      (_, fileContents) <- liftIO $ runAction "Pragmas.GetFileContents" state $ getFileContents normalizedFilePath
      parsedModule <- liftIO $ runAction "Pragmas.GetParsedModule" state $ getParsedModule normalizedFilePath
      let parsedModuleDynFlags = ms_hspp_opts . pm_mod_summary <$> parsedModule

      case ghcSession of
        Just (hscEnv -> hsc_dflags -> sessionDynFlags, _) ->
          let nextPragmaInfo = Pragmas.getNextPragmaInfo sessionDynFlags fileContents
              pedits = nubOrdOn snd . concat $ suggest parsedModuleDynFlags <$> diags
          in
            pure $ Right $ List $ pragmaEditToAction uri nextPragmaInfo <$> pedits
        Nothing -> pure $ Right $ List []
  | otherwise = pure $ Right $ List []


-- | Add a Pragma to the given URI at the top of the file.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
pragmaEditToAction :: Uri -> Pragmas.NextPragmaInfo -> PragmaEdit -> (J.Command J.|? J.CodeAction)
pragmaEditToAction uri Pragmas.NextPragmaInfo{ nextPragmaLine, lineSplitTextEdits } (title, p) =
  J.InR $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing Nothing (Just edit) Nothing Nothing
  where
    render (OptGHC x)  = "{-# OPTIONS_GHC -Wno-" <> x <> " #-}\n"
    render (LangExt x) = "{-# LANGUAGE " <> x <> " #-}\n"
    pragmaInsertPosition = Position (fromIntegral nextPragmaLine) 0
    pragmaInsertRange = Range pragmaInsertPosition pragmaInsertPosition
    -- workaround the fact that for some reason lsp-test applies text
    -- edits in reverse order than lsp (tried in both coc.nvim and vscode)
    textEdits =
      if | Just (Pragmas.LineSplitTextEdits insertTextEdit deleteTextEdit) <- lineSplitTextEdits
         , let J.TextEdit{ _range, _newText } = insertTextEdit ->
             [J.TextEdit _range (render p <> _newText), deleteTextEdit]
         | otherwise -> [J.TextEdit pragmaInsertRange (render p)]

    edit =
      J.WorkspaceEdit
        (Just $ H.singleton uri (J.List textEdits))
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
        catMaybes $ T.stripPrefix "Off " . printOutputable <$> extensions dynFlags
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
#if MIN_VERSION_ghc(9,2,0)
  , "GHC2021"
#endif
  ]

-- ---------------------------------------------------------------------
flags :: [T.Text]
flags = map (T.pack . stripLeading '-') $ flagsForCompletion False

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
                    | "{-# language" `T.isPrefixOf` line
                    = J.List $ map buildCompletion
                        (Fuzzy.simpleFilter (VFS.prefixText pfix) allPragmas)
                    | "{-# options_ghc" `T.isPrefixOf` line
                    = J.List $ map buildCompletion
                        (Fuzzy.simpleFilter (VFS.prefixText pfix) flags)
                    | "{-#" `T.isPrefixOf` line
                    = J.List $ [ mkPragmaCompl (a <> suffix) b c
                                | (a, b, c, w) <- validPragmas, w == NewLine ]
                    | otherwise
                    = J.List $ [ mkPragmaCompl (prefix <> a <> suffix) b c
                                | (a, b, c, _) <- validPragmas, Fuzzy.test word b]
                    where
                        line = T.toLower $ VFS.fullLine pfix
                        word = VFS.prefixText pfix
                        -- Not completely correct, may fail if more than one "{-#" exist
                        -- , we can ignore it since it rarely happen.
                        prefix
                            | "{-# "  `T.isInfixOf` line = ""
                            | "{-#"   `T.isInfixOf` line = " "
                            | otherwise                 = "{-# "
                        suffix
                            | " #-}" `T.isSuffixOf` line = ""
                            | "#-}"  `T.isSuffixOf` line = " "
                            | "-}"   `T.isSuffixOf` line = " #"
                            | "}"    `T.isSuffixOf` line = " #-"
                            | otherwise                 = " #-}"
                result Nothing = J.List []
        _ -> return $ J.List []

-----------------------------------------------------------------------

-- | Pragma where exist
data AppearWhere =
  NewLine
  -- ^Must be on a new line
  | CanInline
  -- ^Can appear in the line
  deriving (Show, Eq)

validPragmas :: [(T.Text, T.Text, T.Text, AppearWhere)]
validPragmas =
  [ ("LANGUAGE ${1:extension}"        , "LANGUAGE"         , "{-# LANGUAGE #-}"         ,   NewLine)
  , ("OPTIONS_GHC -${1:option}"       , "OPTIONS_GHC"      , "{-# OPTIONS_GHC #-}"      ,   NewLine)
  , ("INLINE ${1:function}"           , "INLINE"           , "{-# INLINE #-}"           ,   NewLine)
  , ("NOINLINE ${1:function}"         , "NOINLINE"         , "{-# NOINLINE #-}"         ,   NewLine)
  , ("INLINABLE ${1:function}"        , "INLINABLE"        , "{-# INLINABLE #-}"        ,   NewLine)
  , ("WARNING ${1:message}"           , "WARNING"          , "{-# WARNING #-}"          , CanInline)
  , ("DEPRECATED ${1:message}"        , "DEPRECATED"       , "{-# DEPRECATED  #-}"      , CanInline)
  , ("ANN ${1:annotation}"            , "ANN"              , "{-# ANN #-}"              ,   NewLine)
  , ("RULES"                          , "RULES"            , "{-# RULES #-}"            ,   NewLine)
  , ("SPECIALIZE ${1:function}"       , "SPECIALIZE"       , "{-# SPECIALIZE #-}"       ,   NewLine)
  , ("SPECIALIZE INLINE ${1:function}", "SPECIALIZE INLINE", "{-# SPECIALIZE INLINE #-}",   NewLine)
  , ("SPECIALISE ${1:function}"       , "SPECIALISE"       , "{-# SPECIALISE #-}"       ,   NewLine)
  , ("SPECIALISE INLINE ${1:function}", "SPECIALISE INLINE", "{-# SPECIALISE INLINE #-}",   NewLine)
  , ("MINIMAL ${1:functions}"         , "MINIMAL"          , "{-# MINIMAL #-}"          , CanInline)
  , ("UNPACK"                         , "UNPACK"           , "{-# UNPACK #-}"           , CanInline)
  , ("NOUNPACK"                       , "NOUNPACK"         , "{-# NOUNPACK #-}"         , CanInline)
  , ("COMPLETE ${1:function}"         , "COMPLETE"         , "{-# COMPLETE #-}"         ,   NewLine)
  , ("OVERLAPPING"                    , "OVERLAPPING"      , "{-# OVERLAPPING #-}"      , CanInline)
  , ("OVERLAPPABLE"                   , "OVERLAPPABLE"     , "{-# OVERLAPPABLE #-}"     , CanInline)
  , ("OVERLAPS"                       , "OVERLAPS"         , "{-# OVERLAPS #-}"         , CanInline)
  , ("INCOHERENT"                     , "INCOHERENT"       , "{-# INCOHERENT #-}"       , CanInline)
  ]

mkPragmaCompl :: T.Text -> T.Text -> T.Text -> J.CompletionItem
mkPragmaCompl insertText label detail =
  J.CompletionItem label (Just J.CiKeyword) Nothing (Just detail)
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just J.Snippet)
    Nothing Nothing Nothing Nothing Nothing Nothing


stripLeading :: Char -> String -> String
stripLeading _ [] = []
stripLeading c (s:ss)
  | s == c = ss
  | otherwise = s:ss


buildCompletion :: T.Text -> J.CompletionItem
buildCompletion label =
  J.CompletionItem label (Just J.CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing



