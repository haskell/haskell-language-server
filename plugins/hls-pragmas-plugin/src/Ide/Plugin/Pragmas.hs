{-# LANGUAGE CPP                   #-}
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

import           Control.Applicative              ((<|>))
import           Control.Lens                     hiding (List)
import           Control.Monad                    (join)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.State.Strict (State)
import qualified Data.Attoparsec.Text             as Atto
import           Data.Char                        (isSpace)
import qualified Data.Char                        as Char
import           Data.Coerce                      (coerce)
import           Data.Functor                     (void, ($>))
import qualified Data.HashMap.Strict              as H
import           Data.List
import           Data.List.Extra                  (nubOrdOn)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, listToMaybe,
                                                   mapMaybe)
import qualified Data.Maybe                       as Maybe
import           Data.Monoid                      (Endo (Endo, appEndo))
import           Data.Ord                         (Down (Down))
import qualified Data.Text                        as T
import           Development.IDE                  as D
import           Development.IDE.GHC.Compat
import           Ide.Types
import qualified Language.LSP.Server              as LSP
import qualified Language.LSP.Types               as J
import qualified Language.LSP.Types.Lens          as J
import qualified Language.LSP.VFS                 as VFS
import qualified Text.Fuzzy                       as Fuzzy

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
      insertLine = maybe 0 getNextPragmaInsertLine mbContents
      insertRange = Range (Position insertLine 0) (Position insertLine 0)
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
                    | "{-# language" `T.isPrefixOf` T.toLower (VFS.fullLine pfix)
                    = J.List $ map buildCompletion
                        (Fuzzy.simpleFilter (VFS.prefixText pfix) allPragmas)
                    | "{-# options_ghc" `T.isPrefixOf` T.toLower (VFS.fullLine pfix)
                    = J.List $ map mkExtCompl
                        (Fuzzy.simpleFilter (VFS.prefixText pfix) flags)
                    -- if there already is a closing bracket - complete without one
                    | isPragmaPrefix (VFS.fullLine pfix) && "}" `T.isSuffixOf` VFS.fullLine pfix
                    = J.List $ map (\(a, b, c) -> mkPragmaCompl a b c) (validPragmas Nothing)
                    -- if there is no closing bracket - complete with one
                    | isPragmaPrefix (VFS.fullLine pfix)
                    = J.List $ map (\(a, b, c) -> mkPragmaCompl a b c) (validPragmas (Just "}"))
                    | otherwise
                    = J.List []
                result Nothing = J.List []
                isPragmaPrefix line = "{-#" `T.isPrefixOf` line
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
validPragmas :: Maybe T.Text -> [(T.Text, T.Text, T.Text)]
validPragmas mSuffix =
  [ ("LANGUAGE ${1:extension} #-" <> suffix         , "LANGUAGE",           "{-# LANGUAGE #-}")
  , ("OPTIONS_GHC -${1:option} #-" <> suffix        , "OPTIONS_GHC",        "{-# OPTIONS_GHC #-}")
  , ("INLINE ${1:function} #-" <> suffix            , "INLINE",             "{-# INLINE #-}")
  , ("NOINLINE ${1:function} #-" <> suffix          , "NOINLINE",           "{-# NOINLINE #-}")
  , ("INLINABLE ${1:function} #-"<> suffix          , "INLINABLE",          "{-# INLINABLE #-}")
  , ("WARNING ${1:message} #-" <> suffix            , "WARNING",            "{-# WARNING #-}")
  , ("DEPRECATED ${1:message} #-" <> suffix         , "DEPRECATED",         "{-# DEPRECATED  #-}")
  , ("ANN ${1:annotation} #-" <> suffix             , "ANN",                "{-# ANN #-}")
  , ("RULES #-" <> suffix                           , "RULES",              "{-# RULES #-}")
  , ("SPECIALIZE ${1:function} #-" <> suffix        , "SPECIALIZE",         "{-# SPECIALIZE #-}")
  , ("SPECIALIZE INLINE ${1:function} #-"<> suffix  , "SPECIALIZE INLINE",  "{-# SPECIALIZE INLINE #-}")
  ]
  where suffix = case mSuffix of
                  (Just s) -> s
                  Nothing  -> ""


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


mkExtCompl :: T.Text -> J.CompletionItem
mkExtCompl label =
  J.CompletionItem label (Just J.CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing

-- Parser
data LineType = LineTypeBlank | LineTypeComment | LineTypePragma | LineTypeHash | LineTypeShebang deriving Show

data InsertMode = InsertModeInitial | InsertModeComment | InsertModeHaddock | InsertModePragma deriving Show

data ParserState = ParserState {
  pragmaInsertLine    :: !Int,
  lineCount           :: !Int,
  insertMode          :: !InsertMode,
  lineType            :: !LineType,
  isAfterBlockComment :: !Bool,
  isAfterPragma       :: !Bool
} deriving Show

type ParserStateUpdater = ParserState -> ParserState

incrementLines :: Int -> ParserStateUpdater
incrementLines n state@ParserState{ lineCount = prevLineCount
                                  , insertMode
                                  , lineType
                                  , isAfterBlockComment
                                  , isAfterPragma } =
  let
    lineCount = prevLineCount + n
    pragmaInsertLine = lineCount
    stateWithIncrementedLineCount = state { lineCount = lineCount }
  in
    case insertMode of
      InsertModeInitial ->
        case lineType of
          LineTypeShebang -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine }
          LineTypeComment -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine, insertMode = InsertModeComment }
          LineTypePragma -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine, insertMode = InsertModePragma }
          _ -> stateWithIncrementedLineCount
      InsertModeComment ->
        case lineType of
          LineTypeBlank | isAfterBlockComment -> stateWithIncrementedLineCount { pragmaInsertLine = prevLineCount + 1 }
          LineTypeComment -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine }
          LineTypePragma -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine, insertMode = InsertModePragma }
          _ -> stateWithIncrementedLineCount
      InsertModeHaddock ->
        case lineType of
          LineTypePragma -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine, insertMode = InsertModePragma }
          _ -> stateWithIncrementedLineCount
      InsertModePragma ->
        case lineType of
          LineTypeBlank | isAfterPragma -> stateWithIncrementedLineCount { pragmaInsertLine = prevLineCount + 1 }
          LineTypeComment | isAfterPragma -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine }
          LineTypePragma -> stateWithIncrementedLineCount { pragmaInsertLine = pragmaInsertLine }
          _ -> stateWithIncrementedLineCount

incrementLine :: ParserStateUpdater
incrementLine = incrementLines 1

setLineType :: LineType -> ParserStateUpdater
setLineType lineType1 state = state { lineType = lineType1 }

updateInsertMode :: InsertMode -> ParserStateUpdater
updateInsertMode insertMode state@ParserState{ insertMode = prevInsertMode } =
  case prevInsertMode of
    InsertModeInitial -> state { insertMode = insertMode }
    InsertModeComment ->
      case insertMode of
        InsertModeHaddock -> state { insertMode = insertMode }
        InsertModePragma  -> state { insertMode = insertMode }
        _                 -> state
    InsertModeHaddock ->
      case insertMode of
        InsertModePragma -> state {insertMode = insertMode}
        _                -> state
    InsertModePragma -> state

setIsAfterBlockComment :: Bool -> ParserStateUpdater
setIsAfterBlockComment isAfterBlockComment state =
  state { isAfterBlockComment = isAfterBlockComment }

setIsAfterPragma :: Bool -> ParserStateUpdater
setIsAfterPragma isAfterPragma state =
  state { isAfterPragma = isAfterPragma }

incrementLineThenSetIsAfterBlock :: Bool -> ParserStateUpdater
incrementLineThenSetIsAfterBlock isAfterBlock =
  setIsAfterPragma isAfterBlock . setIsAfterBlockComment isAfterBlock . incrementLine

spacesP :: Atto.Parser ParserStateUpdater
spacesP = Atto.takeWhile Atto.isHorizontalSpace $> id

shebangLineP :: Atto.Parser ParserStateUpdater
shebangLineP =
  spacesP
  *> Atto.string "#!"
  *> Atto.takeTill Atto.isEndOfLine
  *> Atto.endOfLine
  $> (incrementLineThenSetIsAfterBlock False . setLineType LineTypeShebang)

blankLineP :: Atto.Parser ParserStateUpdater
blankLineP =
  spacesP
  *> Atto.endOfLine
  $> (incrementLineThenSetIsAfterBlock False . setLineType LineTypeBlank)

hashLineP :: Atto.Parser ParserStateUpdater
hashLineP =
  spacesP
  *> Atto.char '#'
  *> Atto.takeTill Atto.isEndOfLine
  *> Atto.endOfLine
  $> (incrementLineThenSetIsAfterBlock False . setLineType LineTypeHash)

lineCommentP :: Atto.Parser ParserStateUpdater
lineCommentP =
  spacesP
  *> Atto.string "--"
  *> Atto.takeTill Atto.isEndOfLine
  *> Atto.endOfLine
  $> (incrementLineThenSetIsAfterBlock False . setLineType LineTypeComment . updateInsertMode InsertModeComment)

lineHaddockP :: Atto.Parser ParserStateUpdater
lineHaddockP =
  spacesP
  *> Atto.string "-- |"
  *> Atto.takeTill Atto.isEndOfLine
  *> Atto.endOfLine
  $> (incrementLineThenSetIsAfterBlock False . setLineType LineTypeComment . updateInsertMode InsertModeHaddock)

tillBlockBreakpointP :: T.Text -> T.Text -> Atto.Parser ParserStateUpdater
tillBlockBreakpointP openText closeText
  | Just (openChar, _) <- T.uncons openText
  , Just (closeChar, _) <- T.uncons closeText =
      Atto.takeTill (\c -> c == openChar || c == closeChar)
      & fmap (incrementLines . T.length . T.filter Atto.isEndOfLine)
  | otherwise = fail "empty open or close breakpoint texts"

finishBlockP :: T.Text -> T.Text -> Atto.Parser ParserStateUpdater
finishBlockP openText closeText =
  flip (.)
  <$> tillBlockBreakpointP openText closeText
  <*> do
    endOrStartOrContinue <- Atto.eitherP (Atto.eitherP (void (Atto.string closeText)) (Atto.string openText)) (Atto.take 1)
    case endOrStartOrContinue of
      Left (Left _end) -> pure id
      Left (Right _start) -> flip (.) <$> finishBlockP openText closeText <*> finishBlockP openText closeText
      Right continue -> finishBlockP openText closeText & fmap (. incrementLines (T.length $ T.filter Atto.isEndOfLine continue))

blockP :: InsertMode -> LineType -> T.Text -> T.Text -> Atto.Parser ParserStateUpdater
blockP insertMode lineType openText closeText =
  spacesP
  *> ( flip (.)
       <$> (Atto.string openText $> (setLineType lineType . updateInsertMode insertMode ))
       <*> finishBlockP openText closeText )

blockCommentP :: Atto.Parser ParserStateUpdater
blockCommentP = blockP InsertModeComment LineTypeComment "{-" "-}" & fmap (setIsAfterBlockComment True .)

blockHaddockP :: Atto.Parser ParserStateUpdater
blockHaddockP = blockP InsertModeHaddock LineTypeComment "{-|" "-}" & fmap (setIsAfterBlockComment True .)

pragmaP :: Atto.Parser ParserStateUpdater
pragmaP = blockP InsertModePragma LineTypePragma "{-#" "#-}" & fmap (setIsAfterPragma True .)

preDeclP :: Atto.Parser ParserStateUpdater
preDeclP =
  fmap (appEndo . (mconcat . fmap Endo) . reverse)
       (Atto.many' ( blankLineP
                 <|> shebangLineP
                 <|> hashLineP
                 <|> lineHaddockP
                 <|> lineCommentP
                 <|> pragmaP
                 <|> blockHaddockP
                 <|> blockCommentP))

-- | Parses blank lines, comments, haddock comments ("-- |"), lines that start
-- with "#!", lines that start with "#", pragma lines. It should work with
-- multi-line comments and pragmas with things after them on the same line, as
-- well as nested forms of both.
-- When it doesn't find one of these things then it's assumed that we've found
-- a declaration, end-of-file, or a ghc parse error, and the parser stops.
--
-- While doing this, the parser keeps track of state in order to place the
-- next pragma line according to some rules:
-- - Ignore lines starting with '#' except for shebangs.
-- - If pragmas exist place after last pragma
-- - else if haddock comments exist:
--     - If comments exist place after last comment
--     - else if shebangs exist place after last shebang
--     - else place at first line
-- - else if comments exist place after last comment
-- - else if shebangs exist place after last shebang
-- - else place at first line
--
-- In particular this means that this is possible:
-- -- some comment
-- #! some shebang stuff
--
-- After insert:
--
-- -- some comment
-- {-# some pragma #-}
-- #! some shebang stuff
--
-- This should be fine because it makes no sense to have shebangs not at the
-- top of the file, but can be changed by modifying `insertLines` and/or maybe
-- changing an `InsertMode`.
getNextPragmaInsertLine :: T.Text -> Int
getNextPragmaInsertLine text =
  let
    result = Atto.parseOnly preDeclP text & fmap ($ ParserState 0 0 InsertModeInitial LineTypeBlank False False)
  in
    case result of
      Left _                                -> 0
      Right ParserState{ pragmaInsertLine } -> pragmaInsertLine




