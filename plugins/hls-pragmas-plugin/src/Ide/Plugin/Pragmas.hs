{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
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
import           Data.Bits                        (Bits (bit, complement, (.&.)))
import           Data.Char                        (isSpace)
import qualified Data.Char                        as Char
import           Data.Coerce                      (coerce)
import           Data.Functor                     (void, ($>))
import qualified Data.HashMap.Strict              as H
import           Data.List
import qualified Data.List                        as List
import           Data.List.Extra                  (nubOrdOn)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (catMaybes, listToMaybe,
                                                   mapMaybe)
import qualified Data.Maybe                       as Maybe
import           Data.Monoid                      (Endo (Endo, appEndo))
import           Data.Ord                         (Down (Down))
import qualified Data.Text                        as T
import           Data.Word                        (Word64)
import qualified Debug.Trace                      as Debug
import           Development.IDE                  as D
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util  (StringBuffer, atEnd,
                                                   nextChar,
                                                   stringToStringBuffer)
import           Development.IDE.Types.HscEnvEq   (HscEnvEq, hscEnv)
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
codeActionProvider state _plId (J.CodeActionParams _ _ docId _ (J.CodeActionContext (J.List diags) _monly))
  | let J.TextDocumentIdentifier{ _uri = uri } = docId
  , Just normalizedFilePath <- J.uriToNormalizedFilePath $ toNormalizedUri uri = do
  let mFile = docId ^. J.uri & J.uriToFilePath <&> toNormalizedFilePath'
      uri = docId ^. J.uri
  ghcSession <- liftIO $ runAction "Pragmas.GhcSession" state $ useWithStale GhcSession normalizedFilePath
  (_, mbContents) <- liftIO $ runAction "Pragmas.GetFileContents" state $ getFileContents normalizedFilePath
  parsedModule <- liftIO $ runAction "Pragmas.GetParsedModule" state $ getParsedModule normalizedFilePath
  let dflags = ms_hspp_opts . pm_mod_summary <$> parsedModule
  case ghcSession of
    Just (hscEnv -> hsc_dflags -> dynFlags, _) ->
      let nextPragmaInfo = if | Debug.trace "before mbcontents" True
                              , Just sourceText <- mbContents
                              , Debug.trace "after mbcontents, before dflags" True
                              , Debug.trace "after dflags" True
                              , let sourceStringBuffer = stringToStringBuffer (T.unpack sourceText)
                              , POk _ parserState <- parsePreDecl dynFlags sourceStringBuffer
                              , let nextPragma = case parserState of
                                      ParserStateNotDone { nextPragma } -> nextPragma
                                      ParserStateDone { nextPragma } -> nextPragma
                              -> nextPragma
                              | otherwise
                              -> NextPragma 0 []
          pedits = nubOrdOn snd . concat $ suggest dflags <$> diags
      in do
        return $ Right $ List $ pragmaEditToAction uri nextPragmaInfo <$> Debug.trace (show nextPragmaInfo) pedits
    Nothing -> pure $ Right $ List []
  | otherwise
  = pure $ Right $ List []


-- | Add a Pragma to the given URI at the top of the file.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
pragmaEditToAction :: Uri -> NextPragma -> PragmaEdit -> (J.Command J.|? J.CodeAction)
pragmaEditToAction uri NextPragma{ nextPragmaLine, lineSplitTextEdits } (title, p) =
  J.InR $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing Nothing (Just edit) Nothing Nothing
  where
    render (OptGHC x)  = "{-# OPTIONS_GHC -Wno-" <> x <> " #-}\n"
    render (LangExt x) = "{-# LANGUAGE " <> x <> " #-}\n"
    pragmaInsertPosition = Position nextPragmaLine 0
    pragmaInsertRange = Range pragmaInsertPosition pragmaInsertPosition
    textEdits = lineSplitTextEdits ++ [J.TextEdit pragmaInsertRange $ render p]

    edit =
      J.WorkspaceEdit
        (Just $ H.singleton uri (J.List $ Debug.trace (show textEdits) textEdits))
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
    -- let (J.TextDocumentIdentifier uri) = complParams ^. J.textDocument
        -- position = complParams ^. J.position
    -- contents <- LSP.getVirtualFile $ toNormalizedUri uri
    fmap (Right . J.InL) $ case (undefined, uriToFilePath' undefined) of
        (Just cnts, Just _path) ->
            result <$> pure Nothing    --   VFS.getCompletionPrefix position cnts
            where
                result (Just pfix)
                    --  | "{-# language" `T.isPrefixOf` T.toLower (VFS.fullLine pfix)
                    --  = J.List $ map buildCompletion
                    --      (Fuzzy.simpleFilter (VFS.prefixText pfix) allPragmas)
                    --  | "{-# options_ghc" `T.isPrefixOf` T.toLower (VFS.fullLine pfix)
                    --  = J.List $ map mkExtCompl
                    --      (Fuzzy.simpleFilter (VFS.prefixText pfix) flags)
                    --  -- if there already is a closing bracket - complete without one
                    --  | isPragmaPrefix (VFS.fullLine pfix) && "}" `T.isSuffixOf` VFS.fullLine pfix
                    --  = J.List $ map (\(a, b, c) -> mkPragmaCompl a b c) (validPragmas Nothing)
                    --  -- if there is no closing bracket - complete with one
                    --  | isPragmaPrefix (VFS.fullLine pfix)
                    --  = J.List $ map (\(a, b, c) -> mkPragmaCompl a b c) (validPragmas (Just "}"))
                    --  | otherwise
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
type ExtsBitmap = Word64

xbit :: ExtBits -> ExtsBitmap
xbit = bit . fromEnum

data Mode = ModePragma | ModeHaddock | ModeComment | ModeInitial deriving Show

data NextPragma = NextPragma {
  nextPragmaLine     :: Int,
  lineSplitTextEdits :: [J.TextEdit]
} deriving Show

data ParserState
  = ParserStateNotDone
    { nextPragma           :: !NextPragma
    , mode                 :: !Mode
    , lastBlockCommentLine :: !Int
    , lastPragmaLine       :: !Int
    , isLastTokenHash      :: !Bool
    }
  | ParserStateDone { nextPragma :: NextPragma }
  deriving Show

isPragma :: String -> Bool
isPragma = List.isPrefixOf "{-#"

isDownwardBlockHaddock :: String -> Bool
isDownwardBlockHaddock = List.isPrefixOf "{-|"

isDownwardLineHaddock :: String -> Bool
isDownwardLineHaddock = List.isPrefixOf "-- |"

isLineComment :: String -> Bool
isLineComment = List.isPrefixOf "--"

-- LSP spec describes the horizontal part of a Range as (paraphrasing)
-- "0-based positions between characters"
srcSpanToRange :: SrcSpan -> Maybe J.Range
srcSpanToRange srcSpan
  | RealSrcLoc startRealSrcLoc _ <- srcSpanStart srcSpan
  , RealSrcLoc endRealSrcLoc _ <- srcSpanEnd srcSpan
  , let startLine = srcLocLine startRealSrcLoc
  , let startCol = srcLocCol startRealSrcLoc
  , let endLine = srcLocLine endRealSrcLoc
  , let endCol = srcLocCol endRealSrcLoc
  , let startPosition = J.Position (startLine - 1) (startCol - 1)
  , let endPosition = J.Position (endLine - 1) endCol
  , let range = J.Range startPosition endPosition
  = Just range
  | otherwise
  = Nothing

getLineSplitTextEdits :: J.Range -> String -> [J.TextEdit]
getLineSplitTextEdits (J.Range startPosition@(J.Position startLine startCol) _) s
  --  | Debug.trace ("linesplit" ++ show startLine ++ s) False = undefined
  | Just newLineCol <- if isLineComment s then Just (length s) else List.elemIndex '\n' s
  = let
      isLine = isLineComment s
      deleteTextEdit = J.TextEdit (J.Range startPosition startPosition{ J._character = startCol + newLineCol }) ""
      insertPosition = J.Position (startLine + 1) 0
      insertRange = J.Range insertPosition insertPosition
      insertText = T.pack $ List.take newLineCol s
      insertTextEdit = J.TextEdit insertRange insertText
    in
      [deleteTextEdit, J.TextEdit insertRange "\n", insertTextEdit]
  | otherwise = []

-- ITvarsym "#" after a block comment is a parse error so we don't need to worry about it
updateParserState :: Token -> J.Range -> ParserState -> ParserState
updateParserState token range prevParserState
  -- don't reset lineSplitTextEdits, insertPragmaLine and lineSplitTextEdits should be an item
  | let defaultParserState = prevParserState{ isLastTokenHash = False }
  , let (J.Range startPosition@(J.Position startLine _) (J.Position endLine endCol)) = range
  -- , Debug.trace (show prevParserState) True
  = case prevParserState of
      ParserStateNotDone{
        nextPragma = prevNextPragma,
        mode = prevMode,
        lastBlockCommentLine,
        lastPragmaLine } ->
        case prevMode of
          ModeInitial ->
            case token of
              ITvarsym "#" -> defaultParserState{ isLastTokenHash = True }
              ITlineComment s
                --  | Debug.trace ("linecomment: " ++ s ++ show endLine) False -> undefined
                | isDownwardLineHaddock s -> defaultParserState{ mode = ModeHaddock }
                | otherwise ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [],
                      mode = ModeComment }
              ITblockComment s
                | isPragma s ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [] ,
                      mode = ModePragma,
                      lastPragmaLine = endLine }
                | isDownwardBlockHaddock s -> defaultParserState{ mode = ModeHaddock }
                | otherwise ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [],
                      mode = ModeComment,
                      lastBlockCommentLine = endLine }
              _ -> ParserStateDone prevNextPragma
          ModeComment ->
            case token of
              ITvarsym "#" -> defaultParserState{ isLastTokenHash = True}
              ITlineComment s
                | isDownwardLineHaddock s
                , let lineSplitTextEdits = if lastBlockCommentLine == startLine then getLineSplitTextEdits range s else [] ->
                    defaultParserState{
                      nextPragma = prevNextPragma{ lineSplitTextEdits = lineSplitTextEdits },
                      mode = ModeHaddock }
                | otherwise -> defaultParserState{ nextPragma = NextPragma (endLine + 1) [] }
              ITblockComment s
                --  | Debug.trace (show $ isAfterBlockComment) False -> undefined
                | isPragma s ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [],
                      mode = ModePragma,
                      lastPragmaLine = endLine }
                | isDownwardBlockHaddock s
                , let lineSplitTextEdits = if lastBlockCommentLine == startLine then getLineSplitTextEdits range s else [] ->
                    defaultParserState{
                      nextPragma = prevNextPragma{ lineSplitTextEdits = lineSplitTextEdits },
                      mode = ModeHaddock }
                | otherwise ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [],
                      lastBlockCommentLine = endLine }
              _ -> ParserStateDone prevNextPragma
          ModeHaddock ->
            case token of
              ITvarsym "#" -> defaultParserState{ isLastTokenHash = True }
              ITlineComment _ -> defaultParserState
              ITblockComment s
                | isPragma s ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [],
                      mode = ModePragma,
                      lastPragmaLine = endLine }
                | otherwise -> defaultParserState{ lastBlockCommentLine = endLine }
              _ -> ParserStateDone prevNextPragma
          ModePragma ->
            case token of
              ITvarsym "#" -> defaultParserState{ isLastTokenHash = True }
              ITlineComment s
                --  | Debug.trace ("pragma - linecomment - " ++ s ++ show (getLineSplitTextEdits range s) ++ show endLine) False -> undefined
                | isDownwardLineHaddock s
                , lastPragmaLine == startLine ->
                    defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = getLineSplitTextEdits range s } }
                | otherwise -> defaultParserState
              ITblockComment s
                | isPragma s ->
                    defaultParserState{
                      nextPragma = NextPragma (endLine + 1) [],
                      lastPragmaLine = endLine }
                | lastPragmaLine == startLine ->
                    defaultParserState{ nextPragma = prevNextPragma{ lineSplitTextEdits = getLineSplitTextEdits range s} }
                | otherwise -> defaultParserState{ lastBlockCommentLine = endLine }
              _ -> ParserStateDone prevNextPragma
      ParserStateDone _ -> defaultParserState

lexUntilNextLineIncl :: P (Located Token)
lexUntilNextLineIncl = do
  PState{ last_loc } <- getPState
  let prevEndLine = last_loc & realSrcSpanEnd & srcLocLine
  locatedToken@(L srcSpan token) <- lexer False pure
  if | RealSrcLoc currEndRealSrcLoc _ <- srcSpan & srcSpanEnd
     , let currEndLine = currEndRealSrcLoc & srcLocLine
     -> if prevEndLine < currEndLine then
          pure locatedToken
        else lexUntilNextLineIncl
     | otherwise -> pure locatedToken

dropWhileStringBuffer :: (Char -> Bool) -> StringBuffer -> StringBuffer
dropWhileStringBuffer predicate buffer
  | atEnd buffer = buffer
  | let (c, remainingBuffer) = nextChar buffer
  -- , Debug.trace ("dropwhile" ++ show c) True
  = if predicate c then
      dropWhileStringBuffer predicate remainingBuffer
    else
      buffer

isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'

data ShebangParserState = ShebangParserState {
  nextPragmaLine :: !Int,
  newlineCount   :: !Int,
  prevCharIsHash :: !Bool,
  buffer         :: !StringBuffer
}

-- lexer seems to ignore shebangs completely hence this function
parseShebangs :: ShebangParserState -> ShebangParserState
parseShebangs prev@ShebangParserState{ nextPragmaLine, newlineCount = prevNewlineCount, prevCharIsHash, buffer = prevBuffer }
  | atEnd prevBuffer
  -- , Debug.trace "end" True
  = prev
  | let (c, currBuffer) = nextChar (dropWhileStringBuffer isHorizontalSpace prevBuffer)
  -- , Debug.trace (show c ++ show nextPragmaLine ++ show prevNewlineCount) True
  = if c == '#' then
      parseShebangs prev{ prevCharIsHash = True, buffer = currBuffer }
    else if c == '!' && prevCharIsHash then
      parseShebangs prev{ nextPragmaLine = prevNewlineCount + 1, buffer = dropWhileStringBuffer (/= '\n') currBuffer }
    else if c == '\n' then
      parseShebangs prev{ newlineCount = prevNewlineCount + 1, buffer = currBuffer }
    else
      prev

parsePreDecl :: DynFlags -> StringBuffer -> ParseResult ParserState
parsePreDecl dynFlags buffer = unP (go initialParserState) pState{ options = options }
  where
    initialShebangParserState = ShebangParserState{
      nextPragmaLine = 0,
      newlineCount = 0,
      prevCharIsHash = False,
      buffer = buffer }
    ShebangParserState{ nextPragmaLine } = parseShebangs initialShebangParserState
    start = mkRealSrcLoc "asdf" 1 1
    dynFlagsWithoutHaddockWithRawTokenStream = gopt_set (gopt_unset dynFlags Opt_Haddock) Opt_KeepRawTokenStream
    pState@PState{ options = pStateOptions } = mkPState dynFlagsWithoutHaddockWithRawTokenStream buffer start
    options = pStateOptions{ pExtsBitmap = complement (xbit UsePosPragsBit) .&. pExtsBitmap pStateOptions }
    initialParserState = ParserStateNotDone (NextPragma nextPragmaLine []) ModeInitial (-1) (-1) False

    go :: ParserState -> P ParserState
    go prevParserState =
      case prevParserState of
        ParserStateDone _ -> pure prevParserState
        ParserStateNotDone{..} -> do
          L srcSpan token <-
            if isLastTokenHash then
              lexUntilNextLineIncl
            else
              lexer False pure
          case D.srcSpanToRange srcSpan of
            Just range -> go (updateParserState token range prevParserState)
            Nothing    -> pure prevParserState

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
-- getNextPragmaInsertLine :: T.Text -> Int
-- getNextPragmaInsertLine text =
--   let
--     result = Atto.parseOnly preDeclP text & fmap ($ ParserState 0 0 InsertModeInitial LineTypeBlank False False)
--   in
--     case result of
--       Left _                                -> 0
--       Right ParserState{ pragmaInsertLine } -> pragmaInsertLine




