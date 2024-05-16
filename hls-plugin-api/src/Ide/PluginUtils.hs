{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.PluginUtils
  ( -- * LSP Range manipulation functions
    normalize,
    extendNextLine,
    extendLineStart,
    extendToFullLines,
    WithDeletions(..),
    getProcessID,
    makeDiffTextEdit,
    makeDiffTextEditAdditive,
    diffText,
    diffText',
    pluginDescToIdePlugins,
    idePluginsToPluginDesc,
    getClientConfig,
    getPluginConfig,
    configForPlugin,
    handlesRequest,
    extractTextInRange,
    fullRange,
    mkLspCommand,
    mkLspCmdId,
    getPid,
    allLspCmdIds,
    allLspCmdIds',
    installSigUsr1Handler,
    subRange,
    positionInRange,
    usePropertyLsp,
    -- * Escape
    unescape,
  )
where

import           Control.Arrow               ((&&&))
import           Control.Lens                (_head, _last, re, (%~), (^.))
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.Char                   (isPrint, showLitChar)
import           Data.Functor                (void)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           Data.Void                   (Void)
import           Ide.Plugin.Config
import           Ide.Plugin.Properties
import           Ide.Types
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Char        as P
import qualified Text.Megaparsec.Char.Lexer  as P

-- ---------------------------------------------------------------------

-- | Extend to the line below and above to replace newline character.
--
-- >>> normalize (Range (Position 5 5) (Position 5 10))
-- Range (Position 5 0) (Position 6 0)
normalize :: Range -> Range
normalize = extendLineStart . extendNextLine

-- | Extend 'Range' to the start of the next line.
--
-- >>> extendNextLine (Range (Position 5 5) (Position 5 10))
-- Range (Position 5 5) (Position 6 0)
extendNextLine :: Range -> Range
extendNextLine (Range s (Position el _)) =
  Range s (Position (el + 1) 0)

-- | Extend 'Range' to the start of the current line.
--
-- >>> extendLineStart (Range (Position 5 5) (Position 5 10))
-- Range (Position 5 0) (Position 5 10)
extendLineStart :: Range -> Range
extendLineStart (Range (Position sl _) e) =
  Range (Position sl 0) e

-- | Extend 'Range' to include the start of the first line and start of the next line of the last line.
--
-- Caveat: It always extend the last line to the beginning of next line, even when the last position is at column 0.
-- This is to keep the compatibility with the implementation of old function @extractRange@.
--
-- >>> extendToFullLines (Range (Position 5 5) (Position 5 10))
-- Range (Position 5 0) (Position 6 0)
--
-- >>> extendToFullLines (Range (Position 5 5) (Position 7 2))
-- Range (Position 5 0) (Position 8 0)
--
-- >>> extendToFullLines (Range (Position 5 5) (Position 7 0))
-- Range (Position 5 0) (Position 8 0)
extendToFullLines :: Range -> Range
extendToFullLines = extendLineStart . extendNextLine


-- ---------------------------------------------------------------------

data WithDeletions = IncludeDeletions | SkipDeletions
  deriving (Eq)

-- | Generate a 'WorkspaceEdit' value from a pair of source Text
diffText :: ClientCapabilities -> (VersionedTextDocumentIdentifier, T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText clientCaps old new withDeletions =
  let supports = clientSupportsDocumentChanges clientCaps
   in diffText' supports old new withDeletions

makeDiffTextEdit :: T.Text -> T.Text -> [TextEdit]
makeDiffTextEdit f1 f2 = diffTextEdit f1 f2 IncludeDeletions

makeDiffTextEditAdditive :: T.Text -> T.Text -> [TextEdit]
makeDiffTextEditAdditive f1 f2 = diffTextEdit f1 f2 SkipDeletions

diffTextEdit :: T.Text -> T.Text -> WithDeletions -> [TextEdit]
diffTextEdit fText f2Text withDeletions = r
  where
    r = map diffOperationToTextEdit diffOps
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)

    diffOps =
      filter
        (\x -> (withDeletions == IncludeDeletions) || not (isDeletion x))
        (diffToLineRanges d)

    isDeletion (Deletion _ _) = True
    isDeletion _              = False

    diffOperationToTextEdit :: DiffOperation LineRange -> TextEdit
    diffOperationToTextEdit (Change fm to) = TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ init $ unlines $ lrContents to

    {-
      In order to replace everything including newline characters,
      the end range should extend below the last line. From the specification:
      "If you want to specify a range that contains a line including
      the line ending character(s) then use an end position denoting
      the start of the next line"
    -}
    diffOperationToTextEdit (Deletion (LineRange (sl, el) _) _) = TextEdit range ""
      where
        range =
          Range
            (Position (fromIntegral $ sl - 1) 0)
            (Position (fromIntegral el) 0)
    diffOperationToTextEdit (Addition fm l) = TextEdit range nt
      where
        -- fm has a range wrt to the changed file, which starts in the current file at l + 1
        -- So the range has to be shifted to start at l + 1

        range =
          Range
            (Position (fromIntegral l) 0)
            (Position (fromIntegral l) 0)
        nt = T.pack $ unlines $ lrContents fm

    calcRange fm = Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = Position (fromIntegral $ sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = fromIntegral $ length $ last $ lrContents fm
        e = Position (fromIntegral $ el - 1) ec -- Note: zero-based lines

-- | A pure version of 'diffText' for testing
diffText' :: Bool -> (VersionedTextDocumentIdentifier, T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText' supports (verTxtDocId, fText) f2Text withDeletions =
  if supports
    then WorkspaceEdit Nothing (Just docChanges) Nothing
    else WorkspaceEdit (Just h) Nothing Nothing
  where
    diff = diffTextEdit fText f2Text withDeletions
    h = M.singleton (verTxtDocId ^. L.uri) diff
    docChanges = [InL docEdit]
    docEdit = TextDocumentEdit (verTxtDocId ^. re _versionedTextDocumentIdentifier) $ fmap InL diff

-- ---------------------------------------------------------------------

clientSupportsDocumentChanges :: ClientCapabilities -> Bool
clientSupportsDocumentChanges caps =
  let ClientCapabilities mwCaps _ _ _ _ _ = caps
      supports = do
        wCaps <- mwCaps
        WorkspaceEditClientCapabilities mDc _ _ _ _ <- _workspaceEdit wCaps
        mDc
   in Just True == supports

-- ---------------------------------------------------------------------

pluginDescToIdePlugins :: [PluginDescriptor ideState] -> IdePlugins ideState
pluginDescToIdePlugins = IdePlugins

idePluginsToPluginDesc :: IdePlugins ideState -> [PluginDescriptor ideState]
idePluginsToPluginDesc (IdePlugins pp) = pp

-- ---------------------------------------------------------------------

-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runtime change
-- their configuration.
getClientConfig :: (MonadLsp Config m) => m Config
getClientConfig = getConfig

-- ---------------------------------------------------------------------

-- | Returns the current plugin configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can change their
-- configuration at runtime.
getPluginConfig :: (MonadLsp Config m) => PluginDescriptor c -> m PluginConfig
getPluginConfig plugin = do
  config <- getClientConfig
  return $ configForPlugin config plugin

-- ---------------------------------------------------------------------

-- | Returns the value of a property defined by the current plugin.
usePropertyLsp ::
  (HasProperty s k t r, MonadLsp Config m) =>
  KeyNameProxy s ->
  PluginDescriptor c ->
  Properties r ->
  m (ToHsType t)
usePropertyLsp kn pId p = do
  config <- getPluginConfig pId
  return $ useProperty kn p $ plcConfig config

-- ---------------------------------------------------------------------

-- | Extracts exact matching text in the range.
extractTextInRange :: Range -> T.Text -> T.Text
extractTextInRange (Range (Position sl sc) (Position el ec)) s = newS
  where
    focusLines =
      T.lines s
        -- NOTE: Always append an empty line to the end to ensure there are
        -- sufficient lines to take from.
        --
        -- There is a situation that when the end position is placed at the line
        -- below the last line, if we simply do `drop` and then `take`, there
        -- will be `el - sl` lines left, not `el - sl + 1` lines. And then
        -- the last line of code will be emptied unexpectedly.
        --
        -- For details, see https://github.com/haskell/haskell-language-server/issues/3847
        & (++ [""])
        & drop (fromIntegral sl)
        & take (fromIntegral $ el - sl + 1)
    -- NOTE: We have to trim the last line first to handle the single-line case
    newS =
      focusLines
        & _last %~ T.take (fromIntegral ec)
        & _head %~ T.drop (fromIntegral sc)
        -- NOTE: We cannot use unlines here, because we don't want to add trailing newline!
        & T.intercalate "\n"

-- | Gets the range that covers the entire text
fullRange :: T.Text -> Range
fullRange s = Range startPos endPos
  where
    startPos = Position 0 0
    endPos = Position lastLine 0
    {-
    In order to replace everything including newline characters,
    the end range should extend below the last line. From the specification:
    "If you want to specify a range that contains a line including
    the line ending character(s) then use an end position denoting
    the start of the next line"
    -}
    lastLine = fromIntegral $ length $ T.lines s

subRange :: Range -> Range -> Bool
subRange = isSubrangeOf

-- ---------------------------------------------------------------------

allLspCmdIds' :: T.Text -> IdePlugins ideState -> [T.Text]
allLspCmdIds' pid (IdePlugins ls) =
  allLspCmdIds pid $ map (pluginId &&& pluginCommands) ls

allLspCmdIds :: T.Text -> [(PluginId, [PluginCommand ideState])] -> [T.Text]
allLspCmdIds pid commands = concatMap go commands
  where
    go (plid, cmds) = map (mkLspCmdId pid plid . commandId) cmds

-- ---------------------------------------------------------------------


type TextParser = P.Parsec Void T.Text

-- | Unescape printable escape sequences within double quotes.
-- This is useful if you have to call 'show' indirectly, and it escapes some characters which you would prefer to
-- display as is.
unescape :: T.Text -> T.Text
unescape input =
  case P.runParser escapedTextParser "inline" input of
    Left _     -> input
    Right strs -> T.pack strs

-- | Parser for a string that contains double quotes. Returns unescaped string.
escapedTextParser :: TextParser String
escapedTextParser = concat <$> P.many (outsideStringLiteral P.<|> stringLiteral)
  where
    outsideStringLiteral :: TextParser String
    outsideStringLiteral = P.someTill (P.anySingleBut '"') (P.lookAhead (void (P.char '"') P.<|> P.eof))

    stringLiteral :: TextParser String
    stringLiteral = do
      inside <- P.char '"' >> P.manyTill P.charLiteral (P.char '"')
      let f '"' = "\\\"" -- double quote should still be escaped
      -- Despite the docs, 'showLitChar' and 'showLitString' from 'Data.Char' DOES ESCAPE unicode printable
      -- characters. So we need to call 'isPrint' from 'Data.Char' manually.
          f ch  = if isPrint ch then [ch] else showLitChar ch ""
          inside' = concatMap f inside

      pure $ "\"" <> inside' <> "\""
