{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Ide.PluginUtils
  ( -- * LSP Range manipulation functions
    normalize,
    extendNextLine,
    extendLineStart,
    WithDeletions(..),
    getProcessID,
    makeDiffTextEdit,
    makeDiffTextEditAdditive,
    diffText,
    diffText',
    pluginDescToIdePlugins,
    idePluginsToPluginDesc,
    responseError,
    getClientConfig,
    getPluginConfig,
    configForPlugin,
    pluginEnabled,
    extractRange,
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
    getNormalizedFilePath,
    pluginResponse,
    handleMaybe,
    handleMaybeM,
    throwPluginError,
    unescape,
    )
where


import           Control.Arrow                   ((&&&))
import           Control.Monad.Extra             (maybeM)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Except      (ExceptT, runExceptT, throwE)
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Data.Bifunctor                  (Bifunctor (first))
import           Data.Char                       (isPrint, showLitChar)
import           Data.Functor                    (void)
import qualified Data.HashMap.Strict             as H
import           Data.String                     (IsString (fromString))
import qualified Data.Text                       as T
import           Data.Void                       (Void)
import           Ide.Plugin.Config
import           Ide.Plugin.Properties
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types              hiding
                                                 (SemanticTokenAbsolute (length, line),
                                                  SemanticTokenRelative (length),
                                                  SemanticTokensEdit (_start))
import qualified Language.LSP.Types              as J
import           Language.LSP.Types.Capabilities
import qualified Text.Megaparsec                 as P
import qualified Text.Megaparsec.Char            as P
import qualified Text.Megaparsec.Char.Lexer      as P

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

-- ---------------------------------------------------------------------

data WithDeletions = IncludeDeletions | SkipDeletions
  deriving Eq

-- | Generate a 'WorkspaceEdit' value from a pair of source Text
diffText :: ClientCapabilities -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText clientCaps old new withDeletions =
  let
    supports = clientSupportsDocumentChanges clientCaps
  in diffText' supports old new withDeletions

makeDiffTextEdit :: T.Text -> T.Text -> List TextEdit
makeDiffTextEdit f1 f2 = diffTextEdit f1 f2 IncludeDeletions

makeDiffTextEditAdditive :: T.Text -> T.Text -> List TextEdit
makeDiffTextEditAdditive f1 f2 = diffTextEdit f1 f2 SkipDeletions

diffTextEdit :: T.Text -> T.Text -> WithDeletions -> List TextEdit
diffTextEdit fText f2Text withDeletions = J.List r
  where
    r = map diffOperationToTextEdit diffOps
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)

    diffOps = filter (\x -> (withDeletions == IncludeDeletions) || not (isDeletion x))
                     (diffToLineRanges d)

    isDeletion (Deletion _ _) = True
    isDeletion _              = False


    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
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
    diffOperationToTextEdit (Deletion (LineRange (sl, el) _) _) = J.TextEdit range ""
      where
        range = J.Range (J.Position (fromIntegral $ sl - 1) 0)
                        (J.Position (fromIntegral el) 0)

    diffOperationToTextEdit (Addition fm l) = J.TextEdit range nt
    -- fm has a range wrt to the changed file, which starts in the current file at l + 1
    -- So the range has to be shifted to start at l + 1
      where
        range = J.Range (J.Position (fromIntegral l) 0)
                        (J.Position (fromIntegral l) 0)
        nt = T.pack $ unlines $ lrContents fm


    calcRange fm = J.Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = J.Position (fromIntegral $ sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = fromIntegral $ length $ last $ lrContents fm
        e = J.Position (fromIntegral $ el - 1) ec  -- Note: zero-based lines


-- | A pure version of 'diffText' for testing
diffText' :: Bool -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText' supports (f,fText) f2Text withDeletions  =
  if supports
    then WorkspaceEdit Nothing (Just docChanges) Nothing
    else WorkspaceEdit (Just h) Nothing Nothing
  where
    diff = diffTextEdit fText f2Text withDeletions
    h = H.singleton f diff
    docChanges = J.List [InL docEdit]
    docEdit = J.TextDocumentEdit (J.VersionedTextDocumentIdentifier f (Just 0)) $ fmap InL diff

-- ---------------------------------------------------------------------

clientSupportsDocumentChanges :: ClientCapabilities -> Bool
clientSupportsDocumentChanges caps =
  let ClientCapabilities mwCaps _ _ _ _ = caps
      supports = do
        wCaps <- mwCaps
        WorkspaceEditClientCapabilities mDc _ _ _ _ <- _workspaceEdit wCaps
        mDc
  in
    Just True == supports

-- ---------------------------------------------------------------------

pluginDescToIdePlugins :: [PluginDescriptor ideState] -> IdePlugins ideState
pluginDescToIdePlugins = IdePlugins

idePluginsToPluginDesc :: IdePlugins ideState -> [PluginDescriptor ideState]
idePluginsToPluginDesc (IdePlugins pp) = pp

-- ---------------------------------------------------------------------
-- | Returns the current client configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can at runtime change
-- their configuration.
--
getClientConfig :: MonadLsp Config m => m Config
getClientConfig = getConfig

-- ---------------------------------------------------------------------

-- | Returns the current plugin configuration. It is not wise to permanently
-- cache the returned value of this function, as clients can change their
-- configuration at runtime.
getPluginConfig :: MonadLsp Config m => PluginDescriptor c -> m PluginConfig
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

extractRange :: Range -> T.Text -> T.Text
extractRange (Range (Position sl _) (Position el _)) s = newS
  where focusLines = take (fromIntegral $ el-sl+1) $ drop (fromIntegral sl) $ T.lines s
        newS = T.unlines focusLines

-- | Gets the range that covers the entire text
fullRange :: T.Text -> Range
fullRange s = Range startPos endPos
  where startPos = Position 0 0
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

getNormalizedFilePath :: Monad m => Uri -> ExceptT String m NormalizedFilePath
getNormalizedFilePath uri = handleMaybe errMsg
        $ uriToNormalizedFilePath
        $ toNormalizedUri uri
    where
        errMsg = T.unpack $ "Failed converting " <> getUri uri <> " to NormalizedFilePath"

-- ---------------------------------------------------------------------
throwPluginError :: Monad m => String -> ExceptT String m b
throwPluginError = throwE

handleMaybe :: Monad m => e -> Maybe b -> ExceptT e m b
handleMaybe msg = maybe (throwE msg) return

handleMaybeM :: Monad m => e -> m (Maybe b) -> ExceptT e m b
handleMaybeM msg act = maybeM (throwE msg) return $ lift act

pluginResponse :: Monad m => ExceptT String m a -> m (Either ResponseError a)
pluginResponse =
  fmap (first (\msg -> ResponseError InternalError (fromString msg) Nothing))
    . runExceptT

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
