{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Semantic tokens for @cabal.project@ files.
module Ide.Plugin.CabalProject.Tokens
  ( projectSemanticTokensFull
  , projectSemanticTokensFullDelta
  , scanProjectTokens
  , Log (..)
  ) where

import           Control.Concurrent.STM           (atomically, stateTVar)
import           Control.Lens                     ((^.))
import           Control.Monad.Except             (ExceptT, liftEither,
                                                   withExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import qualified Data.Char                        as Char
import qualified Data.List                        as List
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Text.Utf16.Rope.Mixed       as Rope
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.Shake       (ShakeExtras (..),
                                                   getShakeExtras)
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified StmContainers.Map                as STM
import           System.FilePath                  (takeFileName)

data Log
  = LogDeltaMismatch
  deriving (Show)

instance Pretty Log where
  pretty LogDeltaMismatch = "Semantic tokens delta mismatch"

-- | Empty (encoded) semantic tokens, returned for files this plugin does not
-- handle.
emptySemanticTokens :: SemanticTokens
emptySemanticTokens = SemanticTokens Nothing []

projectSemanticTokensFull :: PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFull
projectSemanticTokensFull state _ param = runActionE "cabal-project.semanticTokensFull" state $ do
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
  if not (isCabalProjectFile nfp)
    then return $ InL emptySemanticTokens
    else do
      tokens <- computeTokens nfp
      lift $ setSemanticTokens nfp tokens
      return $ InL tokens

projectSemanticTokensFullDelta :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentSemanticTokensFullDelta
projectSemanticTokensFullDelta recorder state _ param = do
  nfp <- getNormalizedFilePathE (param ^. L.textDocument . L.uri)
  let previousVersionFromParam = param ^. L.previousResultId
  runActionE "cabal-project.semanticTokensFullDelta" state $
    if not (isCabalProjectFile nfp)
      then return $ InL emptySemanticTokens
      else computeSemanticTokensFullDelta recorder previousVersionFromParam nfp
  where
    computeSemanticTokensFullDelta
      :: Recorder (WithPriority Log)
      -> Text
      -> NormalizedFilePath
      -> ExceptT PluginError Action (MessageResult 'Method_TextDocumentSemanticTokensFullDelta)
    computeSemanticTokensFullDelta recorder previousVersionFromParam nfp = do
      tokens <- computeTokens nfp
      previousSemanticTokensMaybe <- lift $ getPreviousSemanticTokens nfp
      lift $ setSemanticTokens nfp tokens
      case previousSemanticTokensMaybe of
        Nothing -> return $ InL tokens
        Just previousSemanticTokens ->
          if Just previousVersionFromParam == previousSemanticTokens ^. L.resultId
            then return $ InR $ InL $ makeSemanticTokensDeltaWithId (tokens ^. L.resultId) previousSemanticTokens tokens
            else do
              logWith recorder Warning LogDeltaMismatch
              return $ InL tokens

-- | Computes the semantic tokens for the given project file.
computeTokens :: NormalizedFilePath -> ExceptT PluginError Action SemanticTokens
computeTokens nfp = do
  (_, mContents) <- lift $ use_ GetFileContents nfp
  let contents = maybe "" Rope.toText mContents
  sid <- lift getAndIncreaseSemanticTokensId
  withExceptT (PluginInternalError . ("failed to encode semantic tokens: " <>)) . liftEither $
    makeSemanticTokensWithId sid (scanProjectTokens contents)

-----------------------
-- helper functions
-----------------------

-- keep track of the semantic tokens response id
-- so that we can compute the delta between two versions
getAndIncreaseSemanticTokensId :: Action Text
getAndIncreaseSemanticTokensId = do
  ShakeExtras{semanticTokensId} <- getShakeExtras
  liftIO $ atomically $ do
    i <- stateTVar semanticTokensId (\val -> (val, val + 1))
    return $ T.pack $ show i

getPreviousSemanticTokens :: NormalizedFilePath -> Action (Maybe SemanticTokens)
getPreviousSemanticTokens uri = getShakeExtras >>= liftIO . atomically . STM.lookup uri . semanticTokensCache

setSemanticTokens :: NormalizedFilePath -> SemanticTokens -> Action ()
setSemanticTokens uri tokens = getShakeExtras >>= liftIO . atomically . STM.insert tokens uri . semanticTokensCache

makeSemanticTokensWithId :: Text -> [SemanticTokenAbsolute] -> Either Text SemanticTokens
makeSemanticTokensWithId sid tokens = do
  (SemanticTokens _ toks) <- makeSemanticTokens defaultSemanticTokensLegend tokens
  return $ SemanticTokens (Just sid) toks

makeSemanticTokensDeltaWithId :: Maybe Text -> SemanticTokens -> SemanticTokens -> SemanticTokensDelta
makeSemanticTokensDeltaWithId sid previousTokens currentTokens =
  let (SemanticTokensDelta _ stEdits) = makeSemanticTokensDelta previousTokens currentTokens
   in SemanticTokensDelta sid stEdits

-----------------------
-- pure scanner
-----------------------

-- | A line-based scanner for project files: comments, field names (property),
-- section names (keyword), strings, numbers and booleans.
scanProjectTokens :: Text -> [SemanticTokenAbsolute]
scanProjectTokens contents =
  List.sortOn absKey $ concat (zipWith scanLine [0 ..] (T.lines contents))
  where
    absKey (SemanticTokenAbsolute l c _len _ty _mods) = (l, c)
    scanLine :: Int -> Text -> [SemanticTokenAbsolute]
    scanLine lineNo line
      -- whole-line comment
      | "--" `T.isPrefixOf` T.stripStart line =
          [mkToken (intToUInt lineNo) (intToUInt leading) (intToUInt (T.length line - leading)) SemanticTokenTypes_Comment]
      -- field name at (indented) start of line, e.g. `packages:`; also scan the value
      | Just (indent, name) <- fieldAtLineStart line =
          let afterColon = indent + T.length name + 1
           in mkToken (intToUInt lineNo) (intToUInt indent) (intToUInt (T.length name)) SemanticTokenTypes_Property
                : scanChars lineNo afterColon (T.drop afterColon line)
      -- section header: unindented bare word (with optional args), no colon
      | Just word <- sectionAtLineStart line =
          mkToken (intToUInt lineNo) (intToUInt leading) (intToUInt (T.length word)) SemanticTokenTypes_Keyword
            : scanChars lineNo (T.length word) (T.drop (T.length word) line)
      -- anything else: scan for comments, strings, numbers, booleans
      | otherwise = scanChars lineNo 0 line
      where
        leading = T.length line - T.length (T.stripStart line)

    -- `packages:` style field: name chars followed by `:`
    fieldAtLineStart :: Text -> Maybe (Int, Text)
    fieldAtLineStart line = do
      let indent = T.length (T.takeWhile isSpaceChar line)
          rest = T.drop indent line
      if T.null rest || not (Char.isAlphaNum (T.head rest)) then Nothing else do
        let (name, after) = T.span (\c -> Char.isAlphaNum c || c == '-') rest
        if T.null name || not (":" `T.isPrefixOf` after)
          then Nothing
          else Just (indent, name)

    -- section header: bare word (possibly followed by args), no colon in the
    -- leading word; reached only when the line is not a field
    sectionAtLineStart :: Text -> Maybe Text
    sectionAtLineStart line =
      let word = T.takeWhile (\c -> Char.isAlpha c || c == '-') line
       in if T.null word
            || T.null (T.takeWhile (\c -> not (isSpaceChar c) && c /= ':') line)
            then Nothing
            else Just word

    -- character-level scan for strings, comments, numbers, booleans.
    -- Single left-to-right pass over the remaining text, so the cost is
    -- linear in the line length.
    scanChars :: Int -> Int -> Text -> [SemanticTokenAbsolute]
    scanChars lineNo off rest = case T.uncons rest of
      Nothing -> []
      Just (c, rest')
        | "--" `T.isPrefixOf` rest ->
            [mkToken (intToUInt lineNo) (intToUInt off) (intToUInt (T.length rest)) SemanticTokenTypes_Comment]
        | c == '"' ->
            let body = T.drop 1 rest
                bodyLen = maybe (T.length body) id (findClose body) + 1
             in mkToken (intToUInt lineNo) (intToUInt off) (intToUInt bodyLen) SemanticTokenTypes_String
                  : scanChars lineNo (off + 1 + bodyLen) (T.drop (1 + bodyLen) rest)
        | Char.isAlpha c || Char.isDigit c ->
            let (word, restAfter) = T.span (\ch -> Char.isAlphaNum ch || ch == '.' || ch == '-') rest
             in case tokenTypeForWord word of
                  Just ty ->
                    mkToken (intToUInt lineNo) (intToUInt off) (intToUInt (T.length word)) ty
                      : scanChars lineNo (off + T.length word) restAfter
                  Nothing -> scanChars lineNo (off + T.length word) restAfter
        | otherwise -> scanChars lineNo (off + 1) rest'

    tokenTypeForWord :: Text -> Maybe SemanticTokenTypes
    tokenTypeForWord word
      | word == "True" || word == "False" = Just SemanticTokenTypes_Keyword
      | isBareNumber word = Just SemanticTokenTypes_Number
      | otherwise = Nothing

    isBareNumber w =
      not (T.null w)
        && T.all (\ch -> Char.isDigit ch || ch == '.') w
        && T.any Char.isDigit w

    findClose :: Text -> Maybe Int
    findClose = go 0
      where
        go i t
          | i >= T.length t = Nothing
          | T.index t i == '"' = Just i
          | otherwise = go (i + 1) t

mkToken :: UInt -> UInt -> UInt -> SemanticTokenTypes -> SemanticTokenAbsolute
mkToken l c len ty = SemanticTokenAbsolute l c len ty []

intToUInt :: Int -> UInt
intToUInt = fromIntegral

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' ' || c == '\t'

-- | Same predicate as 'Ide.Plugin.CabalProject.isCabalProjectFile'; duplicated
-- locally because importing it from the main module would be a cycle.
isCabalProjectFile :: NormalizedFilePath -> Bool
isCabalProjectFile fp =
  let n = takeFileName (fromNormalizedFilePath fp)
   in n == "cabal.project" || "cabal.project." `List.isPrefixOf` n
