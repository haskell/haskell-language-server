module Ide.Plugin.Notes (descriptor, Log) where

import           Control.Lens                     ((^.))
import           Control.Monad.Except             (ExceptT, MonadError,
                                                   throwError)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.Array                       as A
import           Data.Foldable                    (foldl')
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HM
import qualified Data.HashSet                     as HS
import           Data.List                        (uncons)
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe, mapMaybe)
import           Data.Text                        (Text, intercalate)
import qualified Data.Text                        as T
import qualified Data.Text.Utf16.Rope.Mixed       as Rope
import           Data.Traversable                 (for)
import           Development.IDE                  hiding (line)
import           Development.IDE.Core.PluginUtils (runActionE, useE)
import           Development.IDE.Core.Shake       (toKnownFiles)
import qualified Development.IDE.Core.Shake       as Shake
import           Development.IDE.Graph.Classes    (Hashable, NFData)
import           GHC.Generics                     (Generic)
import           Ide.Plugin.Error                 (PluginError (..))
import           Ide.Types
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (Method_TextDocumentCompletion, Method_TextDocumentDefinition, Method_TextDocumentHover, Method_TextDocumentReferences),
                                                   SMethod (SMethod_TextDocumentCompletion, SMethod_TextDocumentDefinition, SMethod_TextDocumentHover, SMethod_TextDocumentReferences))
import           Language.LSP.Protocol.Types
import           Text.Regex.TDFA                  (Regex, caseSensitive,
                                                   defaultCompOpt,
                                                   defaultExecOpt,
                                                   makeRegexOpts, matchAllText)

data Log
    = LogShake Shake.Log
    | LogNotesFound NormalizedFilePath [(Text, [Position])]
    | LogNoteReferencesFound NormalizedFilePath [(Text, [Position])]
    deriving Show

data GetNotesInFile = MkGetNotesInFile
    deriving (Show, Generic, Eq, Ord)
    deriving anyclass (Hashable, NFData)
-- The GetNotesInFile action scans the source file and extracts a map of note
-- definitions (note name -> position) and a map of note references
-- (note name -> [position]).
type instance RuleResult GetNotesInFile = (HM.HashMap Text Position, HM.HashMap Text [Position])

data GetNotes = MkGetNotes
    deriving (Show, Generic, Eq, Ord)
    deriving anyclass (Hashable, NFData)
-- GetNotes collects all note definition across all files in the
-- project. It returns a map from note name to pair of (filepath, position).
type instance RuleResult GetNotes = HashMap Text (NormalizedFilePath, Position)

data GetNoteReferences = MkGetNoteReferences
    deriving (Show, Generic, Eq, Ord)
    deriving anyclass (Hashable, NFData)
-- GetNoteReferences collects all note references across all files in the
-- project. It returns a map from note name to list of (filepath, position).
type instance RuleResult GetNoteReferences = HashMap Text [(NormalizedFilePath, Position)]

instance Pretty Log where
    pretty = \case
            LogShake l -> pretty l
            LogNoteReferencesFound file refs -> "Found note references in " <> prettyNotes file refs
            LogNotesFound file notes -> "Found notes in " <> prettyNotes file notes
        where prettyNotes file hm = pretty (show file) <> ": ["
                <> pretty (intercalate ", " (fmap (\(s, p) -> "\"" <> s <> "\" at " <> intercalate ", " (map (T.pack . show) p)) hm)) <> "]"

{-
The first time the user requests a jump-to-definition on a note reference, the
project is indexed and searched for all note definitions. Their location and
title is then saved in the HLS database to be retrieved for all future requests.
-}
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides goto definition support for GHC-style notes")
    { Ide.Types.pluginRules = findNotesRules recorder
    , Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentDefinition jumpToNote
        <> mkPluginHandler SMethod_TextDocumentReferences listReferences
        <> mkPluginHandler SMethod_TextDocumentHover hoverNote
        <> mkPluginHandler SMethod_TextDocumentCompletion autocomplete
    }

findNotesRules :: Recorder (WithPriority Log) -> Rules ()
findNotesRules recorder = do
    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \MkGetNotesInFile nfp -> do
        findNotesInFile nfp recorder

    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \MkGetNotes _ -> do
        targets <- toKnownFiles <$> useNoFile_ GetKnownTargets
        definedNotes <- catMaybes <$> mapM (\nfp -> fmap (HM.map (nfp,) . fst) <$> use MkGetNotesInFile nfp) (HS.toList targets)
        pure $ Just $ HM.unions definedNotes

    defineNoDiagnostics (cmapWithPrio LogShake recorder) $ \MkGetNoteReferences _ -> do
        targets <- toKnownFiles <$> useNoFile_ GetKnownTargets
        definedReferences <- catMaybes <$> for (HS.toList targets) (\nfp -> do
                references <- fmap snd <$> use MkGetNotesInFile nfp
                pure $ fmap (HM.map (fmap (nfp,))) references
            )
        pure $ Just $ foldl' (HM.unionWith (<>)) HM.empty definedReferences

err :: MonadError PluginError m => Text -> Maybe a -> m a
err s = maybe (throwError $ PluginInternalError s) pure

getNote :: NormalizedFilePath -> IdeState -> Position -> ExceptT PluginError (HandlerM c) (Maybe Text)
getNote nfp state (Position l c) = do
    contents <-
        err "Error getting file contents"
        =<< liftIO (runAction "notes.getfileContents" state (getFileContents nfp))
    line <- err "Line not found in file" (listToMaybe $ Rope.lines $ fst
        (Rope.splitAtLine 1 $ snd $ Rope.splitAtLine (fromIntegral l) contents))
    pure $ listToMaybe $ mapMaybe (atPos $ fromIntegral c) $ matchAllText noteRefRegex line
  where
    atPos c arr = case arr A.! 0 of
        -- We check if the line we are currently at contains a note
        -- reference. However, we need to know if the cursor is within the
        -- match or somewhere else. The second entry of the array contains
        -- the title of the note as extracted by the regex.
        (_, (c', len)) -> if c' <= c && c <= c' + len
            then Just (fst (arr A.! 1)) else Nothing

listReferences :: PluginMethodHandler IdeState Method_TextDocumentReferences
listReferences state _ param
    | Just nfp <- uriToNormalizedFilePath uriOrig
    = do
        let pos@(Position l _) = param ^. L.position
        noteOpt <- getNote nfp state pos
        case noteOpt of
            Nothing -> pure (InR Null)
            Just note -> do
                notes <- runActionE "notes.definedNoteReferencess" state $ useE MkGetNoteReferences nfp
                poss <- err ("Note reference (a comment of the form `{- Note [" <> note <> "] -}`) not found") (HM.lookup note notes)
                pure $ InL (mapMaybe (\(noteFp, pos@(Position l' _)) -> if l' == l then Nothing else Just (
                        Location (fromNormalizedUri $ normalizedFilePathToUri noteFp) (Range pos pos))) poss)
    where
        uriOrig = toNormalizedUri $ param ^. (L.textDocument . L.uri)
listReferences _ _ _ = throwError $ PluginInternalError "conversion to normalized file path failed"

jumpToNote :: PluginMethodHandler IdeState Method_TextDocumentDefinition
jumpToNote state _ param
    | Just nfp <- uriToNormalizedFilePath uriOrig
    = do
        noteOpt <- getNote nfp state (param ^. L.position)
        case noteOpt of
            Nothing -> pure (InR (InR Null))
            Just note -> do
                notes <- runActionE "notes.definedNotes" state $ useE MkGetNotes nfp
                (noteFp, pos) <- err ("Note definition (a comment of the form `{- Note [" <> note <> "]\\n~~~ ... -}`) not found") (HM.lookup note notes)
                pure $ InL (Definition (InL
                        (Location (fromNormalizedUri $ normalizedFilePathToUri noteFp) (Range pos pos))
                    ))
    where
        uriOrig = toNormalizedUri $ param ^. (L.textDocument . L.uri)
jumpToNote _ _ _ = throwError $ PluginInternalError "conversion to normalized file path failed"

findNotesInFile :: NormalizedFilePath -> Recorder (WithPriority Log) -> Action (Maybe (HM.HashMap Text Position, HM.HashMap Text [Position]))
findNotesInFile file recorder = do
    -- GetFileContents only returns a value if the file is open in the editor of
    -- the user. If not, we need to read it from disk.
    contentOpt <- (snd =<<) <$> use GetFileContents file
    content <- case contentOpt of
        Just x  -> pure $ Rope.toText x
        Nothing -> liftIO $ readFileUtf8 $ fromNormalizedFilePath file
    let noteMatches = (A.! 1) <$> matchAllText noteRegex content
        notes = toPositions noteMatches content
    logWith recorder Debug $ LogNotesFound file (HM.toList notes)
    let refMatches = (A.! 1) <$> matchAllText noteRefRegex content
        refs = toPositions refMatches content
    logWith recorder Debug $ LogNoteReferencesFound file (HM.toList refs)
    pure $ Just (HM.mapMaybe (fmap fst . uncons) notes, refs)
    where
        uint = fromIntegral . toInteger
        -- the regex library returns the character index of the match. However
        -- to return the position from HLS we need it as a (line, character)
        -- tuple. To convert between the two we count the newline characters and
        -- reset the current character index every time. For every regex match,
        -- once we have counted up to their character index, we save the current
        -- line and character values instead.
        toPositions matches = snd . fst . T.foldl' (\case
            (([], m), _) -> const (([], m), (0, 0, 0))
            ((x@(name, (char, _)):xs, m), (n, nc, c)) -> \char' ->
                let !c' = c + 1
                    (!n', !nc') = if char' == '\n' then (n + 1, c') else (n, nc)
                    p@(!_, !_) = if char == c then
                            (xs, HM.insertWith (<>) name [Position (uint n') (uint (char - nc'))] m)
                        else (x:xs, m)
                in (p, (n', nc', c'))
            ) ((matches, HM.empty), (0, 0, 0))

noteRefRegex, noteRegex :: Regex
(noteRefRegex, noteRegex) =
    ( mkReg ("note[[:blank:]]?\\[(.+)\\]" :: String)
    , mkReg ("note[[:blank:]]?\\[([[:print:]]+)\\][[:blank:]]*\r?\n[[:blank:]]*(--)?[[:blank:]]*~~~" :: String)
    )
    where
        mkReg = makeRegexOpts (defaultCompOpt { caseSensitive = False }) defaultExecOpt

-- | Find the precise range of `note[...]` or `note [...]` on a line.
findNoteRange
  :: Text   -- ^ Full line text
  -> Text   -- ^ Note title
  -> UInt   -- ^ Line number
  -> Maybe Range
findNoteRange line _note lineNo =
  case matchAllText noteRefRegex line of
    [] -> Nothing
    (arr:_) ->
      case arr A.! 0 of
        (_, (start, len)) ->
          let startCol = fromIntegral start
              endCol   = startCol + fromIntegral len
          in Just $
               Range
                 (Position lineNo startCol)
                 (Position lineNo endCol)

-- Given the path and position of a Note Declaration, finds Content in it.
-- ignores ~ as a seprator
extractNoteContent
  :: NormalizedFilePath
  -> Position
  -> IO (Maybe Text)
extractNoteContent nfp (Position startLine _) = do
  fileText <- readFileUtf8 $ fromNormalizedFilePath nfp

  let allLines = T.lines fileText
      afterDecl = drop (fromIntegral startLine + 1) allLines
      afterSeparator =
        case dropWhile (not . isSeparatorLine) afterDecl of
          []     -> []
          (_:xs) -> xs

      bodyLines = takeWhile (not . isStopLine) afterSeparator

  if null afterSeparator
     then pure Nothing
     else pure $ Just (T.unlines (map stripCommentPrefix bodyLines))

  where
    isSeparatorLine :: Text -> Bool
    isSeparatorLine t = "~~~" `T.isInfixOf` t

    isNoteHeader :: Text -> Bool
    isNoteHeader t =
      let stripped = T.toLower (T.stripStart (stripCommentPrefix t))
      in "note [" `T.isPrefixOf` stripped

    isEndMarker :: Text -> Bool
    isEndMarker t = "-}" `T.isInfixOf` t

    isStopLine :: Text -> Bool
    isStopLine t = isSeparatorLine t || isNoteHeader t || isEndMarker t

    stripCommentPrefix :: Text -> Text
    stripCommentPrefix t =
      let s = T.stripStart t
      in if "--" `T.isPrefixOf` s
            then T.stripStart (T.drop 2 s)
            else s

normalizeNewlines :: Text -> Text
normalizeNewlines = T.replace "\r\n" "\n"

-- on hovering Note References, shows corresponding Declaration if it Exists
-- ignores Note Declaration
hoverNote :: PluginMethodHandler IdeState Method_TextDocumentHover
hoverNote state _ params
  | Just nfp <- uriToNormalizedFilePath uriOrig
  = do
      let pos@(Position line _) = params ^. L.position
      noteOpt <- getNote nfp state pos
      case noteOpt of
        Nothing -> pure (InR Null)

        Just note -> do
          mbRope <- liftIO $ runAction "notes.hoverLine" state (getFileContents nfp)

          -- compute precise hover range for highlighting corresponding Note Reference on Hover
          let lineText =
                case mbRope of
                  Nothing   -> ""
                  Just rope -> fromMaybe "" $ listToMaybe $ drop (fromIntegral line) $ Rope.lines rope

              mbRange = findNoteRange lineText note line

          notes <- runActionE "notes.hover" state $ useE MkGetNotes nfp
          case HM.lookup note notes of
            Nothing -> pure $ InL $ Hover (InL $ MarkupContent MarkupKind_Markdown "_No declaration available_") mbRange

            Just (defFile, defPos) ->
              if nfp == defFile && pos ^. L.line == defPos ^. L.line
                then pure (InR Null)
                else do
                  mbContent <- liftIO $ extractNoteContent defFile defPos

                  let contents =
                        case mbContent of
                          Nothing   -> "_No declaration available_"
                          Just body -> T.unlines [note, "", body]
                      normalizedContents = normalizeNewlines contents

                  pure $ InL $ Hover (InL $ MarkupContent MarkupKind_Markdown normalizedContents) mbRange
  where
    uriOrig = toNormalizedUri $ params ^. (L.textDocument . L.uri)

hoverNote _ _ _ = pure (InR Null)

-- Gives an autocomplete suggestion when 'note' prefix is detected
autocomplete :: PluginMethodHandler IdeState Method_TextDocumentCompletion
autocomplete state _ params = do
  let uri = params ^. (L.textDocument . L.uri)
      pos = params ^. L.position
      nuri = toNormalizedUri uri

  contents <-
    liftIO $
      runAction "Notes.GetUriContents" state $
        getUriContents nuri

  fmap InL $
    case contents of
      Nothing -> pure []

      Just rope -> do
        let linePrefix =  T.toLower $  T.stripEnd $  getLinePrefix rope pos

        -- Suggest NOTE DECLARATION snippit if "note" prefix detected
        if T.strip linePrefix == "note"
        then
          pure [CompletionItem "Note" Nothing (Just CompletionItemKind_Keyword) Nothing
              (Just "Note Declaration") Nothing Nothing Nothing Nothing
              Nothing (Just noteSnippet) (Just InsertTextFormat_Snippet) Nothing
              Nothing Nothing Nothing Nothing Nothing Nothing
          ]

        -- Suggest list of all NOTE DECLARATION if "note [" infix detected
        else if "note[" `T.isInfixOf` linePrefix || "note [" `T.isInfixOf` linePrefix
        then
          case uriToNormalizedFilePath nuri of
            Nothing -> pure []

            Just nfp -> do
              let typed =
                   case T.breakOnEnd "[" linePrefix of
                    (_, "")   -> ""
                    (_, rest)-> T.strip rest

              notesMap <-
                runActionE "notes.completion.notes" state $
                  useE MkGetNotes nfp

              let allNotes = HM.keys notesMap
                  matches =
                    filter
                      (\n -> T.toLower typed `T.isPrefixOf` T.toLower n)
                      allNotes

                  finalNotes =
                    if null matches then allNotes else matches
              pure $
                map
                  (\n ->
                     CompletionItem n Nothing (Just CompletionItemKind_Reference) Nothing (Just "Note reference")
                       Nothing Nothing (Just True) (Just "0") (Just n)
                       Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing
                  )
                  finalNotes
        else
          pure []

noteSnippet :: Text
noteSnippet =
  T.unlines
    [ "{- Note [${1:Declaration title}]"
    , "~~~"
    , "${2:Content}"
    , "-}"
    ]

getLinePrefix :: Rope.Rope -> Position -> Text
getLinePrefix rope (Position line col) =
  case Rope.splitAtLine (fromIntegral line) rope of
    (_, rest) ->
      case Rope.lines rest of
        (l:_) -> T.take (fromIntegral col) l
        _     -> ""
