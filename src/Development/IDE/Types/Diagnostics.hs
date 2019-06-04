-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE BlockArguments #-}
module Development.IDE.Types.Diagnostics (
  LSP.Diagnostic(..),
  FileDiagnostics,
  FileDiagnostic,
  Location(..),
  Range(..),
  LSP.DiagnosticSeverity(..),
  Position(..),
  DiagnosticStore,
  DiagnosticRelatedInformation(..),
  List(..),
  StoreItem(..),
  Uri(..),
  noLocation,
  noRange,
  noFilePath,
  ideErrorText,
  ideErrorPretty,
  errorDiag,
  ideTryIOException,
  showDiagnostics,
  showDiagnosticsColored,
  defDiagnostic,
  filePathToUri',
  uriToFilePath',
  ProjectDiagnostics,
  emptyDiagnostics,
  setStageDiagnostics,
  getAllDiagnostics,
  filterDiagnostics,
  getFileDiagnostics,
  prettyDiagnostics
  ) where

import Control.Exception
import Data.Either.Combinators
import Data.Maybe as Maybe
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Syntax
import qualified Data.SortedList as SL
import Network.URI (escapeURIString)
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Pretty
import qualified Language.Haskell.LSP.Types as LSP
import Language.Haskell.LSP.Types as LSP (
    DiagnosticSeverity(..)
  , Diagnostic(..)
  , filePathToUri
  , List(..)
  , DiagnosticRelatedInformation(..)
  , Uri(..)
  )
import Language.Haskell.LSP.Diagnostics

import Development.IDE.Types.Location

-- | We use an empty string as a filepath when we don’t have a file.
-- However, haskell-lsp doesn’t support that in uriToFilePath and given
-- that it is not a valid filepath it does not make sense to upstream a fix.
-- So we have our own wrapper here that supports empty filepaths.
uriToFilePath' :: Uri -> Maybe FilePath
uriToFilePath' uri
    | uri == filePathToUri' "" = Just ""
    | otherwise = LSP.uriToFilePath uri

-- TODO This is a temporary hack: VSCode escapes ':' in URIs while haskell-lsp’s filePathToUri doesn't.
-- This causes issues since haskell-lsp stores the original URI in the VFS while we roundtrip once via
-- uriToFilePath' and filePathToUri before we look it up again. At that point : will be unescaped in the URI
-- so the lookup fails. The long-term solution here is to avoid roundtripping URIs but that is a larger task
-- so for now we have our own version of filePathToUri that does escape colons.
filePathToUri' :: FilePath -> Uri
filePathToUri' fp =
    case T.stripPrefix "file:" (getUri uri) of
        Just suffix -> Uri $ T.pack $ "file:" <> escapeURIString (/= ':') (T.unpack suffix)
        Nothing -> uri
    where uri = filePathToUri fp

ideErrorText :: FilePath -> T.Text -> FileDiagnostic
ideErrorText fp = errorDiag fp "Ide Error"

ideErrorPretty :: Pretty.Pretty e => FilePath -> e -> FileDiagnostic
ideErrorPretty fp = ideErrorText fp . T.pack . Pretty.prettyShow

errorDiag :: FilePath -> T.Text -> T.Text -> FileDiagnostic
errorDiag fp src msg =
  (fp,  diagnostic noRange LSP.DsError src msg)

-- | This is for compatibility with our old diagnostic type
diagnostic :: Range
           -> LSP.DiagnosticSeverity
           -> T.Text -- ^ source
           -> T.Text -- ^ message
           -> LSP.Diagnostic
diagnostic rng sev src msg
    = LSP.Diagnostic {
          _range = rng,
          _severity = Just sev,
          _code = Nothing,
          _source = Just src,
          _message = msg,
          _relatedInformation = Nothing
          }

-- | Any optional field is instantiated to Nothing
defDiagnostic ::
  Range ->
  T.Text -> -- ^ error message
  LSP.Diagnostic
defDiagnostic _range _message = LSP.Diagnostic {
    _range
  , _message
  , _severity = Nothing
  , _code = Nothing
  , _source = Nothing
  , _relatedInformation = Nothing
  }

ideTryIOException :: FilePath -> IO a -> IO (Either FileDiagnostic a)
ideTryIOException fp act =
  mapLeft (\(e :: IOException) -> ideErrorText fp $ T.pack $ show e) <$> try act

-- | Human readable diagnostics for a specific file.
--
--   This type packages a pretty printed, human readable error message
--   along with the related source location so that we can display the error
--   on either the console or in the IDE at the right source location.
--
type FileDiagnostics = (FilePath, [Diagnostic])
type FileDiagnostic = (FilePath, Diagnostic)

prettyRange :: Range -> Doc SyntaxClass
prettyRange Range{..} = f _start <> "-" <> f _end
    where f Position{..} = pretty (_line+1) <> colon <> pretty _character

stringParagraphs :: T.Text -> Doc a
stringParagraphs = vcat . map (fillSep . map pretty . T.words) . T.lines

showDiagnostics :: [FileDiagnostic] -> T.Text
showDiagnostics = srenderPlain . prettyDiagnostics

showDiagnosticsColored :: [FileDiagnostic] -> T.Text
showDiagnosticsColored = srenderColored . prettyDiagnostics


prettyDiagnostics :: [FileDiagnostic] -> Doc SyntaxClass
prettyDiagnostics = vcat . map prettyDiagnostic

prettyDiagnostic :: FileDiagnostic -> Doc SyntaxClass
prettyDiagnostic (fp, LSP.Diagnostic{..}) =
    vcat
        [ slabel_ "File:    " $ pretty fp
        , slabel_ "Range:   " $ prettyRange _range
        , slabel_ "Source:  " $ pretty _source
        , slabel_ "Severity:" $ pretty $ show sev
        , slabel_ "Message: "
            $ case sev of
              LSP.DsError -> annotate ErrorSC
              LSP.DsWarning -> annotate WarningSC
              LSP.DsInfo -> annotate InfoSC
              LSP.DsHint -> annotate HintSC
            $ stringParagraphs _message
        , slabel_ "Code:" $ pretty _code
        ]
    where
        sev = fromMaybe LSP.DsError _severity

getDiagnosticsFromStore :: StoreItem -> [Diagnostic]
getDiagnosticsFromStore (StoreItem _ diags) =
    toList =<< Map.elems diags

-- | This represents every diagnostic in a LSP project, the stage type variable is
--   the type of the compiler stages, in this project that is always the Key data
--   type found in Development.IDE.State.Shake
newtype ProjectDiagnostics stage = ProjectDiagnostics {getStore :: DiagnosticStore}
    deriving Show

emptyDiagnostics :: ProjectDiagnostics stage
emptyDiagnostics = ProjectDiagnostics mempty

-- | Sets the diagnostics for a file and compilation step
--   if you want to clear the diagnostics call this with an empty list
setStageDiagnostics ::
  Show stage =>
  FilePath ->
  Maybe Int ->
  -- ^ the time that the file these diagnostics originate from was last edited
  stage ->
  [LSP.Diagnostic] ->
  ProjectDiagnostics stage ->
  ProjectDiagnostics stage
setStageDiagnostics fp timeM stage diags (ProjectDiagnostics ds) =
    ProjectDiagnostics $ updateDiagnostics ds uri timeM diagsBySource
    where
        diagsBySource = Map.singleton (Just $ T.pack $ show stage) (SL.toSortedList diags)
        uri = filePathToUri fp

fromUri :: LSP.Uri -> FilePath
fromUri = fromMaybe noFilePath . uriToFilePath'

getAllDiagnostics ::
    ProjectDiagnostics stage ->
    [FileDiagnostic]
getAllDiagnostics =
    concatMap (\(k,v) -> map (fromUri k,) $ getDiagnosticsFromStore v) . Map.toList . getStore

getFileDiagnostics ::
    FilePath ->
    ProjectDiagnostics stage ->
    [LSP.Diagnostic]
getFileDiagnostics fp ds =
    maybe [] getDiagnosticsFromStore $
    Map.lookup (filePathToUri fp) $
    getStore ds

filterDiagnostics ::
    (FilePath -> Bool) ->
    ProjectDiagnostics stage ->
    ProjectDiagnostics stage
filterDiagnostics keep =
    ProjectDiagnostics .
    Map.filterWithKey (\uri _ -> maybe True keep $ uriToFilePath' uri) .
    getStore
