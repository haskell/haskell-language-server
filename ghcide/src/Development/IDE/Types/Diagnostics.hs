-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Development.IDE.Types.Diagnostics (
  Diagnostic(..),
  FileDiagnostics(..),
  Location(..),
  Range(..),
  Severity(..),
  Position(..),
  noLocation,
  noRange,
  ideErrorText,
  ideErrorPretty,
  errorDiag,
  ideTryIOException,
  prettyFileDiagnostics,
  prettyDiagnostic
  ) where

import Control.DeepSeq
import Control.Exception
import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Combinators
import Data.List.Extra
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc.Syntax
import GHC.Generics
import qualified Network.URI.Encode
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Pretty

import Development.IDE.Types.Location

ideErrorText :: FilePath -> T.Text -> Diagnostic
ideErrorText absFile = errorDiag absFile "Ide Error"

ideErrorPretty :: Pretty.Pretty e => FilePath -> e -> Diagnostic
ideErrorPretty absFile = ideErrorText absFile . T.pack . Pretty.prettyShow

errorDiag :: FilePath -> T.Text -> T.Text -> Diagnostic
errorDiag fp src msg =
  Diagnostic
  { dFilePath = fp
  , dRange    = noRange
  , dSeverity = Error
  , dSource   = src
  , dMessage  = msg
  }

ideTryIOException :: FilePath -> IO a -> IO (Either Diagnostic a)
ideTryIOException fp act =
  mapLeft (\(e :: IOException) -> ideErrorText fp $ T.pack $ show e) <$> try act

data Diagnostic = Diagnostic
    { dFilePath     :: !FilePath
      -- ^ Specific file that the diagnostic refers to.
    , dRange        :: !Range
      -- ^ The range to which the diagnostic applies.
    , dSeverity     :: !Severity
      -- ^ The severity of the diagnostic, such as 'SError' or 'SWarning'.
    , dSource       :: !T.Text
      -- ^ Human-readable description for the source of the diagnostic,
      -- for example 'parser'.
    , dMessage      :: !T.Text
      -- ^ The diagnostic's message.
    }
    deriving (Eq, Ord, Show, Generic)

instance NFData Diagnostic

-- | The diagnostic severity.
data Severity
    = Error | Warning
    deriving (Eq, Ord, Show, Generic)

instance NFData Severity

-- | Human readable diagnostics for a specific file.
--
--   This type packages a pretty printed, human readable error message
--   along with the related source location so that we can display the error
--   on either the console or in the IDE at the right source location.
--
data FileDiagnostics = FileDiagnostics
    { fdFilePath    :: !FilePath
      -- ^ Path of the module that we were trying to process.
      --   In a multi-module program this is the file that we started
      --   trying to compile, not necessarily the one in which we found the
      --   reported errors or warnings.
    , fdDiagnostics :: ![Diagnostic]
      -- ^ Diagnostics for the desired module,
      --   as well as any transitively imported modules.
    }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON Diagnostic
instance ToJSON Diagnostic

instance FromJSON Severity
instance ToJSON Severity

instance FromJSON FileDiagnostics
instance ToJSON FileDiagnostics

prettyFileDiagnostics :: FileDiagnostics -> Doc SyntaxClass
prettyFileDiagnostics (FileDiagnostics filePath diagnostics) =
    label_ "Compiler error in" $ vcat
        [ label_ "File:" $ pretty filePath
        , label_ "Errors:" $ vcat $ map prettyDiagnostic $ nubOrd diagnostics
        ]

stringParagraphs :: T.Text -> Doc a
stringParagraphs = vcat . map (fillSep . map pretty . T.words) . T.lines

prettyDiagnostic :: Diagnostic -> Doc SyntaxClass
prettyDiagnostic (Diagnostic filePath range severity source msg) =
    vcat
        [ label_ "File:    " $ pretty filePath
        , label_ "Range:   "
            $ annotate (LinkSC uri title)
            $ pretty range
        , label_ "Source:  " $ pretty source
        , label_ "Severity:" $ pretty $ show severity
        , label_ "Message: "
            $ case severity of
              Error -> annotate ErrorSC
              Warning -> annotate WarningSC
            $ stringParagraphs msg
        ]
    where
        -- FIXME(JM): Move uri construction to DA.Pretty?
        Position sline _ = rangeStart range
        Position eline _ = rangeEnd range
        uri = "command:daml.revealLocation?"
            <> Network.URI.Encode.encodeText ("[\"file://" <> T.pack filePath <> "\","
            <> T.pack (show sline) <> ", " <> T.pack (show eline) <> "]")
        title = T.pack filePath
