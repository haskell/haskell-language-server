-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
module Development.IDE.GHC.Error
  (
    -- * Producing Diagnostic values
    diagFromErrMsgs
  , diagFromErrMsg
  , diagFromString
  , diagFromStrings
  , diagFromGhcException

  -- * utilities working with spans
  , srcSpanToLocation
  , srcSpanToFilename
  , zeroSpan
  , realSpan
  , noSpan
  ) where

import                     Development.IDE.Types.Diagnostics as D
import qualified           Data.Text as T
import Development.IDE.Types.Location
import Development.IDE.GHC.Orphans()
import qualified FastString as FS
import           GHC
import           Bag
import           ErrUtils
import           SrcLoc
import qualified Outputable                 as Out



diagFromText :: D.DiagnosticSeverity -> SrcSpan -> T.Text -> FileDiagnostic
diagFromText sev loc msg = (toNormalizedFilePath $ srcSpanToFilename loc,)
    Diagnostic
    { _range    = srcSpanToRange loc
    , _severity = Just sev
    , _source   = Just "compiler" -- should really be 'daml' or 'haskell', but not shown in the IDE so who cares
    , _message  = msg
    , _code     = Nothing
    , _relatedInformation = Nothing
    }

-- | Produce a GHC-style error from a source span and a message.
diagFromErrMsg :: DynFlags -> ErrMsg -> [FileDiagnostic]
diagFromErrMsg dflags e =
    [ diagFromText sev (errMsgSpan e) $ T.pack $ Out.showSDoc dflags $ ErrUtils.pprLocErrMsg e
    | Just sev <- [toDSeverity $ errMsgSeverity e]]


diagFromErrMsgs :: DynFlags -> Bag ErrMsg -> [FileDiagnostic]
diagFromErrMsgs dflags = concatMap (diagFromErrMsg dflags) . bagToList


-- | Convert a GHC SrcSpan to a DAML compiler Range
srcSpanToRange :: SrcSpan -> Range
srcSpanToRange (UnhelpfulSpan _)  = noRange
srcSpanToRange (RealSrcSpan real) = realSrcSpanToRange real

realSrcSpanToRange :: RealSrcSpan -> Range
realSrcSpanToRange real =
  Range (Position (srcSpanStartLine real - 1) (srcSpanStartCol real - 1))
            (Position (srcSpanEndLine real - 1) (srcSpanEndCol real - 1))

-- | Extract a file name from a GHC SrcSpan (use message for unhelpful ones)
-- FIXME This may not be an _absolute_ file name, needs fixing.
srcSpanToFilename :: SrcSpan -> FilePath
srcSpanToFilename (UnhelpfulSpan fs) = FS.unpackFS fs
srcSpanToFilename (RealSrcSpan real) = FS.unpackFS $ srcSpanFile real

srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation src =
  Location (filePathToUri $ srcSpanToFilename src) (srcSpanToRange src)

-- | Convert a GHC severity to a DAML compiler Severity. Severities below
-- "Warning" level are dropped (returning Nothing).
toDSeverity :: GHC.Severity -> Maybe D.DiagnosticSeverity
toDSeverity SevOutput      = Nothing
toDSeverity SevInteractive = Nothing
toDSeverity SevDump        = Nothing
toDSeverity SevInfo        = Just DsInfo
toDSeverity SevWarning     = Just DsWarning
toDSeverity SevError       = Just DsError
toDSeverity SevFatal       = Just DsError


-- | Produce a bag of GHC-style errors (@ErrorMessages@) from the given
--   (optional) locations and message strings.
diagFromStrings :: [(SrcSpan, String)] -> [FileDiagnostic]
diagFromStrings = concatMap (uncurry diagFromString)

-- | Produce a GHC-style error from a source span and a message.
diagFromString :: SrcSpan -> String -> [FileDiagnostic]
diagFromString sp x = [diagFromText DsError sp $ T.pack x]


-- | Produces an "unhelpful" source span with the given string.
noSpan :: String -> SrcSpan
noSpan = UnhelpfulSpan . FS.fsLit


-- | creates a span with zero length in the filename of the argument passed
zeroSpan :: FS.FastString -- ^ file path of span
         -> RealSrcSpan
zeroSpan file = realSrcLocSpan (mkRealSrcLoc file 1 1)

realSpan :: SrcSpan
         -> Maybe RealSrcSpan
realSpan = \case
  RealSrcSpan r -> Just r
  UnhelpfulSpan _ -> Nothing


diagFromGhcException :: DynFlags -> GhcException -> [FileDiagnostic]
diagFromGhcException dflags exc = diagFromString (noSpan "<Internal>") (showGHCE dflags exc)

showGHCE :: DynFlags -> GhcException -> String
showGHCE dflags exc = case exc of
        Signal n
          -> "Signal: " <> show n

        Panic s
          -> unwords ["Compilation Issue:", s, "\n", requestReport]
        PprPanic  s sdoc
          -> unlines ["Compilation Issue", s,""
                     , Out.showSDoc dflags sdoc
                     , requestReport ]

        Sorry s
          -> "Unsupported feature: " <> s
        PprSorry s sdoc
          -> unlines ["Unsupported feature: ", s,""
                     , Out.showSDoc dflags sdoc]


        ---------- errors below should not happen at all --------
        InstallationError str
          -> "Installation error: " <> str

        UsageError str -- should never happen
          -> unlines ["Unexpected usage error", str]

        CmdLineError str
          -> unlines ["Unexpected usage error", str]

        ProgramError str
            -> "Program error: " <> str
        PprProgramError str  sdoc  ->
            unlines ["Program error:", str,""
                    , Out.showSDoc dflags sdoc]
  where
    requestReport = "Please report this bug to the compiler authors."
