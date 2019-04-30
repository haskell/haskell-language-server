-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE DuplicateRecordFields #-}
module Development.IDE.Functions.GHCError
  ( mkDiag
  , toDiagnostics
  , srcSpanToLocation

    -- * Producing GHC ErrorMessages
  , mkErrors
  , mkError
  , mkErrorDoc
  , mkErrorsGhcException

  -- * Handling errors in the GHC monad (SourceError, ErrorMessages)
  , Diagnostic
  , ErrorMessages -- included in module export below
  , ErrMsg
  , errMsgSpan
  , errMsgSeverity
  , mkPlainErrMsg

  -- * utilities working with 'ErrMsg' and 'ErrorMessages'
  , zeroSpan
  , realSpan
  , noSpan
  ) where

import Control.Lens
import                     Development.IDE.Types.Diagnostics as D
import qualified           Data.Text as T
import Development.IDE.UtilGHC()
import qualified "ghc-lib-parser" FastString as FS
import "ghc-lib"           GHC
import "ghc-lib-parser"           Bag
import Data.Maybe
import           "ghc-lib-parser" ErrUtils
import           "ghc-lib-parser" SrcLoc
import qualified "ghc-lib-parser" Outputable                 as Out
import qualified Language.Haskell.LSP.Types as LSP



toDiagnostics :: DynFlags -> ErrorMessages -> [Diagnostic]
toDiagnostics dflags = mapMaybe (mkDiag dflags $ T.pack "Compiler") . bagToList


mkDiag :: DynFlags -> T.Text -> ErrMsg -> Maybe Diagnostic
mkDiag dflags src e =
  case toDSeverity $ errMsgSeverity e of
    Nothing        -> Nothing
    Just bSeverity ->
      Just $ set dLocation (Just $ srcSpanToLocation $ errMsgSpan e)
        Diagnostic
        { _range    = srcSpanToRange $ errMsgSpan e
        , _severity = Just bSeverity
        , _source   = Just src
        , _message  = T.pack $ Out.showSDoc dflags (ErrUtils.pprLocErrMsg e)
        , _code     = Nothing
        , _relatedInformation = Nothing
        }

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
  Location (LSP.filePathToUri $ srcSpanToFilename src) (srcSpanToRange src)

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
mkErrors :: DynFlags -> [(SrcSpan, String)] -> [Diagnostic]
mkErrors dflags = concatMap (uncurry $ mkError dflags)

-- | Produce a GHC-style error from a source span and a message.
mkError :: DynFlags -> SrcSpan -> String -> [Diagnostic]
mkError dflags sp = toDiagnostics dflags . Bag.listToBag . pure . mkPlainErrMsg dflags sp . Out.text

-- | Produce a GHC-style error from a source span and a message.
mkErrorDoc :: DynFlags -> SrcSpan -> Out.SDoc -> [Diagnostic]
mkErrorDoc dflags sp = toDiagnostics dflags . Bag.listToBag . pure . mkPlainErrMsg dflags sp


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


mkErrorsGhcException :: DynFlags -> GhcException -> [Diagnostic]
mkErrorsGhcException dflags exc = mkErrors dflags [(noSpan "<Internal>", showGHCE dflags exc)]

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
