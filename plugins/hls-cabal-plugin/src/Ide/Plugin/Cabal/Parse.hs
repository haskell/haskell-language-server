{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Parse
( parseCabalFileContents
, readCabalFields
) where

import qualified Data.ByteString                              as BS
import           Data.List.NonEmpty                           (NonEmpty (..))
import           Distribution.Fields                          (PError (..),
                                                               PWarning (..))
import           Distribution.Fields.ParseResult              (runParseResult)
import           Distribution.PackageDescription              (PackageDescription (..))
import           Distribution.PackageDescription.Parsec       (parseGenericPackageDescription)
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import           Distribution.Types.Version                   (Version)
import qualified Ide.Plugin.Cabal.Diagnostics                 as Diagnostics

import qualified Data.List                                    as L
import qualified Data.List.NonEmpty                           as NE
import qualified Data.Text                                    as T
import           Development.IDE
import qualified Distribution.Fields.Parser                   as Syntax
import qualified Distribution.Parsec.Position                 as Syntax

#if MIN_VERSION_Cabal_syntax(3,17,0)
import           Distribution.Fields.ParseResult              (withSource)
import           Distribution.Parsec                          (PErrorWithSource,
                                                               PWarningWithSource,
                                                               showPErrorWithSource)
import           Distribution.Parsec.Source                   (CabalFileSource (..),
                                                               renderCabalFileSource)
#else
import           Distribution.Parsec                          (showPError)
#endif

parseCabalFileContents
  :: FilePath
  -> BS.ByteString -- ^ UTF-8 encoded bytestring
#if MIN_VERSION_Cabal_syntax(3,17,0)
  -> IO ([PWarningWithSource CabalFileSource],
         Either (Maybe Version, NonEmpty (PErrorWithSource CabalFileSource))
                GenericPackageDescription)
#else
  -> IO ([PWarning],
         Either (Maybe Version, NonEmpty PError)
                GenericPackageDescription)
#endif
parseCabalFileContents fp bs =
    pure $
    case runParseResult $
#if MIN_VERSION_Cabal_syntax(3,17,0)
      withSource (PCabalFile (fp, bs)) $
#endif
      parseGenericPackageDescription bs of
#if MIN_VERSION_Cabal_syntax(3,17,0)
      (warnings, Left (mbVer, errs)) ->
        (warnings, Left (mbVer, errs))
      (warnings, Right gpd) ->
        (warnings, Right gpd)
#else
      (warnings, Left errs) ->
        (warnings, Left (Nothing, errs))
      (warnings, Right gpd) ->
        (warnings, Right gpd)
#endif

readCabalFields ::
  NormalizedFilePath ->
  BS.ByteString ->
  Either FileDiagnostic [Syntax.Field Syntax.Position]
readCabalFields file contents  = do
  case Syntax.readFields' contents of
    Left parseError ->
      Left $ Diagnostics.fatalParseErrorDiagnostic file
           $ "Failed to parse cabal file: " <> T.pack (show parseError)
    Right (fields, _warnings) -> do
      -- we don't want to double report diagnostics, all diagnostics are produced by 'ParseCabalFile'.
      Right fields
