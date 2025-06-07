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
import           Distribution.PackageDescription.Parsec       (parseGenericPackageDescription)
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import           Distribution.Types.Version                   (Version)
import qualified Ide.Plugin.Cabal.Diagnostics                 as Diagnostics

import qualified Data.Text                                    as T
import           Development.IDE
import qualified Distribution.Fields.Parser                   as Syntax
import qualified Distribution.Parsec.Position                 as Syntax


parseCabalFileContents
  :: BS.ByteString -- ^ UTF-8 encoded bytestring
  -> ([PWarning], Either (Maybe Version, NonEmpty PError) GenericPackageDescription)
parseCabalFileContents bs =
  runParseResult (parseGenericPackageDescription bs)

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
