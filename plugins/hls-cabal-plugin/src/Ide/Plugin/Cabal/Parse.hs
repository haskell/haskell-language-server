module Ide.Plugin.Cabal.Parse
( parseCabalFileContents
  -- * Re-exports
, FilePath
, NonEmpty(..)
, PWarning(..)
, Version
, PError(..)
, Position(..)
, GenericPackageDescription(..)
) where

import qualified Data.ByteString                              as BS
import           Data.List.NonEmpty                           (NonEmpty (..))
import           Distribution.Fields                          (PError (..),
                                                               PWarning (..))
import           Distribution.Fields.ParseResult              (runParseResult)
import           Distribution.PackageDescription.Parsec       (parseGenericPackageDescription)
import           Distribution.Parsec.Position                 (Position (..))
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import           Distribution.Types.Version                   (Version)

parseCabalFileContents
  :: BS.ByteString -- ^ UTF-8 encoded bytestring
  -> IO ([PWarning], Either (Maybe Version, NonEmpty PError) GenericPackageDescription)
parseCabalFileContents bs =
  pure $ runParseResult (parseGenericPackageDescription bs)
