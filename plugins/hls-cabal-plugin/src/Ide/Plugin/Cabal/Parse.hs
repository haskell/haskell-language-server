module Ide.Plugin.Cabal.Parse
( parseCabalFile
, parseCabalFileContents
  -- * Re-exports
, FilePath
, NonEmpty(..)
, PWarning(..)
, Version
, PError(..)
, Position(..)
, GenericPackageDescription(..)
) where

import           Control.Monad                                (unless)
import qualified Data.ByteString                              as BS
import           Data.List.NonEmpty                           (NonEmpty (..))
import           Distribution.Fields                          (PError (..),
                                                               PWarning (..))
import           Distribution.Fields.ParseResult              (runParseResult)
import           Distribution.PackageDescription.Parsec       (parseGenericPackageDescription)
import           Distribution.Parsec.Position                 (Position (..))
import           Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import           Distribution.Types.Version                   (Version)
import qualified System.Directory                             as Dir
import qualified System.Exit                                  as Exit


parseCabalFile
    :: FilePath
    -> IO ([PWarning], Either (Maybe Version, NonEmpty PError) GenericPackageDescription)
parseCabalFile =
    readAndParseFile'
  where
    readAndParseFile' fpath = do
        exists <- Dir.doesFileExist fpath
        unless exists $
            Exit.die $
                "Error Parsing: file \"" ++ fpath ++ "\" doesn't exist. Cannot continue."
        bs <- BS.readFile fpath
        parseCabalFileContents bs

parseCabalFileContents
    :: BS.ByteString -- ^ UTF-8 encoded bytestring
    -> IO ([PWarning], Either (Maybe Version, NonEmpty PError) GenericPackageDescription)
parseCabalFileContents bs =
    pure $ runParseResult (parseGenericPackageDescription bs)
