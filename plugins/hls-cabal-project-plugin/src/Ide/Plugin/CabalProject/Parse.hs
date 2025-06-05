{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Parse
  ( parseCabalProjectFileContents,
    readCabalProjectFields
  ) where

import qualified Crypto.Hash.SHA1                         as H
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Base16                   as B16
import qualified Data.ByteString.Char8                    as B
import           Data.List.NonEmpty                       (NonEmpty (..))
import qualified Data.List.NonEmpty                       as NE
import qualified Data.Text                                as T
import           Development.IDE
import           Distribution.Client.HttpUtils            (configureTransport)
import           Distribution.Client.ProjectConfig.Parsec (ProjectConfigSkeleton,
                                                           parseProject,
                                                           readPreprocessFields)
import           Distribution.Client.ProjectConfig.Types  (ProjectConfigToParse (..))
import           Distribution.Fields                      (PError (..),
                                                           PWarning (..))
import qualified Distribution.Fields.Parser               as Syntax
import qualified Distribution.Fields.ParseResult          as PR
import qualified Distribution.Parsec.Position             as Syntax
import           Distribution.Types.Version               (Version)
import           Distribution.Verbosity                   (normal)
import qualified Ide.Plugin.CabalProject.Diagnostics      as Diagnostics
import           System.Directory.Extra                   (XdgDirectory (..),
                                                           getXdgDirectory)
import           System.FilePath                          (takeBaseName,
                                                           takeDirectory, (</>))

parseCabalProjectFileContents
  :: FilePath
  -> BS.ByteString
  -> IO ([PWarning]
         , Either (Maybe Version, NonEmpty PError) ProjectConfigSkeleton)
parseCabalProjectFileContents fp bytes = do
  cacheDir <- getCabalProjectCacheDir fp
  let toParse = ProjectConfigToParse bytes
      verb    = normal
  httpTransport <- configureTransport verb [fp] Nothing

  parseRes :: PR.ParseResult ProjectConfigSkeleton
    <- parseProject fp cacheDir httpTransport verb toParse

  pure (PR.runParseResult parseRes)

readCabalProjectFields
  :: NormalizedFilePath
  -> BS.ByteString
  -> Either FileDiagnostic [Syntax.Field Syntax.Position]
readCabalProjectFields file contents =
  case PR.runParseResult (readPreprocessFields contents) of
    (_warnings, Left (_mbVer, errs)) ->
      let perr = NE.head errs
       in Left $
            Diagnostics.fatalParseErrorDiagnostic file
              ("Failed to parse cabal.project file: " <> T.pack (show perr))

    (_warnings, Right fields) ->
      Right fields

getCabalProjectCacheDir :: FilePath -> IO FilePath
getCabalProjectCacheDir fp = do
    getXdgDirectory XdgCache (cacheDir </> prefix ++ "-" ++ opts_hash)
    where
        prefix = takeBaseName $ takeDirectory fp
        -- Create a unique folder per cabal.project file
        opts_hash = B.unpack $ B16.encode $ H.finalize $ H.updates H.init [B.pack fp]

cacheDir :: String
cacheDir = "ghcide"
