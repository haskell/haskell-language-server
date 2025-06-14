{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Parse
  ( parseCabalProjectFileContents,
    readCabalProjectFields
  ) where

-- base -----------------------------------------------------------------------
import           Control.Monad                            (unless)
import qualified Data.ByteString                          as BS
import           Data.List.NonEmpty                       (NonEmpty (..))
import           Distribution.Client.HttpUtils            (configureTransport)
import           Distribution.Client.ProjectConfig.Parsec (ProjectConfigSkeleton,
                                                           parseProject,
                                                           readPreprocessFields)
import           Distribution.Client.ProjectConfig.Types  (ProjectConfigToParse (..))
import           Distribution.Fields                      (PError (..),
                                                           PWarning (..))
import qualified Distribution.Fields.ParseResult          as PR
-- import           Distribution.Fields.ParseResult          (ParseResult,
--                                                            runParseResult)
import qualified Data.List.NonEmpty                       as NE
import qualified Data.Text                                as T
import           Development.IDE
import qualified Distribution.Fields.Parser               as Syntax
import qualified Distribution.Parsec.Position             as Syntax
import           Distribution.Types.Version               (Version)
import           Distribution.Verbosity                   (normal)
import qualified Ide.Plugin.CabalProject.Diagnostics      as Diagnostics
import           System.Directory                         (doesFileExist)
import           System.FilePath                          (takeDirectory)

parseCabalProjectFileContents
  :: FilePath
  -> IO ([PWarning]
         , Either (Maybe Version, NonEmpty PError) ProjectConfigSkeleton)
parseCabalProjectFileContents fp = do
  bytes <- BS.readFile fp
  let toParse = ProjectConfigToParse bytes
      rootDir = takeDirectory fp
      verb    = normal
  httpTransport <- configureTransport verb [fp] Nothing

  parseRes :: PR.ParseResult ProjectConfigSkeleton
    <- parseProject rootDir fp httpTransport verb toParse

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

-- parseCabalProjectContents :: FilePath -> IO (Either String (Project Void String String))
-- parseCabalProjectContents file = do
--   contents <- BS.readFile file
--   case parseProject file contents of
--     Left parseErr ->
--       pure $ Left ("Parse error in " ++ file ++ ": " ++ show parseErr)
--     Right project ->
--       pure $ Right project
