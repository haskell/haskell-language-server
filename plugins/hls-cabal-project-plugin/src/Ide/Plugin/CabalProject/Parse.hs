{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Parse
  ( parseCabalProjectFileContents
  ) where

-- base -----------------------------------------------------------------------
import           Control.Monad                            (unless)
import qualified Data.ByteString                          as BS
import           Data.List.NonEmpty                       (NonEmpty (..))
import           Distribution.Client.HttpUtils            (configureTransport)
import           Distribution.Client.ProjectConfig.Parsec (ProjectConfigSkeleton,
                                                           parseProject)
import           Distribution.Client.ProjectConfig.Types  (ProjectConfigToParse (..))
import           Distribution.Fields                      (PError (..),
                                                           PWarning (..))
import           Distribution.Fields.ParseResult          (ParseResult,
                                                           runParseResult)
import           Distribution.Types.Version               (Version)
import           Distribution.Verbosity                   (normal)
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

  parseRes :: ParseResult ProjectConfigSkeleton
    <- parseProject rootDir fp httpTransport verb toParse

  pure (runParseResult parseRes)

-- parseCabalProjectContents :: FilePath -> IO (Either String (Project Void String String))
-- parseCabalProjectContents file = do
--   contents <- BS.readFile file
--   case parseProject file contents of
--     Left parseErr ->
--       pure $ Left ("Parse error in " ++ file ++ ": " ++ show parseErr)
--     Right project ->
--       pure $ Right project
