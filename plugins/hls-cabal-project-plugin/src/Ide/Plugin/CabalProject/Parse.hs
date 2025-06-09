{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Parse
  ( parseCabalProjectContents
  ) where

import           Data.Void                                    (Void)

-- cabal-install-parsers 0.6 modules -----------------------------
import           Cabal.Parse                                  (ParseError)
import           Cabal.Project                                (Project,
                                                               parseProject)

-- error type lives in Cabal-syntax
-- import           Distribution.Parsec.Error           (ParseError)

import           Distribution.Types.GenericPackageDescription (GenericPackageDescription)

import qualified Data.ByteString                              as BS
-- import Distribution.Parsec.Project (parseProject)
-- import Distribution.Parsec.Common (ParseError)
import           Data.List.NonEmpty                           (NonEmpty)
import           Data.Text                                    (pack)

parseCabalProjectContents :: FilePath -> IO (Either String (Project Void String String))
parseCabalProjectContents file = do
  contents <- BS.readFile file
  case parseProject file contents of
    Left parseErr ->
      pure $ Left ("Parse error in " ++ file ++ ": " ++ show parseErr)
    Right project ->
      pure $ Right project
