{-# LANGUAGE OverloadedStrings     #-}

module Ide.Plugin.Cabal.Dependencies (
    DependencyInstance(..),
    DependencyInstances(..),
    PositionedDependency(..),
    parseDeps,
    planJsonPath
) where

import Distribution.Fields qualified as Syntax
import Distribution.Parsec.Position qualified as Syntax

import Data.Text.Encoding qualified as Encoding
import Data.Text qualified as T
import System.FilePath ((</>), (<.>))

import Text.Regex.TDFA ((=~), AllTextMatches (getAllTextMatches))
import Data.ByteString (ByteString)

import Ide.Plugin.Cabal.Completion.Types
    
planJsonPath :: FilePath
planJsonPath = "dist-newstyle" </> "cache" </> "plan" <.> "json" -- hard coded for now
    
-- | Parses a Field that may contain dependencies
parseDeps :: Syntax.Field Syntax.Position -> [PositionedDependency]
parseDeps (Syntax.Field (Syntax.Name _ "build-depends") fls) = concatMap mkPosDeps fls
parseDeps (Syntax.Section _ _ fls) = concatMap parseDeps fls 
parseDeps _ = []

-- | Matches valid Cabal dependency names 
packageRegex :: T.Text
packageRegex = ".+"

-- | Parses a single FieldLine of Cabal dependencies. Returns a list since a single line may
-- contain multiple dependencies.
mkPosDeps :: Syntax.FieldLine Syntax.Position -> [PositionedDependency]
mkPosDeps (Syntax.FieldLine pos dep) = map (PositionedDependency pos) $ getPackageNames dep
    where 
        getPackageNames :: ByteString -> [T.Text]
        getPackageNames dep = getAllTextMatches (Encoding.decodeUtf8Lenient dep =~ packageRegex)