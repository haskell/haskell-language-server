{-# LANGUAGE OverloadedStrings     #-}

module Ide.Plugin.Cabal.Dependencies (
    DependencyInstance(..),
    DependencyInstances(..),
    parseDeps,
    planJsonPath,
    packageRegex
) where

import Distribution.Fields qualified as Syntax
import Distribution.Parsec.Position qualified as Syntax

import Data.Text.Encoding qualified as Encoding
import Data.Text qualified as T
import System.FilePath ((</>), (<.>))

import Text.Regex.TDFA ((=~), AllTextMatches (getAllTextMatches), AllMatches(getAllMatches))
import Data.ByteString (ByteString)

import Ide.Plugin.Cabal.Completion.Types
import Debug.Trace
import Data.Tuple.Extra (dupe)

planJsonPath :: FilePath
planJsonPath = "dist-newstyle" </> "cache" </> "plan" <.> "json" -- hard coded for now

-- | Parses a Field that may contain dependencies
parseDeps :: Syntax.Field Syntax.Position -> [Positioned PkgName]
parseDeps (Syntax.Field (Syntax.Name _ "build-depends") fls) = concatMap mkPosDeps fls
parseDeps (Syntax.Section _ _ fls) = concatMap parseDeps fls
parseDeps _ = []

-- | Matches valid Cabal dependency names 
packageRegex :: T.Text
packageRegex = "[a-zA-Z0-9_-]+" -- not sure if this is correct

-- | Parses a single FieldLine of Cabal dependencies. Returns a list since a single line may
-- contain multiple dependencies.
mkPosDeps :: Syntax.FieldLine Syntax.Position -> [Positioned PkgName]
mkPosDeps (Syntax.FieldLine pos dep) = zipWith 
        (\n (o, _) -> Positioned (Syntax.Position (Syntax.positionRow pos) (Syntax.positionCol pos + o + 1)) n) 
        (getPackageNames dep) 
        (getPackageNameOffsets dep)
    where
        getPackageNames :: ByteString -> [T.Text]
        getPackageNames dep = getAllTextMatches (Encoding.decodeUtf8Lenient dep =~ packageRegex)

        getPackageNameOffsets :: ByteString -> [(Int, Int)]
        getPackageNameOffsets dep = getAllMatches (Encoding.decodeUtf8Lenient dep =~ packageRegex)
