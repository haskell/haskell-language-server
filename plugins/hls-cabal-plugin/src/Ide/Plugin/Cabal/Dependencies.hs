{-# LANGUAGE OverloadedStrings     #-}

module Ide.Plugin.Cabal.Dependencies where

import Distribution.Fields qualified as Syntax
import Distribution.Parsec.Position qualified as Syntax

import Data.Text.Encoding qualified as Encoding
import Data.Text qualified as T
import Data.Aeson qualified as A
import Data.Aeson ((.:))

import Text.Regex.TDFA ((=~), AllTextMatches (getAllTextMatches))
import Data.ByteString (ByteString)

data DependencyInstances = DependencyInstances 
    { installPlan :: [DependencyInstance] }

data DependencyInstance = DependencyInstance 
    { _pkgName :: T.Text
    , _pkgVersion :: T.Text 
    }
    
instance A.FromJSON DependencyInstance where
    parseJSON = A.withObject "InstallPlan" $ \obj -> do
        pkgName <- obj .: "pkg-name"
        pkgVersion <- obj .: "pkg-version"
        return $ DependencyInstance pkgName pkgVersion

instance A.FromJSON DependencyInstances where
  parseJSON = A.withObject "PlanJson" $ \obj -> do
    deps <- obj .: "install-plan" >>= A.parseJSON
    return (DependencyInstances deps) 

data PositionedDependency = PositionedDependency Syntax.Position T.Text
    deriving Show

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
    
-- | Parses a Field that may contain dependencies
parseDeps :: Syntax.Field Syntax.Position -> [PositionedDependency]
parseDeps (Syntax.Field (Syntax.Name _ "build-depends") fls) = concatMap mkPosDeps fls
parseDeps (Syntax.Section _ _ fls) = concatMap parseDeps fls 
parseDeps _ = []