{-# LANGUAGE OverloadedStrings #-}

module Wingman.FeatureSet
  ( Feature (..)
  , FeatureSet
  , hasFeature
  , defaultFeatures
  , allFeatures
  , parseFeatureSet
  , prettyFeatureSet
  ) where

import           Data.List  (intercalate)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as S
import qualified Data.Text  as T


------------------------------------------------------------------------------
-- | All the available features. A 'FeatureSet' describes the ones currently
-- available to the user.
data Feature
  = FeatureDestructAll
  | FeatureUseDataCon
  | FeatureRefineHole
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


------------------------------------------------------------------------------
-- | A collection of enabled features.
type FeatureSet = Set Feature


------------------------------------------------------------------------------
-- | Parse a feature set.
parseFeatureSet :: T.Text -> FeatureSet
parseFeatureSet
  = mappend defaultFeatures
  . S.fromList
  . mapMaybe (readMaybe . mappend featurePrefix . rot13 . T.unpack)
  . T.split (== '/')


------------------------------------------------------------------------------
-- | Features that are globally enabled for all users.
defaultFeatures :: FeatureSet
defaultFeatures = S.fromList
  [
  ]


------------------------------------------------------------------------------
-- | All available features.
allFeatures :: FeatureSet
allFeatures = S.fromList $ enumFromTo minBound maxBound


------------------------------------------------------------------------------
-- | Pretty print a feature set.
prettyFeatureSet :: FeatureSet -> String
prettyFeatureSet
  = intercalate "/"
  . fmap (rot13 . drop (length featurePrefix) . show)
  . S.toList


------------------------------------------------------------------------------
-- | Is a given 'Feature' currently enabled?
hasFeature :: Feature -> FeatureSet -> Bool
hasFeature = S.member


------------------------------------------------------------------------------
-- | Like 'read', but not partial.
readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads


featurePrefix :: String
featurePrefix = "Feature"


rot13 :: String -> String
rot13 = fmap (toEnum . rot13int . fromEnum)


rot13int :: Integral a => a -> a
rot13int x
  | (fromIntegral x :: Word) - 97 < 26 = 97 + rem (x - 84) 26
  | (fromIntegral x :: Word) - 65 < 26 = 65 + rem (x - 52) 26
  | otherwise   = x

