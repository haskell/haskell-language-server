{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.Retrie.Orphans
  () where
import           Data.Aeson      (FromJSON (..), ToJSON (..))
import           GHC.Generics    (Generic)
import           Retrie.Rewrites

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec
