{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.Retrie.Orphans
  () where
import           Control.DeepSeq             (NFData (..), rwhnf)
import           Data.Aeson                  (FromJSON (..), ToJSON (..))
import           GHC.Generics                (Generic)
import           Retrie.ExactPrint.Annotated
import           Retrie.Rewrites

deriving instance Eq RewriteSpec

deriving instance Show RewriteSpec

deriving instance Generic RewriteSpec

deriving instance FromJSON RewriteSpec

deriving instance ToJSON RewriteSpec

instance Show (Annotated ast) where show _ = "<Annotated>"

instance NFData (Annotated ast) where rnf = rwhnf
