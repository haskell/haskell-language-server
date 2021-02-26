{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.TestTypes where

import           Data.Aeson
import qualified Data.Text                    as T
import           Ide.Plugin.Tactic.FeatureSet

------------------------------------------------------------------------------
-- | The list of tactics exposed to the outside world. These are attached to
-- actual tactics via 'commandTactic' and are contextually provided to the
-- editor via 'commandProvider'.
data TacticCommand
  = Auto
  | Intros
  | Destruct
  | Homomorphism
  | DestructLambdaCase
  | HomomorphismLambdaCase
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle Auto _                   = "Attempt to fill hole"
tacticTitle Intros _                 = "Introduce lambda"
tacticTitle Destruct var             = "Case split on " <> var
tacticTitle Homomorphism var         = "Homomorphic case split on " <> var
tacticTitle DestructLambdaCase _     = "Lambda case split"
tacticTitle HomomorphismLambdaCase _ = "Homomorphic lambda case split"


------------------------------------------------------------------------------
-- | Plugin configuration for tactics
newtype Config = Config
  { cfg_feature_set :: FeatureSet
  }

emptyConfig :: Config
emptyConfig = Config defaultFeatures

instance ToJSON Config where
  toJSON (Config features) = object
    [ "features" .= prettyFeatureSet features
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> do
    features <- parseFeatureSet <$> obj .: "features"
    pure $ Config features

