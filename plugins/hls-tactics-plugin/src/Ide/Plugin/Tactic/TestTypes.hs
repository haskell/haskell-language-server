{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.Tactic.TestTypes where

import           Data.Aeson
import           Data.Maybe (fromMaybe)
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
  | DestructAll
  | UseDataCon
  | Refine
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle Auto _                   = "Attempt to fill hole"
tacticTitle Intros _                 = "Introduce lambda"
tacticTitle Destruct var             = "Case split on " <> var
tacticTitle Homomorphism var         = "Homomorphic case split on " <> var
tacticTitle DestructLambdaCase _     = "Lambda case split"
tacticTitle HomomorphismLambdaCase _ = "Homomorphic lambda case split"
tacticTitle DestructAll _            = "Split all function arguments"
tacticTitle UseDataCon dcon          = "Use constructor " <> dcon
tacticTitle Refine _                 = "Refine hole"


------------------------------------------------------------------------------
-- | Plugin configuration for tactics
data Config = Config
  { cfg_feature_set          :: FeatureSet
  , cfg_max_use_ctor_actions :: Int
  }

emptyConfig :: Config
emptyConfig = Config defaultFeatures 5

instance ToJSON Config where
  toJSON Config{..} = object
    [ "features" .= prettyFeatureSet cfg_feature_set
    , "max_use_ctor_actions" .= cfg_max_use_ctor_actions
    ]

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> do
    cfg_feature_set          <-
      parseFeatureSet . fromMaybe "" <$> obj .:? "features"
    cfg_max_use_ctor_actions <-
      fromMaybe 5 <$> obj .:? "max_use_ctor_actions"
    pure $ Config{..}

