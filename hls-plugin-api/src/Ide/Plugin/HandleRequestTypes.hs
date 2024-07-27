{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.HandleRequestTypes where

import           Data.Text
import           Prettyprinter

-- | Reasons why a plugin could reject a specific request.
data RejectionReason =
  -- | The resolve request is not meant for this plugin or handler. The text
  -- field should contain the identifier for the plugin who owns this resolve
  -- request.
  NotResolveOwner Text
  -- | The plugin is disabled globally in the users config.
  | DisabledGlobally
  -- | The feature in the plugin that responds to this request is disabled in
  -- the users config
  | FeatureDisabled
  -- | This plugin is not the formatting provider selected in the users config.
  -- The text should be the formatting provider in your config.
  | NotFormattingProvider Text
  -- | This plugin does not support the file type. The text field here should
  -- contain the filetype of the rejected request.
  | DoesNotSupportFileType Text
  deriving (Eq)

-- | Whether a plugin will handle a request or not.
data HandleRequestResult = HandlesRequest | DoesNotHandleRequest RejectionReason
  deriving (Eq)

instance Pretty HandleRequestResult where
  pretty HandlesRequest                = "handles this request"
  pretty (DoesNotHandleRequest reason) = pretty reason

instance Pretty RejectionReason where
  pretty (NotResolveOwner s) = "does not handle resolve requests for " <> pretty s <> ")."
  pretty DisabledGlobally = "is disabled globally in your config."
  pretty FeatureDisabled = "'s feature that handles this request is disabled in your config."
  pretty (NotFormattingProvider s) = "is not the formatting provider ("<> pretty s<>") you chose in your config."
  pretty (DoesNotSupportFileType s) = "does not support " <> pretty s <> " filetypes)."

-- We always want to keep the leftmost disabled reason
instance Semigroup HandleRequestResult where
  HandlesRequest <> HandlesRequest = HandlesRequest
  DoesNotHandleRequest r <> _      = DoesNotHandleRequest r
  _ <> DoesNotHandleRequest r      = DoesNotHandleRequest r
