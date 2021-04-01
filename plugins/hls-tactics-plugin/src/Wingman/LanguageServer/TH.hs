module Wingman.LanguageServer.TH where

import Data.Aeson.Types

lspOptions :: Options
lspOptions = defaultOptions { omitNothingFields = True, fieldLabelModifier = modifier }
  where
  modifier :: String -> String
  -- For fields called data and type in the spec, we call them xdata and xtype
  -- in haskell-lsp-types to avoid it clashing with the Haskell keywords. This
  -- fixes up the json derivation
  modifier "_xdata" = "data"
  modifier "_xtype" = "type"
  modifier xs = drop 1 xs

