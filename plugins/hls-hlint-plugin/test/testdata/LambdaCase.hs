{-# LANGUAGE LambdaCase #-}
module LambdaCase where

f = \case "true" -> (True)
          _ -> False
