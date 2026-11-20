{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.Retrie.Log
  ( Log(..)
  ) where

import           Development.IDE

data Log
  = forall a. Pretty a => LogResolve a
  | forall a. Pretty a => LogShake a

instance Pretty Log where
  pretty = \case
    LogResolve l -> pretty l
    LogShake l   -> pretty l
