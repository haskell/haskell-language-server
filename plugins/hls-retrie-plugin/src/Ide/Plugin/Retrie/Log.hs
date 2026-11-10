{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Retrie.Log
  ( Log(..)
  ) where

import           Development.IDE

data Log
  = LogParsingModule FilePath
  | forall a. Pretty a => LogResolve a
  | forall a. Pretty a => LogShake a

instance Pretty Log where
  pretty = \case
    LogParsingModule fp -> "Parsing module:" <+> pretty fp
    LogResolve l        -> pretty l
    LogShake l          -> pretty l
