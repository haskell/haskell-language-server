{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.InlayHints.Types(InlayHintLog(LogShake)) where

import           Development.IDE            (Pretty (pretty))
import qualified Development.IDE.Core.Shake as Shake

newtype InlayHintLog = LogShake Shake.Log

instance Pretty InlayHintLog where
    pretty = \case
        LogShake log -> pretty log

