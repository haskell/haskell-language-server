{-# LANGUAGE FlexibleContexts #-}

module Ide.Plugin.Tactic.Context where

import Ide.Plugin.Tactic.Types
import           Control.Monad.Reader

mkContext :: [(OccName, CType)] -> Context
mkContext = Context

getCurrentDefinitions :: MonadReader Context m => m [OccName]
getCurrentDefinitions = asks $ fmap fst . ctxDefiningFuncs

