{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.CabalProject.Orphans where

import           Control.DeepSeq
import           Distribution.Fields.Field
import           Distribution.Parsec.Position
-- import Control.DeepSeq (NFData)
import qualified Distribution.Solver.Types.ProjectConfigPath as PCPath
import           GHC.Generics                                (Generic)

import qualified Distribution.Client.ProjectConfig.Types     as PC
import           Ide.Plugin.Cabal.Orphans                    ()

-- Project Config Orphans

deriving instance NFData PCPath.ProjectConfigPath

instance NFData PC.ProjectConfig where
  rnf !_ = ()

