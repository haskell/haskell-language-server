{-# LANGUAGE TypeFamilies #-}

module Ide.Plugin.CabalProject.Types where

import           Control.DeepSeq                          (NFData)
import           Data.Hashable                            (Hashable)
import           Development.IDE                          (RuleResult)
import           Distribution.Client.ProjectConfig.Parsec (ProjectConfigSkeleton)
import qualified Distribution.Fields                      as Syntax
import qualified Distribution.Parsec.Position             as Syntax
import           GHC.Generics                             (Generic)

type instance RuleResult ParseCabalProjectFile = ProjectConfigSkeleton

data ParseCabalProjectFile = ParseCabalProjectFile
  deriving (Eq, Show, Generic)

instance Hashable ParseCabalProjectFile

instance NFData ParseCabalProjectFile

type instance RuleResult ParseCabalProjectFields = [Syntax.Field Syntax.Position]

data ParseCabalProjectFields = ParseCabalProjectFields
  deriving (Eq, Show, Generic)

instance Hashable ParseCabalProjectFields

instance NFData  ParseCabalProjectFields

