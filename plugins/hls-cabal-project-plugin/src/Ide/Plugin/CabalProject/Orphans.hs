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

-- ----------------------------------------------------------------
-- Cabal-syntax orphan instances we need sometimes
-- ----------------------------------------------------------------

instance NFData (Field Position) where
    rnf (Field name fieldLines) = rnf name `seq` rnf fieldLines
    rnf (Section name sectionArgs fields) =  rnf name `seq` rnf sectionArgs `seq` rnf fields

instance NFData (Name Position) where
    rnf (Name ann fName) = rnf ann `seq` rnf fName

instance NFData (FieldLine Position) where
    rnf (FieldLine ann bs) = rnf ann `seq` rnf bs

instance NFData (SectionArg Position) where
    rnf (SecArgName ann bs)  = rnf ann `seq` rnf bs
    rnf (SecArgStr ann bs)   = rnf ann `seq` rnf bs
    rnf (SecArgOther ann bs) = rnf ann `seq` rnf bs

-- Project Config Orphans

deriving instance NFData PCPath.ProjectConfigPath

instance NFData PC.ProjectConfig where
  rnf !_ = ()

