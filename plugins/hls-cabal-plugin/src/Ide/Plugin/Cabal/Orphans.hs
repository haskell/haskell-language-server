{-# OPTIONS_GHC -Wno-orphans #-}
module Ide.Plugin.Cabal.Orphans where
import           Control.DeepSeq
import           Distribution.Fields.Field
import           Distribution.Parsec.Position

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
