{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module TDeclPatSyn where
import Language.Haskell.TH

pattern MyPattern <- 42
