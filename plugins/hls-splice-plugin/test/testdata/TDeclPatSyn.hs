{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module TDeclPatSyn where
import Language.Haskell.TH

$(pure <$> patSynD (mkName "MyPattern") (prefixPatSyn []) unidir (litP (integerL 42)))
