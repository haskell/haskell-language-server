{-# LANGUAGE TemplateHaskell #-}
module TDeclNewtype where
import Language.Haskell.TH

$(pure <$> newtypeD (pure []) (mkName "MyNewtype") [] Nothing
    (normalC (mkName "MkMyNewtype") [bangType (bang noSourceUnpackedness noSourceStrictness) [t|Int|]])
    [derivClause Nothing [conT ''Show]])
