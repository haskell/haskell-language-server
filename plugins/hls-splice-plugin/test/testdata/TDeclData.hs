{-# LANGUAGE TemplateHaskell #-}
module TDeclData where
import Language.Haskell.TH

$(pure <$> dataD (pure []) (mkName "MyData") [] Nothing
    [ normalC (mkName "MyConA") [bangType (bang noSourceUnpackedness noSourceStrictness) [t|Int|]]
    , normalC (mkName "MyConB") [bangType (bang noSourceUnpackedness noSourceStrictness) [t|String|]]
    ]
    [derivClause Nothing [conT ''Show, conT ''Eq]])
