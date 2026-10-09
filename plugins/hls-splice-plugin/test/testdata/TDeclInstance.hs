{-# LANGUAGE TemplateHaskell #-}
module TDeclInstance where
import Language.Haskell.TH

$(do dataDec <- dataD (pure []) (mkName "Wrapper") [] Nothing
       [normalC (mkName "MkWrapper") [bangType (bang noSourceUnpackedness noSourceStrictness) [t|Int|]]]
       []
     instDec <- instanceD (pure []) (appT (conT ''Show) (conT (mkName "Wrapper")))
       [ funD (mkName "show") [clause [conP (mkName "MkWrapper") [varP (mkName "n")]]
           (normalB [|"Wrapper:" ++ show n|]) []]
       ]
     pure [dataDec, instDec])
