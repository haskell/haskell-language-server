{-# LANGUAGE TemplateHaskell #-}
module TDeclPragma where
import Language.Haskell.TH

$(sequence
    [ sigD (mkName "myId") [t|Int -> Int|]
    , pragInlD (mkName "myId") Inline FunLike AllPhases
    , funD (mkName "myId") [clause [varP (mkName "x")] (normalB (varE (mkName "x"))) []]
    ])
