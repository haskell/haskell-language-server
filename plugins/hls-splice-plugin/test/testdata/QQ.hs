{-# LANGUAGE TemplateHaskell #-}
module QQ (str) where

import Language.Haskell.TH
    ( mkName,
      stringL,
      litP,
      clause,
      litE,
      normalB,
      funD,
      sigD,
      litT,
      strTyLit )
import Language.Haskell.TH.Quote (QuasiQuoter (..))

str :: QuasiQuoter
str =
    QuasiQuoter
        { quoteExp = litE . stringL
        , quotePat = litP . stringL
        , quoteType = litT . strTyLit
        , quoteDec = \name ->
            sequence
                [ sigD (mkName name) [t|String|]
                , funD (mkName name) [clause [] (normalB $ litE $ stringL name) []]
                ]
        }
