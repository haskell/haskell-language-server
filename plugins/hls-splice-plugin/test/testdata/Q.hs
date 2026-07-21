{-# LANGUAGE TemplateHaskellQuotes #-}

module Q where

import Data.Maybe
import Data.Ord
import Data.String
import Language.Haskell.TH

q :: Q [Dec]
q = do
    let dName = mkName "D"
    dataDec <-
        dataD
            (pure [])
            dName
            []
            Nothing
            [normalC dName []]
            []
    isStringDec <-
        instanceD
            (pure [])
            (appT (conT ''IsString) (conT dName))
            [ funD
                'fromString
                [ clause
                    [varP (mkName "s")]
                    ( normalB
                        ( appE
                            (appE (varE 'fromMaybe) (conE dName))
                            ( infixE
                                (Just (conE dName))
                                (varE '(<$))
                                ( Just
                                    (appE (conE 'Just) (appE (conE 'Down) (litE (integerL 1))))
                                )
                            )
                        )
                    )
                    []
                ]
            ]
    pure [dataDec, isStringDec]
