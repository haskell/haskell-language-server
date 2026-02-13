{-# LANGUAGE TemplateHaskell #-}

module TH where

import Language.Haskell.TH

data MyRec
  = MyRec
  { foo :: Char
  , bar :: Int
  }

mkVal :: Q [Dec]
mkVal = do
  x <- newName "x"
  pure $
    [ ValD
        (VarP x)
        ( NormalB
            ( AppE
                (AppE (ConE 'MyRec) (LitE (CharL 'c')))
                (LitE (IntegerL 42))
            )
        )
        []
    ]
