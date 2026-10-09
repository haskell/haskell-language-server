{-# LANGUAGE TemplateHaskell #-}

module TH where

import THDef

gen ''Bool True
gen ''Char 'a'
