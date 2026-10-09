{-# LANGUAGE TemplateHaskell #-}
module A(th_a) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad.IO.Class

th_a :: DecsQ
th_a = do
  str <- makeRelativeToProject "wdtest" >>= liftIO . readFile
  [d| a = $(lift str) |]
