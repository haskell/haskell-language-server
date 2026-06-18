module AddExportNegatives (placeholder) where

placeholder :: Int
placeholder = 0

withWhere :: ()
withWhere = whereBound
  where whereBound = ()

data Rec = Rec { recField :: () }
