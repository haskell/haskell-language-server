data Gen a

data Obj = Obj Int Bool Char String

arbitrary :: Gen Obj
arbitrary
  = (((Obj <$> arbitrary) <*> arbitrary) <*> arbitrary) <*> arbitrary