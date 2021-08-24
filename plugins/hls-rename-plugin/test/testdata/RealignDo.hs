foo :: Maybe Integer
foo = do x <- Just 5
         t <- Just 10
         pure $ x + t
