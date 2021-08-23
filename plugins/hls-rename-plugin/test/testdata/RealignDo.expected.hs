fooBarQuux :: Maybe Integer
fooBarQuux = do x <- Just 5
                t <- Just 10
                pure $ x + t
