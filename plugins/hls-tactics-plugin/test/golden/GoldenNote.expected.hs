note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just a) = Right a
