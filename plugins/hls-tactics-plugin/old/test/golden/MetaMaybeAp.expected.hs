maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp Nothing Nothing = Nothing
maybeAp Nothing (Just _) = Nothing
maybeAp (Just _) Nothing = Nothing
maybeAp (Just fab) (Just a) = Just (fab a)
