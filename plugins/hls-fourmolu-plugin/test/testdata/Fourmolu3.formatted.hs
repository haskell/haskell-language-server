b :: Bool
b =
    id $
        id $
            case True && True of
                True -> True
                False -> False
