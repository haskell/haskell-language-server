module TypeSyn where

type UserId = Int

lookupUser :: UserId -> Maybe String
lookupUser n = Nothing

mkId :: Int -> UserId
mkId = id
