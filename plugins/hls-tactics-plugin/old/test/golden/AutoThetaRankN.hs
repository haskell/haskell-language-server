{-# LANGUAGE RankNTypes #-}

showMe :: (forall x. Show x => x -> String) -> Int -> String
showMe f = f

showedYou :: Int -> String
showedYou = showMe (\x -> _)

