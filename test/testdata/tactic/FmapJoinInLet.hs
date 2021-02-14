{-# LANGUAGE ScopedTypeVariables #-}

fJoin :: forall f m a. (Monad m, Monad f) => f (m (m a)) -> f (m a)
fJoin =  let f = (_ :: m (m a) -> m a) in fmap f
