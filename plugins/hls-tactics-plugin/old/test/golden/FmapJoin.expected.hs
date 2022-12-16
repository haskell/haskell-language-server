fJoin :: (Monad m, Monad f) => f (m (m a)) -> f (m a)
fJoin = fmap (\ m -> m >>= id)
