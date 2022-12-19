-- There was an old bug where we would only pull skolems from the hole, rather
-- than the entire hypothesis. Because of this, the 'b' here would be
-- considered a univar, which could then be unified with the skolem 'c'.
fish :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
fish amb bmc a = _
