whats_it_deep_of
    :: (a -> a)
    -> [(Int, Either Bool (Maybe [a]))]
    -> [(Int, Either Bool (Maybe [a]))]
-- The assumption here is necessary to tie-break in favor of the longest
-- nesting of fmaps.
whats_it_deep_of f = fmap (fmap (fmap (fmap (fmap f))))

