-- There used to be a bug where we were unable to perform a nested split. The
-- more serious regression test of this is 'AutoTupleSpec'.
bigTuple :: (a, b, c, d) -> (a, b, (c, d))
bigTuple = _
