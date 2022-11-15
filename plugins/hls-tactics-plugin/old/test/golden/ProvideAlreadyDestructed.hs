foo :: Bool -> ()
foo x =
  if True
    then
      case x of
        True  -> _
        False -> ()
    else
      _
