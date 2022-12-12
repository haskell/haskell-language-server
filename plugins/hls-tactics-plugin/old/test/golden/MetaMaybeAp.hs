maybeAp :: Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp = [wingman|
  intros,
  destruct_all,
  obvious,
  obvious,
  obvious,
  ctor Just,
  application,
  assumption
  |]
