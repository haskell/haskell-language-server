module WhereBinderRec where

-- every inlined copy of a recursive body has another use, so refuse
f n = go n
  where
    go k = if k == 0 then 0 else go (k - 1)
