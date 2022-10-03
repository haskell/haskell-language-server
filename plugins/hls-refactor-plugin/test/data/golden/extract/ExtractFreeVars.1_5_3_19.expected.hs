newDefinition v1 v2 =
    let v3 = 3
    in
      v1 + v2 + v3

f1 v1 =
    newDefinition v1 v2
  where
    v2 = 2