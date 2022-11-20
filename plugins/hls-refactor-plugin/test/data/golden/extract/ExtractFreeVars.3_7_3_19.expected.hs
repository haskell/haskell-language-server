newDefinition v1 v2 v3 =
  v1 + v2 + v3

f1 v1 =
    let v3 = 3
    in
      newDefinition v1 v2 v3
  where
    v2 = 2