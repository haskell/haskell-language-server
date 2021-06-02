data ADT = One | Two Int | Three | Four Bool ADT | Five

case_split :: ADT -> Int
case_split One        = _
case_split (Two i)    = _
case_split Three      = _
case_split (Four b One) = _
case_split (Four b (Two n)) = _
case_split (Four b Three) = _
case_split (Four b (Four b' adt)) = _
case_split (Four b Five) = _
case_split Five       = _
