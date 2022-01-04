data ADT = One | Two Int | Three | Four Bool ADT | Five

case_split :: ADT -> Int
case_split One        = _
case_split (Two i)    = _
case_split Three      = _
case_split (Four b One) = _w0
case_split (Four b (Two n)) = _w1
case_split (Four b Three) = _w2
case_split (Four b (Four b' adt)) = _w3
case_split (Four b Five) = _w4
case_split Five       = _
