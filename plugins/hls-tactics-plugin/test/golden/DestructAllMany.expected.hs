data ABC = A | B | C

many :: () -> Either a b -> Bool -> Maybe ABC -> ABC -> ()
many () (Left a) False Nothing A = _w0
many () (Left a) False Nothing B = _w1
many () (Left a) False Nothing C = _w2
many () (Left a) False (Just abc') A = _w3
many () (Left a) False (Just abc') B = _w4
many () (Left a) False (Just abc') C = _w5
many () (Left a) True Nothing A = _w6
many () (Left a) True Nothing B = _w7
many () (Left a) True Nothing C = _w8
many () (Left a) True (Just abc') A = _w9
many () (Left a) True (Just abc') B = _wa
many () (Left a) True (Just abc') C = _wb
many () (Right b') False Nothing A = _wc
many () (Right b') False Nothing B = _wd
many () (Right b') False Nothing C = _we
many () (Right b') False (Just abc') A = _wf
many () (Right b') False (Just abc') B = _wg
many () (Right b') False (Just abc') C = _wh
many () (Right b') True Nothing A = _wi
many () (Right b') True Nothing B = _wj
many () (Right b') True Nothing C = _wk
many () (Right b') True (Just abc') A = _wl
many () (Right b') True (Just abc') B = _wm
many () (Right b') True (Just abc') C = _wn
