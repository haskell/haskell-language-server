data ABC = A | B | C

many :: () -> Either a b -> Bool -> Maybe ABC -> ABC -> ()
many () (Left a) False Nothing A = _
many () (Left a) False Nothing B = _
many () (Left a) False Nothing C = _
many () (Left a) False (Just abc') A = _
many () (Left a) False (Just abc') B = _
many () (Left a) False (Just abc') C = _
many () (Left a) True Nothing A = _
many () (Left a) True Nothing B = _
many () (Left a) True Nothing C = _
many () (Left a) True (Just abc') A = _
many () (Left a) True (Just abc') B = _
many () (Left a) True (Just abc') C = _
many () (Right b') False Nothing A = _
many () (Right b') False Nothing B = _
many () (Right b') False Nothing C = _
many () (Right b') False (Just abc') A = _
many () (Right b') False (Just abc') B = _
many () (Right b') False (Just abc') C = _
many () (Right b') True Nothing A = _
many () (Right b') True Nothing B = _
many () (Right b') True Nothing C = _
many () (Right b') True (Just abc') A = _
many () (Right b') True (Just abc') B = _
many () (Right b') True (Just abc') C = _
