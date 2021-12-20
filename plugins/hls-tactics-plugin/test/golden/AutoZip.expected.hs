zip_it_up_and_zip_it_out :: [a] -> [b] -> [(a, b)]
zip_it_up_and_zip_it_out _ [] = []
zip_it_up_and_zip_it_out [] (_ : _) = []
zip_it_up_and_zip_it_out (a : as') (b : bs')
  = (a, b) : zip_it_up_and_zip_it_out as' bs'

