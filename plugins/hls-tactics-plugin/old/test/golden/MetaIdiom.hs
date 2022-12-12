foo :: Int -> Int -> Int
foo = undefined

test :: Maybe Int
test = [wingman| idiom (use foo) |]

