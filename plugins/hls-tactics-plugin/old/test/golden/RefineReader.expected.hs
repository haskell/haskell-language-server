newtype Reader r a = Reader (r -> a)

test :: b -> Reader r a
test b = Reader _w0

