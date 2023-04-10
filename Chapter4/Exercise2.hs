--using head and tail
third::[a] -> a
third xs = head(tail(tail xs))

--using !!
third'::[a] -> a
third' xs = xs !! 2

--using pattern matching
third''::[a] -> a
third'' (_:_:x:_) = x