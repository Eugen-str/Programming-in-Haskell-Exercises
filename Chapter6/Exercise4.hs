euclid::Int -> Int -> Int
euclid a b | a == b = a
           | a < b = euclid a (b-a)
           | b < a = euclid b (a-b)