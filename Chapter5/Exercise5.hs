pyths::Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], 
                       y <- [1..n], 
                       z <- [1..n], 
                       z*z == x*x + y*y]