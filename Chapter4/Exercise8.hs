luhnDouble::Int -> Int
luhnDouble x | x*2 > 9 = x*2 - 9
             | otherwise = x*2

luhn::Int -> Int -> Int -> Int -> Bool
luhn x y z w = (luhnDouble x + y + luhnDouble z + w) `mod` 10 == 0