altMap::(a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

luhnDouble::Int -> Int
luhnDouble n = if n*2 < 10 then n*2 else n*2 - 9

luhn::[Int] -> Bool
luhn xs = sum (altMap luhnDouble id xs) `mod` 10 == 0