-- a
and'::[Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- b
concat'::[[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- c
replicate'::Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

-- d
(!!!)::[a] -> Int -> a
(!!!) (x:xs) n | n == 0 = x
               | otherwise = xs !!! (n-1)

-- e
elem'::Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) | e == x = True
               | otherwise = elem' e xs