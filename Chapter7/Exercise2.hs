-- a
all'::(a -> Bool) -> [a] -> Bool
all' p = and . map p

-- b
any'::(a -> Bool) -> [a] -> Bool
any' p = or . map p

-- c
takeWhile'::(a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

-- d
dropWhile'::(a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs