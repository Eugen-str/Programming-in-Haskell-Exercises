unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x = []
               | otherwise = h x : unfold p h t (t x)

{-
chop8::[Int] -> [[Int]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
-}

chop8::[Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map'::(a -> b) -> [a] -> [b]
map' f (x:xs) = unfold null (\id -> f x) (const xs) (x:xs)

iterate''::(a -> a) -> a -> [a]
iterate'' f x = unfold (const False) id f x