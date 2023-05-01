data Tree a = Leaf a | Node (Tree a) (Tree a)
            deriving Show


split::[a] -> ([a],[a])
split xs = splitAt (length xs `div` 2) xs

balance::[a] -> Tree a
balance [x] = Leaf x
balance zs = Node (balance xs) (balance ys)
            where (xs,ys) = split zs