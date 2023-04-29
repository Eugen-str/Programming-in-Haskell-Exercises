data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves::Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

bal::Tree a -> Bool
bal (Leaf _) = True
bal (Node l r) = abs (leaves l - leaves r) <= 1
                 && bal l && bal r