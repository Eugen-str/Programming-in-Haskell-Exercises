last'::[a] -> a
last' xs = head(reverse xs)

--or

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1) 