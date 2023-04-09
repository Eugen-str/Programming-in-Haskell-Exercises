product'::[Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

main = do
    print (product' [2,3,4])