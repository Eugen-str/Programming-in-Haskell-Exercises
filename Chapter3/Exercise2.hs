bools :: [Bool]
bools = [False, False, True]

nums :: [[Int]]
nums = [[0,1,2,3,4],[15,4],[5]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z 

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a