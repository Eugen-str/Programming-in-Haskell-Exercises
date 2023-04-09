qsort'::Ord a => [a] -> [a]
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
               where
               smaller = [a | a <- xs, a < x]
               larger = [b | b <- xs, b > x]

{-
Replacing the <= with a < and inputing qsort' [2,2,3,1,1] 
produces the result [1,2,3].

qsort' [2,2,3,1,1]
qsort' [1,1] ++ [2] ++ qsort' [3]
[1] ++ [2] ++ [3]
[1,2,3]
-}