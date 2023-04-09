revqsort::Ord a => [a] -> [a]
revqsort [] = []
revqsort (x:xs) = revqsort smaller ++ [x] ++ revqsort larger
                  where
                  smaller = [a | a <- xs, a > x]
                  larger = [b | b <- xs, b <= x]