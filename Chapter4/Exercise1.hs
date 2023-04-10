halve::[a] -> ([a], [a])
halve xs | length xs `mod` 2 == 0 = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)