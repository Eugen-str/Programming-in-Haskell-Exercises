--using a conditional expression
safetail::[a] -> [a]
safetail xs = if null xs then [] else tail xs

--using guarded equations
safetail'::[a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

--using pattern matching
safetail''::[a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs