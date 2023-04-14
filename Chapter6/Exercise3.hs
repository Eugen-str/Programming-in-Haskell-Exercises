ex::Integral a => a -> a -> a
ex _ 0 = 1
ex x e | e > 0 = x * ex x (e-1)

{-
ex 2 3 =
2 * ex 2 2 =
2 * 2 * ex 2 1 = 
2 * 2 * 2 * ex 2 0 =
2 * 2 * 2 * 1 =
8
-}