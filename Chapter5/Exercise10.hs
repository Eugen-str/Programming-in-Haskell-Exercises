import Data.Char

let2int::Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | isUpper c = ord c - ord 'A' + 26

int2let::Int -> Char
int2let n | n < 26 = chr(n + ord 'a')
          | n > 25 && n < 52 = chr(n + ord 'A' - 26)

shift::Int -> Char -> Char
shift n c | isLetter c = int2let((let2int c + n) `mod` 52)
          | otherwise = c

encode::Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]