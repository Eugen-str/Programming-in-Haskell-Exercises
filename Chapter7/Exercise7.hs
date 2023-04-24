import Data.Char ( ord, chr )
type Bit = Int

bin2int::[Bit] -> Int
bin2int = foldl (\x y -> x*2 + y) 0

int2bin::Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8::[Bit] -> [Bit]
make8 bits = reverse (take 8 (bits ++ repeat 0))

-- int to byte including 1 bit (even) parity
int2Bp::Int -> [Bit]
int2Bp n = bits ++ if even(sum bits) then [0] else [1]
            where bits = make8 (int2bin n)

encode::String -> [Bit]
encode = concat . map (int2Bp . ord)

decode::[Bit] -> String
decode [] = ""
decode bits = if even (sum $ take 9 bits) 
    then chr(bin2int $ take 8 bits) : decode (drop 9 bits) 
    else error "Transmission error!"

channel::[Bit] -> [Bit]
channel = id

transmit::String -> String
transmit = decode . channel . encode