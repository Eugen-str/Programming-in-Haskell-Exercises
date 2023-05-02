data Prop = Const Bool
          | Var Char
          | And Prop Prop
          | Not Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Xnor Prop Prop -- same as equivalence
          deriving Show

type Assoc k v = [(k,v)]

find::Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k'==k]

type Subst = Assoc Char Bool

eval::Subst -> Prop -> Bool
eval _ (Const x) = x
eval s (Var x) = find x s
eval s (Not x) = not (eval s x)
eval s (And x y) = eval s x && eval s y
eval s (Imply x y) = eval s x <= eval s y
eval s (Or x y) = eval s x || eval s y
eval s (Xnor x y) = eval s x == eval s y

vars::Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not x) = vars x
vars (And x y) = vars x ++ vars y
vars (Imply x y) = vars x ++ vars y
vars (Or x y) = vars x ++ vars y
vars (Xnor x y) = vars x ++ vars y

int2bin::Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bools :: Int -> [[Bool]]
bools n = map (map conv . reverse . make n . int2bin) range
    where 
        range = [0..2^n-1]
        make n bs = take n (bs ++ repeat 0)
        conv 0 = False
        conv 1 = True

rmdups::Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs::Prop -> [Subst]
substs s = map (zip vs) (bools (length vs))
            where vs = rmdups(vars s)

isTaut::Prop -> Bool
isTaut s = and [eval x s | x <- substs s]