data Expr = Val Int 
          | Add Expr Expr
          | Mult Expr Expr

data Op = EVAL Expr
        | ADD Int
        | MULT Int
        | MULT_EVAL Expr
        | ADD_EVAL Expr

type Cont = [Op]

eval::Expr -> Cont -> Int
eval (Val x) c = exec c x
eval (Add x y) c = eval x (ADD_EVAL y : c)
eval (Mult x y) c = eval x (MULT_EVAL y : c)

exec::Cont -> Int -> Int
exec [] n = n
exec (MULT_EVAL y : c) x = eval y (MULT x : c)
exec (ADD_EVAL y : c) x = eval y (ADD x : c)
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)
exec (MULT n : c) m = exec c (n * m)

value::Expr -> Int
value e = eval e []