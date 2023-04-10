and'::Bool -> Bool -> Bool
a `and'` b = if a == True then
                if b == True then True else False
             else False