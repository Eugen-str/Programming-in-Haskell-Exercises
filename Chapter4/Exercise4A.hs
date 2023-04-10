or'::Bool -> Bool -> Bool
True `or'` True = True
True `or'` False = True
False `or'` True = True
False `or'` False = False