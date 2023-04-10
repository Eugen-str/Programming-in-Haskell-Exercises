or'::Bool -> Bool -> Bool
True `or'` _ = True
False `or'` b = b