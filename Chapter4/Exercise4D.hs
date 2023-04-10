or'::Bool -> Bool -> Bool
a `or'` b = if a == b then (a && b) else True