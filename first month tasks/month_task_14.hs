sgR1 f' y | f' y == 0 = True
          | otherwise = False
minimize f = minimize' 0 f
    where minimize' y f | sgR1 f y = y
                        | otherwise = minimize' (y+1) f