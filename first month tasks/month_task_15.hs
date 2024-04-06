sgR1 f' y | f' y == 0 = True
          | otherwise = False
minimize f = minimize' 0 f
    where minimize' y f | sgR1 f y = y
                        | otherwise = minimize' (y+1) f
mns a b | a>b = a-b
        | otherwise = 0
newsqrti x = root where
    root' = minimize ((\x t -> mns x ((t+1)^2)) x)
    root | ((root'+1)^2 == x) = root' + 1
         | otherwise = root'