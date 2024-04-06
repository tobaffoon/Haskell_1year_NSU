sgR1 f' y | f' y == 0 = True
          | otherwise = False
minimize f = minimize' 0 f
    where minimize' y f | sgR1 f y = y
                        | otherwise = minimize' (y+1) f
mns a b | a>b = a-b
        | otherwise = 0
newdivi x1 x2 | mod x1 x2 == 0 = 1+minimize ((\a b t-> mns a ((t+1)*b)) x1 x2)
              | otherwise = minimize ((\a b t-> mns a ((t+1)*b)) x1 x2)