minimize f = minimize' 0 f
    where minimize' y f | f y == 0 = y
                        | otherwise = minimize' (y+1) f
{-
f x y = (y-5)^2 - x
minimize ((\x y -> (y-5)^2 - x) 1)                     4        
minimize ((\x y -> (y-5)^2 - x) 9)                     2        
f x y = (-1) * abs(y*2-10) + x
minimize ((\x y -> (-1) * abs(y*2-10) + x) 2)          4
minimize ((\x y -> (-1) * abs(y*2-10) + x) 4)          3
-}