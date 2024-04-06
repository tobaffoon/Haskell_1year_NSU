rcubsum :: Num a => [a] -> a
rcubsum xs = foldr (\x y -> x^3 + y) 0 xs


lcubsum :: Num a => [a] -> a
lcubsum xs = foldl (\x y -> x + y^3) 0 xs

{-
rcubsum [1..4]
100

lcubsum [1..3]
36  

rcubsum []
0
-}