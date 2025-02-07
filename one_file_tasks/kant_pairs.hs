kant :: Integer -> [[(Integer, Integer)]]
kant n = [[(a,b) | a <- [1..k], b <- [1..k], a+b==k] | k <- [2..n]]
{-
kant 5
[[(1,1)],[(1,2),(2,1)],[(1,3),(2,2),(3,1)],[(1,4),(2,3),(3,2),(4,1)]]
kant 7 
[[(1,1)],[(1,2),(2,1)],[(1,3),(2,2),(3,1)],[(1,4),(2,3),(3,2),(4,1)],[(1,5),(2,4),(3,3),(4,2),(5,1)],[(1,6),(2,5),(3,4),(4,3),(5,2),(6,1)]]
-}
akant :: Integer -> [(Integer, Integer)]
akant a = [(n, m-n) | m <- [1..a], n <- [1..(m-1)]]
{-
akant 5
[(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1)]
akant 7
[(1,1),(1,2),(2,1),(1,3),(2,2),(3,1),(1,4),(2,3),(3,2),(4,1),(1,5),(2,4),(3,3),(4,2),(5,1),(1,6),(2,5),(3,4),(4,3),(5,2),(6,1)]
-}