fact :: Int -> Int
fact 1 = 1
fact n = n * (fact (n-1))

expT :: Double -> Int -> Double
expT x n = foldr (\a b -> a + x * b) x [x / (fromIntegral $ fact c) | c <- [1..n]]      --every next summand (слагаемое) is multiplied by the start value
{-
expT 1 10 
2.718281801146385

expT 1 2 
2.5

expT 1 20
2.7182818284590455

expT 2 2
12.0

expT 0 10
0.0
-}