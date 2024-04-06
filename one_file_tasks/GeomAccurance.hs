import Data.Ratio
sumg :: Double -> Double -> Integer -> Double
rezg :: Double -> Double -> Double -> Double -> Double
sumg x y 1 = x
sumg x y z = x + y * (sumg x y (z - 1))
rezg x1 x2 y d | abs(x2*y + x1 - x2)<=d = x2*y+x1
               | otherwise = rezg x1 (x2*y+x1) y d
rez x y d = rezg x x y d
main = do
    print(sumg 0.31 0.33 200) 
    print(rez 0.31 0.33 0.000000002)