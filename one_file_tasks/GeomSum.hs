module Main where
import Data.Ratio
sumg :: Double -> Double -> Integer -> Double
sumg x y 1 = x
sumg x y z = x + y * (sumg x y (z - 1))
main = print(sumg 2 0.5 100000)
