import Data.List
arc' :: Double -> Double -> [Double]
arc' x n = [ (x**abs(k)) / (k :: Double) | k <- [1.0, (-3.0)..], abs(k) <= (2*n-1)]
{-arcF x n = foldl (+) xs where
    xs = arc' x n-}