foo :: Double -> Double
foo x = (2**x) - (x^2) 
solver :: Double -> Double -> Double -> Double
solver a b eps | abs (foo ((a + b) / 2)) < eps = ((a + b) / 2)
               | foo ((a + b) / 2) < 0 = solver ((a + b) / 2) b eps
               | otherwise = solver a ((a + b) / 2) eps
