normalize :: (Floating a) => [a] -> a
normalize vs = sqrt (foldl1 (+) [a^2 | a <- vs])
{-
    normalize [1,2,3,4,5]
    7.416198487095663

    normalize [2,2,2,2]
    4.0
    
    normalize [1, 0, (-2), 0, 3, 0, (-4), 0, 5]
    7.416198487095663
-}