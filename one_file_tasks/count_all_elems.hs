import Data.List
countElem x ls | null ls = 0
               | head ls == x = 1 + (countElem x (tail ls))
               | otherwise = (countElem x (tail ls))
naive_imp xs = [(a, countElem a xs) | a <- (nub xs)]
{-bit_less_naive (x:xs) | null (x:xs) = []
                      | otherwise = (x, len) : bit_less_naive nxs where
                          len = 1+countElem x xs-}