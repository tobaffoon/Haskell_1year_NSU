countElem x ls = length (filter (==x) ls)
countElem1 x ls | null ls = 0
                | head ls == x = 1 + (countElem1 x (tail ls))
                | otherwise = (countElem1 x (tail ls))
{-
countElem 1 [-1, 2, 0, 1, -999, 2, 1 ,-21, 1, 313, 1, 0]
4
countElem 1 [mod a 2 | a <- [1..10000]]
5000
countElem 0 [mod a 2 | a <- [1, 3..10000]]
0
countElem 2 [mod a 2 | a <- [1..10000]]
0
-}
countElem' x ls = foldl1 (+) (fill_out ls)                
    where fill_out lis = map (\y -> if y==x then 1 else 0) lis