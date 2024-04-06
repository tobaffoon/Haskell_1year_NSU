combList' :: (Ord a) => [a] -> [a] -> [a] -> [a]
combList' dest [] [] = dest
combList' dest [] xs2 = dest ++ xs2
combList' dest xs1 [] = dest ++ xs1
combList' dest (x1:xs1) (x2:xs2) | x1<x2 = (dest ++ [x1])  ++ (combList' dest xs1 (x2:xs2))
                                 | otherwise = (dest ++ [x2]) ++ (combList' dest (x1:xs1) xs2)
combList :: (Ord a) => [a] -> [a] -> [a]
combList src1 src2 = combList' [] src1 src2
{-
combList [1, 3, 9, 10] [0, 2, 11, 21]
[0,1,2,3,9,10,11,21]

combList [(-421.32), (-42.132), 0.11, 4.51, 94.000001] [(-421.33), (-33.42132), (-0.1), 0.1, 94]
[-421.33,-421.32,-42.132,-33.42132,-0.1,0.1,0.11,4.51,94.0,94.000001]

combList ['a', 'c', 'x', 'z'] ['t', 'u', 'w', 'y']
"actuwxyz"

combList ["ab", "cd", "lm", "no", "yz"] ["adc", "ef", "gh", "pq", "vw"]
["ab","adc","cd","ef","gh","lm","no","pq","vw","yz"]
-}