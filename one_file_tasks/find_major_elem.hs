import Data.Maybe
countElem x ls = foldl1 (+) (fill_out ls)                
    where fill_out as = map (\y -> if y==x then 1 else 0) ls
--Найти самый частый. Посчитать его вхождения в список
find_maj :: (Eq a) => [a] -> Maybe a
find_maj [] = Nothing
find_maj [x] = Just x
find_maj (x:xs) = maj where
    n = maj_sort x (x:xs) 1
    maj | (countElem n (x:xs)) > (length (x:xs) `div` 2) = Just n
        | otherwise = Nothing
    maj_sort num [] count = num
    maj_sort num ls count | head ls == num = maj_sort num (drop 1 ls) (count + 1)
                          | count == 1 = maj_sort (head ls) (drop 1 ls) 1
                          | otherwise = maj_sort num (drop 1 ls) (count - 1)
{-
    find_maj [1]
    Just 1

    find_maj [1,2]
    Nothing

    find_maj [1,2,2]
    Just 2

    find_maj [1,1,2]
    Just 1

    find_maj [1,2,1,2]
    Nothing

    find_maj [1,2,1,3,1,4,1,5,1,6]
    Nothing

    find_maj [1,2,1,3,1,4,1,5,1,6,1]
    Just 1

    find_maj [3, 3, 4, 2, 4, 4, 2, 4, 4]
    Just 4
    
    find_maj [3, 3, 4, 2, 4, 4, 2, 4]
    Nothing
-}