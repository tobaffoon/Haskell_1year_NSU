clean_list :: Eq a => [a] -> [a]
clean_list [] = []
clean_list (x:xs) = clean_list' x xs where
    clean_list' a [] = [a]
    clean_list' a (y:ys) | a == y = clean_list' a ys
                         | otherwise = a : clean_list' y ys
{-
    clean_list [2,2,2,3,3,4,4,4]
    [2,3,4]

    clean_list [1,1,1,1,1,1,2,3,4,5,6]
    [1,2,3,4,5,6]
    
    clean_list [1,1,2,2]
    [1,2]
-}