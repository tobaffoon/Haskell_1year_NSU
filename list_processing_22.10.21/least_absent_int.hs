--It is just a binary search, but we don't access the middle element, we suppose it (for a limited list of integers)
my_partition :: (Ord a) => (a -> Bool) -> [a] -> ([a], [a])
my_partition' :: (Ord a) => (a -> Bool) -> [a] -> [a] -> [a] -> ([a], [a])
my_partition cond ls = my_partition' cond ls [] []
my_partition' _ [] as bs = (as, bs)
my_partition' cond (x:xs) as bs | cond x = my_partition' cond xs (x:as) bs
                                | otherwise = my_partition' cond xs as (x:bs)
--It only works for lists without duplicates
least_absent xs = least_absent_from 0 (length xs, xs)
least_absent_from low_bnd (n, xs) | n == 0 = low_bnd                                               --low_bnd is lower bound
                                  | m == mid - low_bnd = least_absent_from mid (n - m, big_s)      --n is length of current list. 
                                  | otherwise = least_absent_from low_bnd (m, smal_s)               
                            where (smal_s, big_s) = my_partition (< mid) xs                           --smal_s and big_s are bigger and smaller parts of current list
                                  mid = low_bnd + 1 + (div n 2)                                            --mid is not middle of the current list
                                  m = length smal_s                                                      --m is length of elements lower then b