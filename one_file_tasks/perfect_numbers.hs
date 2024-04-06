isqr :: Integer -> Integer -> Integer -> Integer
isqr comp comp_const n | comp < n = isqr (comp+2*comp_const+1) (comp_const+1) n
                       | comp == n = comp_const
                       | otherwise = (comp_const-1)
isqrt n = isqr 0 0 n
divisors_list :: Integer -> [Integer]
divisors_list n = [a | a <- [1..(div n 2)], mod n a==0]
perf_n :: Int -> [Integer]
perf_n n = take n [p | p <- [1..], sum (divisors_list p) == p]
{-
perf_n 4
[6,28,496,8128]
-}
--div_list n = divisors_list (isqrt n) n