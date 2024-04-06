import Data.Ratio 
sq x = x^^2 
arc' :: Rational -> Int -> Int -> Rational                                      --Работает для Rational
arc' x n n_const | n==2 = 0                                                               -- n_const - Неизменяемое n, т.к. n будет счётчиком
                 | otherwise = ((-1) * (x / ((n_const::Rational) - (n::Rational))) - arc' x (n-2) n_const) * sq x -- (-1), т.к. начинаем с 3-х, умножаем все посчитанные члены на x^2
arcN x n = arc' x (2*n) (2*n+3) + x                                                       --Т.к. начали с трёх, прибавим x, такое n, т.к. идём только по нечётным