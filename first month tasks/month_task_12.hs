divides numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"
                    | denom == 0 = True
                    | otherwise = dividesi numer denom
                        where dividesi num den | num == den = True 
                                               | (num - den) <= 0 = False
                                               | otherwise = dividesi (num - den) den
sqrti x = sqrti' 1 1 x
    where sqrti' cur cur_square x | cur_square == x = cur
                                  | cur_square > x = cur-1
                                  | otherwise = sqrti' (cur+1) (cur_square+cur*2+1) x
prime x  | x <= 1 = False 
         | otherwise = prime' 2 x (sqrti x)
          where prime' n x sqrt_const | n > sqrt_const = True
                                      | divides x n = False
                                      | otherwise = prime' (n+1) x sqrt_const
lcm1 x y = lcm' 2 x y
                where lcm' n x y | x==1 = y
                                 | y==1 = x
                                 | (divides x n) && (divides y n) = n * (lcm' n (div x n) (div y n))
                                 | (divides x n) = n * (lcm' n (div x n) y)
                                 | (divides y n) = n * (lcm' n x (div y n))
                                 | otherwise = lcm' (n+1) x y