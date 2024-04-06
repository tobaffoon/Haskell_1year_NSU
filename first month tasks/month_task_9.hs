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
issquare x = (sqrti x)^2 == x
prime x  | x <= 1 = False 
         | otherwise = prime' 1 x
            where prime' n x | n > x = True
                             | divides x n && n/=1 && n/=x = False
                             | otherwise = prime' (n+1) x
prime1 x | x <= 1 = False 
         | otherwise = prime1' 2 x (sqrti x)
          where prime1' n x sqrt_const | n > sqrt_const = True
                                      | divides x n = False
                                      | otherwise = prime1' (n+1) x sqrt_const