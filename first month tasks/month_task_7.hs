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
nd x | issquare x = ndcount 1 x (sqrti x) - 1
     | otherwise = ndcount 1 x (sqrti x)
        where ndcount n x sqrt_const | n >= sqrt_const = 2
                                     | divides x n = 2 + ndcount (n+1) x sqrt_const
                                     | otherwise = ndcount (n+1) x sqrt_const