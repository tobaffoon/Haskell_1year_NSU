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
sumnd x | issquare x = sumnd' 1 x (sqrti x) - sqrti x
        | otherwise = sumnd' 1 x (sqrti x)
          where sumnd' n x sqrt_const | n >= sqrt_const = n + div x n
                                      | divides x n =  n + div x n + sumnd' (n+1) x sqrt_const
                                      | otherwise = sumnd' (n+1) x sqrt_const