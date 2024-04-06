gcdEuclid x y | x==y = x
              | x < y = gcdEuclid (y-x) x
              | otherwise = gcdEuclid y (x-y)
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
gcd1 x y = gcd1' 1 1 x y (sqrti (max x y))
    where gcd1' cur ans x y cmp_const | cur > cmp_const = ans
                                      | (divides x cur) && (divides y cur) && (cur/=1) = ans * (gcd1' cur cur (div x cur) (div y cur) cmp_const)
                                      | otherwise = gcd1' (cur+1) ans x y cmp_const