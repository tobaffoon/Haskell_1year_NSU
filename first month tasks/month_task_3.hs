inc x | x >= 0 = x + 1
      | otherwise = error "Arg must be positive!"
dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Arg must be positive!"
plus :: Integer -> Integer -> Integer
plus 0 b = b
plus a b = plus (dec $! a) (inc $! b)
mns :: Integer -> Integer -> Integer
mns 0 _ = 0
mns a 0 = a
mns a b = mns (dec $! a) (dec $! b)
div' numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"                      
                 | denom == 0 = 0
                 | otherwise = divi numer denom
                        where divi num den | num == den = 1
                                           | (mns num den) == 0 = 0
                                           | otherwise = inc (divi num (plus denom den)) 