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
div'' _ 0 = 0
div'' numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"
                  | numer == denom = 1
                  | (mns numer denom) == 0 = 0
                  | otherwise = inc (div'' (mns numer denom) denom)
