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
mlt :: Integer -> Integer -> Integer
mlt 0 _ = 0
mlt _ 0 = 0                                         
mlt 1 b = b
mlt a b = plus b (mlt (dec $! a) b)
div' numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"                      
                 | denom == 0 = 0
                 | otherwise = divi numer denom
                        where divi num den | num == den = 1
                                           | (mns num den) == 0 = 0
                                           | otherwise = inc (divi num (plus denom den))    
mod' numer denom = numer - mlt denom (div' numer denom)                                     --co-recursive
mod1' numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"
                  | denom == 0 = numer
                  | otherwise = mod1i numer denom
                        where mod1i num den | num == den = 0
                                            | (mns num den) == 0 = mns (plus num denom) den
                                            | otherwise = mod1i num (plus denom den)

div'' _ 0 = 0
div'' numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"
                  | numer == denom = 1
                  | (mns numer denom) == 0 = 0
                  | otherwise = inc (div'' (mns numer denom) denom)
mod'' numer denom = numer - mlt denom (div'' numer denom)                                   --recursive
mod1'' numer denom | numer < 0 || denom < 0  = error "Arg must be positive!"
                   | denom == 0 = numer
                   | otherwise = mod1i numer denom
                        where mod1i num den | num == den = 0
                                            | (mns num den) == 0 = num
                                            | otherwise = mod1i (mns num den) den