inc x = x + 1
dec x = x - 1
plus :: Integer -> Integer -> Integer
plus 0 b = b
plus a 0 = a
plus a b | a<0 = plus (inc $! a) (dec $! b)
         | otherwise = plus (dec $! a) (inc $! b)
mns :: Integer -> Integer -> Integer
mns a 0 = a
mns a b | b<0 = mns (inc $! a) (inc $! b)
        | otherwise = mns (dec $! a) (dec $! b)
neg' a = mns 0 a
abs' x | x >= 0 = x
       | otherwise = neg' x
div'' _ 0 = 0
div'' numer denom | numer == denom = 1
                  | numer < denom = 0
                  | otherwise = inc (div'' (mns numer denom) denom)
divint numer denom | (numer < 0  && denom > 0) || (numer > 0  && denom < 0) = neg' (div'' (abs' numer) (abs' denom))                --use divint, not div''
                   | otherwise = (div'' (abs' numer) (abs' denom))                   
mod'' numer denom | denom == 0 = numer
                  | otherwise = mod1i numer denom
                    where mod1i num den | num == den = 0
                                        | (num < den) = num
                                        | otherwise = mod1i (mns num den) den                                                             --use modint, not mod''
modint numer denom | (numer < 0  && denom > 0) || (numer > 0  && denom < 0) = mod'' (mns denom (mod'' (abs' numer) (abs' denom))) denom               
                   | otherwise = (mod'' (abs' numer) (abs' denom))         