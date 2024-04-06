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
mlt _ 0 = 0                                         --Just to optimize
mlt 1 b = b
mlt a b = plus b (mlt (dec $! a) b)