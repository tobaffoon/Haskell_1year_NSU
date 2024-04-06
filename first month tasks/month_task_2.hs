inc x | x >= 0 = x + 1
      | otherwise = error "Arg must be positive!"
dec x | x > 0 = x - 1
      | x == 0 = 0
      | otherwise = error "Arg must be positive!"
lesser :: Integer -> Integer -> Integer                    --it's basically min
lesser 0 _ = 0
lesser _ 0 = 0
lesser a b = inc (lesser (dec $! a) (dec $! b))
minlist' :: [Integer] -> Integer
minlist' [x] = x
minlist' (x:xs) = lesser x (minlist' xs)
minlist'' :: [Integer] -> Integer
minlist'' [x] = x
minlist'' (x:y:xs) = minlist'' ((lesser x y):xs)            --way slower
bigger :: Integer -> Integer -> Integer                     --it's basically max
bigger 0 b = b
bigger a 0 = a
bigger a b = inc (bigger (dec $! a) (dec $! b))
maxlist' :: [Integer] -> Integer
maxlist' [x] = x
maxlist' (x:xs) = bigger x (maxlist' $! xs)
maxlist'' :: [Integer] -> Integer
maxlist'' [x] = x
maxlist'' (x:y:xs) = maxlist'' (((bigger x) $! y):xs)      --didn't spot much of a difference