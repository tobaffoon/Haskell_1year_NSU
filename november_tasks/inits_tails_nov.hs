import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem, length)
length :: [a] -> Int
length xs = if null xs 
                then 0
                else (1 + (length $ tail xs))
                
take :: Int -> [a] -> [a]
take n xs = if null xs
            then []
            else if n == 0 
                then []
                else if n < 0
                    then error "Negative index!" 
                    else head xs : (take (n-1) $ tail xs)

inits1 :: [a] -> [[a]]
inits1 xs = [take a xs| a <- [0..length xs]]

inits :: [a] -> [[a]]
inits xs = inits' 0 xs (length xs) where
    inits' n ys boarder = if n > boarder
                              then []
                              else take n ys : inits' (n+1) ys boarder

tails :: [a] -> [[a]]
tails xs = if null xs
            then [[]]
            else xs : (tails $ tail xs)


drop' :: Int -> [a] -> [a]
drop' n xs = if null xs
                then []
                else if n == 0
                    then xs
                    else drop' (n-1) (tail xs)

tails1 :: [a] -> [[a]]
tails1 xs = [drop' a xs | a <- [0..length xs]]

{-
inits "cinema"
["","c","ci","cin","cine","cinem","cinema"]

inits1 "night"
["","n","ni","nig","nigh","night"]

inits ""
[""]

tails []
[[]]

tails1 [1..5]
[[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]]

tails [1,3..9]
[[1,3,5,7,9],[3,5,7,9],[5,7,9],[7,9],[9],[]]
-}