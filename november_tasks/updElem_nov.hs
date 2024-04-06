import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem, length)
length :: [a] -> Int
length xs = if null xs 
                then 0
                else (1 + (length $ tail xs))

(++) :: [a] -> [a] -> [a]
xs ++ ys = if null xs
            then ys
            else head xs : (tail xs ++ ys)

take :: Int -> [a] -> [a]
take n xs = if null xs || n == 0 
            then []
            else if n < 0
                then error "Negative index!" 
                else head xs : (take (n-1) $ tail xs)

drop' :: Int -> [a] -> [a]
drop' n xs = if null xs
                then []
                else if n == 0
                    then xs
                    else drop' (n-1) (tail xs)

updElemBy :: [a] -> Int -> a -> [a]
updElemBy xs n new = if (null xs || n < 0 || n >= length xs)
                        then error "Wrong input!"
                        else (take n xs) ++ (new : (drop' (n+1) xs))

                        
updElemBy' :: [a] -> Int -> a -> [a]
updElemBy' xs n new = if (null xs || n < 0)
                        then error "Wrong input!"
                        else if n == 0
                            then new : (tail xs)
                            else (head xs) : (updElemBy' (tail xs) (n-1) new)
{-
updElemBy "Alekei" 3 'x'
"Alexei"

updElemBy' [1..30] 10 12
[1,2,3,4,5,6,7,8,9,10,12,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30]

updElemBy [] 0 1   
*** Exception: Wrong input!
CallStack (from HasCallStack):
  error, called at updElem_nov.hs:28:30 in main:Main

updElemBy' [2] 0 1
[1]
-}