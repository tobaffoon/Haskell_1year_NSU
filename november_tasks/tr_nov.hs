import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem, length)
(!!) :: [a] -> Int -> a
xs !! n = if (null xs || n < 0)
            then error "Wrong input!"
            else if n == 0
                 then head xs
                 else (tail xs) !! (n-1)

updElemBy :: [a] -> Int -> a -> [a]
updElemBy xs n new = if (null xs || n < 0)
                        then error "Wrong input!"
                        else if n == 0
                            then new : (tail xs)
                            else (head xs) : (updElemBy (tail xs) (n-1) new)

tr :: [a] -> Int -> Int -> [a]
tr xs i j = updElemBy (updElemBy xs i (xs !! j)) j (xs !! i)


tr' :: [a] -> Int -> Int -> [a]
tr' xs i j = if (null xs || i < 0 || j < 0)
                then error "Wrong input!"
                else if i == 0
                    then (xs !! j) : (tail $ updElemBy xs j (xs !! i))
                    else if j== 0
                        then (xs !! i) : (tail $ updElemBy xs i (xs !! j))
                        else (head xs) : (tr' (tail xs) (i-1) (j-1))
{-
tr [1..10] 0 9  
[10,2,3,4,5,6,7,8,9,1]

tr' "oellh" 0 4
"hello"

tr [] 1 2
*** Exception: Wrong input!
-}