import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
take :: Int -> [a] -> [a]
take n xs = if null xs || n == 0
                then []
                else if n < 0
                    then error "Negative index!" 
                    else head xs : (take (n-1) $ tail xs)
                    
take1 :: Int -> [a] -> [a]
take1 _ [] = []
take1 0 _ = []
take1 n (x:xs) = if n < 0 
                    then error "Negative index!"
                    else x : (take (n-1) xs)

{-
take 5 "hello, dear user"
"hello"

take 4 "vladimir nikolaevich"
"vlad"

take1 16 "monoids are life-ruining"
"monoids are life"

take 1 []
[]
-}