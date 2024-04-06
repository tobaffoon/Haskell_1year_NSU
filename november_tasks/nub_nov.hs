import Prelude hiding ((!!), reverse, init, cycle, (++), take, elem)
elem :: Eq a => a -> [a] -> Bool
x `elem` xs = if null xs
                then False
                else if head xs == x
                    then True
                    else x `elem` (tail xs)
                    
nub :: Eq a => [a] -> [a]
nub xs = if null xs
            then []
            else if (head xs) `elem` (tail xs)
                then nub $ tail xs
                else (head xs) : (nub $ tail xs)

{-
nub "hello"
"helo"

nub "alexei"
"alxei"

nub [mod a 2 | a <- [1..100]]
[1,0]

nub []
[]
-}